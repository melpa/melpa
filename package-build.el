;;; package-build.el --- Tools for assembling a package archive

;; Copyright (C) 2011-2013 Donald Ephraim Curtis <dcurtis@milkbox.net>
;; Copyright (C) 2012-2014 Steve Purcell <steve@sanityinc.com>
;; Copyright (C) 2009 Phil Hagelberg <technomancy@gmail.com>

;; Author: Donald Ephraim Curtis <dcurtis@milkbox.net>
;; Created: 2011-09-30
;; Version: 0.1
;; Keywords: tools
;; Package-Requires: ((cl-lib "0.5"))

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file allows a curator to publish an archive of Emacs packages.

;; The archive is generated from a set of recipes which describe elisp
;; projects and repositories from which to get them.  The term
;; "package" here is used to mean a specific version of a project that
;; is prepared for download and installation.

;;; Code:

(require 'cl-lib)

(require 'package)
(require 'lisp-mnt)
(require 'json)

;;; Options

(defconst package-build--melpa-base
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name (buffer-file-name))))))

(defgroup package-build nil
  "Facilities for building package.el-compliant packages from upstream source code."
  :group 'development)

(defcustom package-build-working-dir
  (expand-file-name "working/" package-build--melpa-base)
  "Directory in which to keep checkouts."
  :group 'package-build
  :type 'string)

(defcustom package-build-archive-dir
  (expand-file-name "packages/" package-build--melpa-base)
  "Directory in which to keep compiled archives."
  :group 'package-build
  :type 'string)

(defcustom package-build-recipes-dir
  (expand-file-name "recipes/" package-build--melpa-base)
  "Directory containing recipe files."
  :group 'package-build
  :type 'string)

(defcustom package-build-verbose t
  "When non-nil, then print additional progress information."
  :group 'package-build
  :type 'boolean)

(defcustom package-build-stable nil
  "When non-nil, then try to build packages from versions-tagged code."
  :group 'package-build
  :type 'boolean)

(defcustom package-build-timeout-executable
  (let ((prog (or (executable-find "timeout")
                  (executable-find "gtimeout"))))
    (when (and prog
               (string-match-p "^ *-k"
                               (shell-command-to-string (concat prog " --help"))))
      prog))
  "Path to a GNU coreutils \"timeout\" command if available.
This must be a version which supports the \"-k\" option."
  :group 'package-build
  :type '(file :must-match t))

(defcustom package-build-timeout-secs 600
  "Wait this many seconds for external processes to complete.

If an external process takes longer than specified here to
complete, then it is terminated.  This only has an effect
if `package-build-timeout-executable' is non-nil."
  :group 'package-build
  :type 'number)

(defcustom package-build-tar-executable
  (or (executable-find "gtar")
      (executable-find "tar"))
  "Path to a (preferably GNU) tar command.
Certain package names (e.g. \"@\") may not work properly with a BSD tar."
  :group 'package-build
  :type '(file :must-match t))

(defcustom package-build-write-melpa-badge-images nil
  "When non-nil, write MELPA badge images alongside packages.
These batches can, for example, be used on GitHub pages."
  :group 'package-build
  :type 'boolean)

(defcustom package-build-version-regexp "^[rRvV]?\\(.*\\)$"
  "Default pattern for matching valid version-strings within repository tags.
The string in the capture group should be parsed as valid by `version-to-list'."
  :group 'package-build
  :type 'string)

;;; Internal Variables

(defvar package-build--recipe-alist nil
  "Internal list of package recipes.
Use function `package-build-recipe-alist' instead of this variable.")

(defvar package-build--archive-alist nil
  "Internal list of already-built packages, in the standard package.el format.
Use function `package-build-archive-alist' instead of this variable.")

(defconst package-build-default-files-spec
  '("*.el" "*.el.in" "dir"
    "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"))
  "Default value for :files attribute in recipes.")

;;; Utilities

(defun package-build--message (format-string &rest args)
  "Behave like `message' if `package-build-verbose' is non-nil.
Otherwise do nothing."
  (when package-build-verbose
    (apply 'message format-string args)))

(defun package-build--slurp-file (file)
  "Return the contents of FILE as a string, or nil if no such file exists."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun package-build--string-rtrim (str)
  "Remove trailing whitespace from `STR'."
  (replace-regexp-in-string "[ \t\n\r]+$" "" str))

(defun package-build--trim (str &optional chr)
  "Return a copy of STR without any trailing CHR (or space if unspecified)."
  (if (equal (elt str (1- (length str))) (or chr ? ))
      (substring str 0 (1- (length str)))
    str))

;;; Version Handling

(defun package-build--valid-version (str &optional regexp)
  "Apply to STR the REGEXP if defined, \
then pass the string to `version-to-list' and return the result, \
or nil if the version cannot be parsed."
  (when (and regexp (string-match regexp str))
    (setq str (match-string 1 str)))
  (ignore-errors (version-to-list str)))

(defun package-build--parse-time (str)
  "Parse STR as a time, and format as a YYYYMMDD.HHMM string."
  ;; We remove zero-padding the HH portion, as it is lost
  ;; when stored in the archive-contents
  (setq str (substring-no-properties str))
  (let ((time (date-to-time
               (if (string-match "\
^\\([0-9]\\{4\\}\\)/\\([0-9]\\{2\\}\\)/\\([0-9]\\{2\\}\\) \
\\([0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)$" str)
                   (concat (match-string 1 str) "-" (match-string 2 str) "-"
                           (match-string 3 str) " " (match-string 4 str))
                 str))))
    (concat (format-time-string "%Y%m%d." time)
            (format "%d" (string-to-number (format-time-string "%H%M" time))))))

(defun package-build--find-parse-time (regexp &optional bound)
  "Find REGEXP in current buffer and format as a time-based version string.
An optional second argument bounds the search; it is a buffer
position.  The match found must not end after that position."
  (and (re-search-backward regexp bound t)
       (package-build--parse-time (match-string-no-properties 1))))

(defun package-build--find-parse-time-newest (regexp &optional bound)
  "Find REGEXP in current buffer and format as a time-based version string.
An optional second argument bounds the search; it is a buffer
position.  The match found must not end after that position."
  (save-match-data
    (let (cur matches)
      (while (setq cur (package-build--find-parse-time regexp bound))
        (push cur matches))
      (car (nreverse (sort matches 'string<))))))

(defun package-build--find-version-newest (regexp &optional bound)
  "Find the newest version matching REGEXP before point.
An optional second argument bounds the search; it is a buffer
position.  The match found must not before after that position."
  (let ((tags (split-string
               (buffer-substring-no-properties
                (or bound (point-min)) (point))
               "\n")))
    (setq tags (append
                (mapcar
                 ;; Because the default `version-separator' is ".",
                 ;; version-strings like "1_4_5" will be parsed
                 ;; wrongly as (1 -4 4 -4 5), so we set
                 ;; `version-separator' to "_" below and run again.
                 (lambda (tag)
                   (when (package-build--valid-version tag regexp)
                     (list (package-build--valid-version tag regexp) tag)))
                 tags)
                (mapcar
                 ;; Check for valid versions again, this time using
                 ;; "_" as a separator instead of "." to catch
                 ;; version-strings like "1_4_5".  Since "_" is
                 ;; otherwise treated as a snapshot separator by
                 ;; `version-regexp-alist', we don't have to worry
                 ;; about the incorrect version list above—(1 -4 4 -4
                 ;; 5)—since it will always be treated as older by
                 ;; `version-list-<'.
                 (lambda (tag)
                   (let ((version-separator "_"))
                     (when (package-build--valid-version tag regexp)
                       (list (package-build--valid-version tag regexp) tag))))
                 tags)))
    (setq tags (cl-remove-if nil tags))
    ;; Returns a list like ((0 1) ("v0.1")); the first element is used
    ;; for comparison and for `package-version-join', and the second
    ;; (the original tag) is used by git/hg/etc.
    (car (nreverse (sort tags (lambda (v1 v2) (version-list-< (car v1) (car v2))))))))

;;; Run Process

(defun package-build--run-process (dir command &rest args)
  "In DIR (or `default-directory' if unset) run COMMAND with ARGS.
Output is written to the current buffer."
  (let ((default-directory (file-name-as-directory (or dir default-directory)))
        (argv (nconc (unless (eq system-type 'windows-nt)
                       (list "env" "LC_ALL=C"))
                     (if package-build-timeout-executable
                         (nconc (list package-build-timeout-executable
                                      "-k" "60" (number-to-string
                                                 package-build-timeout-secs)
                                      command)
                                args)
                       (cons command args)))))
    (unless (file-directory-p default-directory)
      (error "Can't run process in non-existent directory: %s" default-directory))
    (let ((exit-code (apply 'process-file
                            (car argv) nil (current-buffer) t
                            (cdr argv))))
      (or (zerop exit-code)
          (error "Command '%s' exited with non-zero status %d: %s"
                 argv exit-code (buffer-string))))))

(defun package-build--run-process-match (regexp dir prog &rest args)
  "Run PROG with args and return the first match for REGEXP in its output.
PROG is run in DIR, or if that is nil in `default-directory'."
  (with-temp-buffer
    (apply 'package-build--run-process dir prog args)
    (goto-char (point-min))
    (re-search-forward regexp)
    (match-string-no-properties 1)))

;;; Checkout
;;;; Common

(defun package-build-checkout (name config working-dir)
  "Check out source for the package named NAME with CONFIG under WORKING-DIR.
In turn, this function uses the :fetcher option in the CONFIG to
choose a source-specific fetcher function, which it calls with
the same arguments.

Returns the package version as a string."
  (let ((fetcher (plist-get config :fetcher)))
    (package-build--message "Fetcher: %s" fetcher)
    (unless (eq fetcher 'wiki)
      (package-build--message "Source: %s\n"
                              (or (plist-get config :repo)
                                  (plist-get config :url)))
      (funcall (intern (format "package-build--checkout-%s" fetcher))
               name config (file-name-as-directory working-dir)))))

(defun package-build--princ-exists (dir)
  "Print a message that the contents of DIR will be updated."
  (package-build--message "Updating %s" dir))

(defun package-build--princ-checkout (repo dir)
  "Print a message that REPO will be checked out into DIR."
  (package-build--message "Cloning %s to %s" repo dir))

;;;; Git

(defun package-build--git-repo (dir)
  "Get the current git repo for DIR."
  (package-build--run-process-match
   "Fetch URL: \\(.*\\)" dir "git" "remote" "show" "-n" "origin"))

(defun package-build--checkout-git (name config dir)
  "Check package NAME with config CONFIG out of git into DIR."
  (let ((repo (plist-get config :url))
        (commit (or (plist-get config :commit)
                    (let ((branch (plist-get config :branch)))
                      (when branch
                        (concat "origin/" branch))))))
    (with-current-buffer (get-buffer-create "*package-build-checkout*")
      (goto-char (point-max))
      (cond
       ((and (file-exists-p (expand-file-name ".git" dir))
             (string-equal (package-build--git-repo dir) repo))
        (package-build--princ-exists dir)
        (package-build--run-process dir "git" "fetch" "--all" "--tags"))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t))
        (package-build--princ-checkout repo dir)
        (package-build--run-process nil "git" "clone" repo dir)))
      (if package-build-stable
          (let* ((min-bound (goto-char (point-max)))
                 (tag-version
                  (and (package-build--run-process dir "git" "tag")
                       (or (package-build--find-version-newest
                            (or (plist-get config :version-regexp)
                                package-build-version-regexp)
                            min-bound)
                           (error "No valid stable versions found for %s" name)))))
            ;; Using reset --hard here to comply with what's used for
            ;; unstable, but maybe this should be a checkout?
            (package-build--update-git-to-ref
             dir (concat "tags/" (cadr tag-version)))
            ;; Return the parsed version as a string
            (package-version-join (car tag-version)))
        (package-build--update-git-to-ref
         dir (or commit (concat "origin/" (package-build--git-head-branch dir))))
        (apply 'package-build--run-process
               dir "git" "log" "--first-parent" "-n1" "--pretty=format:'\%ci'"
               (package-build--expand-source-file-list dir config))
        (package-build--find-parse-time "\
\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \
[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\( [+-][0-9]\\{4\\}\\)?\\)")))))

(defun package-build--git-head-branch (dir)
  "Get the current git repo for DIR."
  (or (ignore-errors
        (package-build--run-process-match
         "HEAD branch: \\(.*\\)" dir "git" "remote" "show" "origin"))
      "master"))

(defun package-build--git-head-sha (dir)
  "Get the current head SHA for DIR."
  (ignore-errors
    (package-build--run-process-match
     "\\(.*\\)" dir "git" "rev-parse" "HEAD")))

(defun package-build--update-git-to-ref (dir ref)
  "Update the git repo in DIR so that HEAD is REF."
  (package-build--run-process dir "git" "reset" "--hard" ref)
  (package-build--run-process dir "git" "submodule" "sync" "--recursive")
  (package-build--run-process dir "git" "submodule" "update" "--init" "--recursive"))

(defun package-build--checkout-github (name config dir)
  "Check package NAME with config CONFIG out of github into DIR."
  (let ((url (format "https://github.com/%s.git" (plist-get config :repo))))
    (package-build--checkout-git name (plist-put (copy-sequence config) :url url) dir)))

(defun package-build--checkout-gitlab (name config dir)
  "Check package NAME with config CONFIG out of gitlab into DIR."
  (let ((url (format "https://gitlab.com/%s.git" (plist-get config :repo))))
    (package-build--checkout-git name (plist-put (copy-sequence config) :url url) dir)))

;;;; Hg

(defun package-build--hg-repo (dir)
  "Get the current hg repo for DIR."
  (package-build--run-process-match "default = \\(.*\\)" dir "hg" "paths"))

(defun package-build--checkout-hg (name config dir)
  "Check package NAME with config CONFIG out of hg into DIR."
  (let ((repo (plist-get config :url)))
    (with-current-buffer (get-buffer-create "*package-build-checkout*")
      (goto-char (point-max))
      (cond
       ((and (file-exists-p (expand-file-name ".hg" dir))
             (string-equal (package-build--hg-repo dir) repo))
        (package-build--princ-exists dir)
        (package-build--run-process dir "hg" "pull")
        (package-build--run-process dir "hg" "update"))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t))
        (package-build--princ-checkout repo dir)
        (package-build--run-process nil "hg" "clone" repo dir)))
      (if package-build-stable
          (let ((min-bound (goto-char (point-max)))
                (regexp (or (plist-get config :version-regexp)
                            package-build-version-regexp))
                tag-version)
            (package-build--run-process dir "hg" "tags")
            ;; The output of `hg tags` shows the ref of the tag as well
            ;; as the tag itself, e.g.:
            ;;
            ;; tip                             1696:73ad80e8fea1
            ;; 1.2.8                           1691:464af57fd2b7
            ;;
            ;; So here we remove that second column before passing the
            ;; buffer contents to `package-build--find-version-newest'.
            ;; This isn't strictly necessary for Mercurial since the
            ;; colon in "1691:464af57fd2b7" means that won't be parsed
            ;; as a valid version-string, but it's an example of how to
            ;; do it in case it's necessary elsewhere.
            (goto-char min-bound)
            (ignore-errors (while (re-search-forward "\\ +.*")
                             (replace-match "")))
            (setq tag-version
                  (or (package-build--find-version-newest regexp min-bound)
                      (error "No valid stable versions found for %s" name)))
            (package-build--run-process dir "hg" "update" (cadr tag-version))
            ;; Return the parsed version as a string
            (package-version-join (car tag-version)))
        (apply 'package-build--run-process
               dir "hg" "log" "--style" "compact" "-l1"
               (package-build--expand-source-file-list dir config))
        (package-build--find-parse-time "\
\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \
[0-9]\\{2\\}:[0-9]\\{2\\}\\( [+-][0-9]\\{4\\}\\)?\\)")))))

(defun package-build--checkout-bitbucket (name config dir)
  "Check package NAME with config CONFIG out of bitbucket into DIR."
  (let ((url (format "https://bitbucket.com/%s" (plist-get config :repo))))
    (package-build--checkout-hg name (plist-put (copy-sequence config) :url url) dir)))

;;; Utilities

(defun package-build--dump (data file &optional pretty-print)
  "Write DATA to FILE as a Lisp sexp.
Optionally PRETTY-PRINT the data."
  (with-temp-file file
    (package-build--message "File: %s" file)
    (if pretty-print
        (pp data (current-buffer))
      (print data (current-buffer)))))

(defun package-build--write-pkg-file (pkg-file pkg-info)
  "Write PKG-FILE containing PKG-INFO."
  (with-temp-file pkg-file
    (pp
     `(define-package
        ,(aref pkg-info 0)
        ,(aref pkg-info 3)
        ,(aref pkg-info 2)
        ',(mapcar
           (lambda (elt)
             (list (car elt)
                   (package-version-join (cadr elt))))
           (aref pkg-info 1))
        ;; Append our extra information
        ,@(cl-mapcan (lambda (entry)
                       (let ((value (cdr entry)))
                         (when (or (symbolp value) (listp value))
                           ;; We must quote lists and symbols,
                           ;; because Emacs 24.3 and earlier evaluate
                           ;; the package information, which would
                           ;; break for unquoted symbols or lists
                           (setq value (list 'quote value)))
                         (list (car entry) value)))
                     (when (> (length pkg-info) 4)
                       (aref pkg-info 4))))
     (current-buffer))
    (princ ";; Local Variables:\n;; no-byte-compile: t\n;; End:\n"
           (current-buffer))))

(defun package-build--read-from-file (file)
  "Read and return the Lisp data stored in FILE, or nil if no such file exists."
  (when (file-exists-p file)
    (car (read-from-string (package-build--slurp-file file)))))

(defun package-build--create-tar (file dir &optional files)
  "Create a tar FILE containing the contents of DIR, or just FILES if non-nil."
  (when (eq system-type 'windows-nt)
    (setq file (replace-regexp-in-string "^\\([a-z]\\):" "/\\1" file)))
  (apply 'process-file
         package-build-tar-executable nil
         (get-buffer-create "*package-build-checkout*")
         nil "-cvf"
         file
         "--exclude=.git"
         "--exclude=.hg"
         (or (mapcar (lambda (fn) (concat dir "/" fn)) files) (list dir))))

(defun package-build--find-package-commentary (file-path)
  "Get commentary section from FILE-PATH."
  (when (file-exists-p file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (lm-commentary))))

(defun package-build--write-pkg-readme (target-dir commentary file-name)
  "In TARGET-DIR, write COMMENTARY to a -readme.txt file prefixed with FILE-NAME."
  (when commentary
    (with-temp-buffer
      (insert commentary)
      ;; Adapted from `describe-package-1'.
      (goto-char (point-min))
      (save-excursion
        (when (re-search-forward "^;;; Commentary:\n" nil t)
          (replace-match ""))
        (while (re-search-forward "^\\(;+ ?\\)" nil t)
          (replace-match ""))
        (goto-char (point-min))
        (when (re-search-forward "\\`\\( *\n\\)+" nil t)
          (replace-match "")))
      (delete-trailing-whitespace)
      (let ((coding-system-for-write buffer-file-coding-system))
        (write-region nil nil
                      (package-build--readme-file-name target-dir file-name))))))

(defun package-build--readme-file-name (target-dir file-name)
  "Name of the readme file in TARGET-DIR for the package FILE-NAME."
  (expand-file-name (concat file-name "-readme.txt")
                    target-dir))

(defun package-build--update-or-insert-version (version)
  "Ensure current buffer has a \"Package-Version: VERSION\" header."
  (goto-char (point-min))
  (if (let ((case-fold-search t))
        (re-search-forward "^;+* *Package-Version *: *" nil t))
      (progn
        (move-beginning-of-line nil)
        (search-forward "V" nil t)
        (backward-char)
        (insert "X-Original-")
        (move-beginning-of-line nil))
    ;; Put the new header in a sensible place if we can
    (re-search-forward "^;+* *\\(Version\\|Package-Requires\\|Keywords\\|URL\\) *:"
                       nil t)
    (forward-line))
  (insert (format ";; Package-Version: %s" version))
  (newline))

(defun package-build--ensure-ends-here-line (file-path)
  "Add a 'FILE-PATH ends here' trailing line if missing."
  (save-excursion
    (goto-char (point-min))
    (let ((trailer (concat ";;; "
                           (file-name-nondirectory file-path)
                           " ends here")))
      (unless (search-forward trailer nil t)
        (goto-char (point-max))
        (newline)
        (insert trailer)
        (newline)))))

(defun package-build--get-package-info (file-path)
  "Get a vector of package info from the docstrings in FILE-PATH."
  (when (file-exists-p file-path)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents file-path)
        ;; next few lines are a hack for some packages that aren't
        ;; commented properly.
        (package-build--update-or-insert-version "0")
        (package-build--ensure-ends-here-line file-path)
        (cl-flet ((package-strip-rcs-id (str) "0"))
          (package-build--package-buffer-info-vec))))))

(defun package-build--get-pkg-file-info (file-path)
  "Get a vector of package info from \"-pkg.el\" file FILE-PATH."
  (when (file-exists-p file-path)
    (let ((package-def (package-build--read-from-file file-path)))
      (if (eq 'define-package (car package-def))
          (let* ((pkgfile-info (cdr package-def))
                 (descr (nth 2 pkgfile-info))
                 (rest-plist (cl-subseq pkgfile-info (min 4 (length pkgfile-info))))
                 (extras (let (alist)
                           (while rest-plist
                             (unless (memq (car rest-plist) '(:kind :archive))
                               (let ((value (cadr rest-plist)))
                                 (when value
                                   (push (cons (car rest-plist)
                                               (if (eq (car-safe value) 'quote)
                                                   (cadr value)
                                                 value))
                                         alist))))
                             (setq rest-plist (cddr rest-plist)))
                           alist)))
            (when (string-match "[\r\n]" descr)
              (error "Illegal multi-line package description in %s" file-path))
            (vector
             (nth 0 pkgfile-info)
             (mapcar
              (lambda (elt)
                (unless (symbolp (car elt))
                  (error "Invalid package name in dependency: %S" (car elt)))
                (list (car elt) (version-to-list (cadr elt))))
              (eval (nth 3 pkgfile-info)))
             descr
             (nth 1 pkgfile-info)
             extras))
        (error "No define-package found in %s" file-path)))))

(defun package-build--merge-package-info (pkg-info name version)
  "Return a version of PKG-INFO updated with NAME, VERSION and info from CONFIG.
If PKG-INFO is nil, an empty one is created."
  (let ((merged (or (copy-sequence pkg-info)
                    (vector name nil "No description available." version))))
    (aset merged 0 name)
    (aset merged 3 version)
    merged))

(defun package-build--archive-entry (pkg-info type)
  "Return the archive-contents cons cell for PKG-INFO and TYPE."
  (let ((name (intern (aref pkg-info 0)))
        (requires (aref pkg-info 1))
        (desc (or (aref pkg-info 2) "No description available."))
        (version (aref pkg-info 3))
        (extras (and (> (length pkg-info) 4)
                     (aref pkg-info 4))))
    (cons name
          (vector (version-to-list version)
                  requires
                  desc
                  type
                  extras))))

(defun package-build--archive-file-name (archive-entry)
  "Return the path of the file in which the package for ARCHIVE-ENTRY is stored."
  (let* ((name (car archive-entry))
         (pkg-info (cdr archive-entry))
         (version (package-version-join (aref pkg-info 0)))
         (flavour (aref pkg-info 3)))
    (expand-file-name
     (format "%s-%s.%s" name version (if (eq flavour 'single) "el" "tar"))
     package-build-archive-dir)))

(defun package-build--entry-file-name (archive-entry)
  "Return the path of the file in which the package for ARCHIVE-ENTRY is stored."
  (let* ((name (car archive-entry))
         (pkg-info (cdr archive-entry))
         (version (package-version-join (aref pkg-info 0))))
    (expand-file-name
     (format "%s-%s.entry" name version)
     package-build-archive-dir)))

(defun package-build--delete-file-if-exists (file)
  "Delete FILE if it exists."
  (when (file-exists-p file)
    (delete-file file)))

(defun package-build--remove-archive-files (archive-entry)
  "Remove ARCHIVE-ENTRY from archive-contents, and delete associated file.
Note that the working directory (if present) is not deleted by
this function, since the archive list may contain another version
of the same-named package which is to be kept."
  (package-build--message "Removing archive: %s" archive-entry)
  (mapcar 'package-build--delete-file-if-exists
          (list  (package-build--archive-file-name archive-entry)
                 (package-build--entry-file-name archive-entry))))

;;; Recipes

(defun package-build--read-recipe (name)
  "Return the recipe of the package named NAME as a list.
It performs some basic checks on the recipe to ensure that known
keys have values of the right types, and raises an error if that
is the not the case.  If invalid combinations of keys are
supplied then errors will only be caught when an attempt is made
to build the recipe."
  (let* ((recipe (package-build--read-from-file
                  (expand-file-name name package-build-recipes-dir)))
         (ident (car recipe))
         (plist (cdr recipe)))
    (cl-assert ident)
    (cl-assert (symbolp ident))
    (cl-assert (string= (symbol-name ident) name)
               nil "Recipe '%s' contains mismatched package name '%s'"
               name ident)
    (cl-assert plist)
    (let* ((symbol-keys '(:fetcher))
           (string-keys '(:url :repo :module :commit :branch :version-regexp))
           (list-keys '(:files :old-names))
           (all-keys (append symbol-keys string-keys list-keys)))
      (dolist (thing plist)
        (when (keywordp thing)
          (cl-assert (memq thing all-keys) nil "Unknown keyword %S" thing)))
      (let ((fetcher (plist-get plist :fetcher)))
        (cl-assert fetcher nil ":fetcher is missing")
        (when (memq fetcher '(github gitlab bitbucket))
          (cl-assert (plist-get plist :repo) ":repo is missing")))
      (dolist (key symbol-keys)
        (let ((val (plist-get plist key)))
          (when val
            (cl-assert (symbolp val) nil "%s must be a symbol but is %S" key val))))
      (dolist (key list-keys)
        (let ((val (plist-get plist key)))
          (when val
            (cl-assert (listp val) nil "%s must be a list but is %S" key val))))
      (dolist (key string-keys)
        (let ((val (plist-get plist key)))
          (when val
            (cl-assert (stringp val) nil "%s must be a string but is %S" key val)))))
    recipe))

(defun package-build--read-recipes ()
  "Return a list of data structures for all recipes."
  (mapcar #'package-build--read-recipe
          (directory-files package-build-recipes-dir nil "^[^.]")))

(defun package-build--read-recipes-ignore-errors ()
  "Return a list of data structures for all recipes."
  (cl-mapcan (lambda (name)
               (condition-case err
                   (list (package-build--read-recipe name))
                 (error (package-build--message "Error reading recipe %s: %s"
                                                name (error-message-string err))
                        nil)))
             (directory-files package-build-recipes-dir nil "^[^.]")))

(defun package-build-expand-file-specs (dir specs &optional subdir allow-empty)
  "In DIR, expand SPECS, optionally under SUBDIR.
The result is a list of (SOURCE . DEST), where SOURCE is a source
file path and DEST is the relative path to which it should be copied.

If the resulting list is empty, an error will be reported.  Pass t
for ALLOW-EMPTY to prevent this error."
  (let ((default-directory dir)
        (prefix (if subdir (format "%s/" subdir) ""))
        (lst))
    (dolist (entry specs lst)
      (setq lst
            (if (consp entry)
                (if (eq :exclude (car entry))
                    (cl-nset-difference lst
                                        (package-build-expand-file-specs
                                         dir (cdr entry) nil t)
                                        :key 'car
                                        :test 'equal)
                  (nconc lst
                         (package-build-expand-file-specs
                          dir
                          (cdr entry)
                          (concat prefix (car entry))
                          t)))
              (nconc
               lst (mapcar (lambda (f)
                             (let ((destname)))
                             (cons f
                                   (concat prefix
                                           (replace-regexp-in-string
                                            "\\.in\\'"
                                            ""
                                            (file-name-nondirectory f)))))
                           (file-expand-wildcards entry))))))
    (when (and (null lst) (not allow-empty))
      (error "No matching file(s) found in %s: %s" dir specs))
    lst))

(defun package-build--config-file-list (config)
  "Get the :files spec from CONFIG, or return `package-build-default-files-spec'."
  (let ((file-list (plist-get config :files)))
    (cond
     ((null file-list)
      package-build-default-files-spec)
     ((eq :defaults (car file-list))
      (append package-build-default-files-spec (cdr file-list)))
     (t
      file-list))))

(defun package-build--expand-source-file-list (dir config)
  "Shorthand way to expand paths in DIR for source files listed in CONFIG."
  (mapcar 'car
          (package-build-expand-file-specs
           dir (package-build--config-file-list config))))

(defun package-build--generate-info-files (files source-dir target-dir)
  "Create .info files from any .texi files listed in FILES.

The source and destination file paths are expanded in SOURCE-DIR
and TARGET-DIR respectively.

Any of the original .texi(nfo) files found in TARGET-DIR are
deleted."
  (dolist (spec files)
    (let* ((source-file (car spec))
           (source-path (expand-file-name source-file source-dir))
           (dest-file (cdr spec))
           (info-path (expand-file-name
                       (concat (file-name-sans-extension dest-file) ".info")
                       target-dir)))
      (when (string-match ".texi\\(nfo\\)?$" source-file)
        (when (not (file-exists-p info-path))
          (with-current-buffer (get-buffer-create "*package-build-info*")
            (ignore-errors
              (package-build--run-process
               (file-name-directory source-path)
               "makeinfo"
               source-path
               "-o"
               info-path)
              (package-build--message "Created %s" info-path))))
        (package-build--message "Removing %s"
                                (expand-file-name dest-file target-dir))
        (delete-file (expand-file-name dest-file target-dir))))))

;;; Info Manuals

(defun package-build--generate-dir-file (files target-dir)
  "Create dir file from any .info files listed in FILES in TARGET-DIR."
  (dolist (spec files)
    (let* ((source-file (car spec))
           (dest-file (cdr spec))
           (info-path (expand-file-name
                       (concat (file-name-sans-extension dest-file) ".info")
                       target-dir)))
      (when (and (or (string-match ".info$" source-file)
                     (string-match ".texi\\(nfo\\)?$" source-file))
                 (file-exists-p info-path))
        (with-current-buffer (get-buffer-create "*package-build-info*")
          (ignore-errors
            (package-build--run-process
             nil
             "install-info"
             (concat "--dir=" (expand-file-name "dir" target-dir))
             info-path)))))))

;;; Utilities

(defun package-build--copy-package-files (files source-dir target-dir)
  "Copy FILES from SOURCE-DIR to TARGET-DIR.
FILES is a list of (SOURCE . DEST) relative filepath pairs."
  (cl-loop for (source-file . dest-file) in files
           do (package-build--copy-file
               (expand-file-name source-file source-dir)
               (expand-file-name dest-file target-dir))))

(defun package-build--copy-file (file newname)
  "Copy FILE to NEWNAME and create parent directories for NEWNAME if they don't exist."
  (let ((newdir (file-name-directory newname)))
    (unless (file-exists-p newdir)
      (make-directory newdir t)))
  (cond
   ((file-regular-p file)
    (package-build--message "%s -> %s" file newname)
    (copy-file file newname))
   ((file-directory-p file)
    (package-build--message "%s => %s" file newname)
    (copy-directory file newname))))

(defun package-build--package-name-completing-read ()
  "Read the name of a package and return it as a string."
  (completing-read "Package: " (package-build-packages)))

(defun package-build--find-source-file (target files)
  "Search for source of TARGET in FILES."
  (car (rassoc target files)))

(defun package-build--find-package-file (name)
  "Return the most recently built archive of the package named NAME."
  (package-build--archive-file-name (assoc name (package-build-archive-alist))))

(defun package-build--package-buffer-info-vec ()
  "Return a vector of package info.
`package-buffer-info' returns a vector in older Emacs versions,
and a cl struct in Emacs HEAD.  This wrapper normalises the results."
  (let ((desc (package-buffer-info))
        (keywords (lm-keywords-list)))
    (if (fboundp 'package-desc-create)
        (let ((extras (package-desc-extras desc)))
          (when (and keywords (not (assq :keywords extras)))
            ;; Add keywords to package properties, if not already present
            (push (cons :keywords keywords) extras))
          (vector (package-desc-name desc)
                  (package-desc-reqs desc)
                  (package-desc-summary desc)
                  (package-desc-version desc)
                  extras))
      ;; The regexp and the processing is taken from `lm-homepage' in Emacs 24.4
      (let* ((page (lm-header "\\(?:x-\\)?\\(?:homepage\\|url\\)"))
             (homepage (if (and page (string-match "^<.+>$" page))
                           (substring page 1 -1)
                         page))
             extras)
        (when keywords (push (cons :keywords keywords) extras))
        (when homepage (push (cons :url homepage) extras))
        (vector  (aref desc 0)
                 (aref desc 1)
                 (aref desc 2)
                 (aref desc 3)
                 extras)))))

(defconst package-build--this-file load-file-name)

;; TODO: This function should be fairly sound, but it has a few
;; possible failure modes. Primarily, if a file matching the recipe's
;; file spec appears in a new upstream revision, but that file has an
;; older date than the version timestamp provided here, the function
;; will return t.
(defun package-build--up-to-date-p (name version)
  "Return non-nil if there is an up-to-date package named NAME with the given VERSION."
  (let* ((package-file-base (expand-file-name (format "%s-%s." name version)
                                              package-build-archive-dir))
         (recipe-file (expand-file-name name package-build-recipes-dir)))
    (cl-dolist (ext '("tar" "el"))
      (let ((package-file (concat package-file-base ext)))
        (when (and (file-newer-than-file-p package-file recipe-file)
                   (or (null package-build--this-file)
                       (file-newer-than-file-p package-file
                                               package-build--this-file)))
          (cl-return t))))))

(defun package-build-get-commit (config working-dir)
  "Return a commit identifier as a string for CONFIG under WORKING-DIR."
  (let* ((fetcher (plist-get config :fetcher))
         (func (intern (format "package-build--get-commit-%s" fetcher))))
    (when (functionp func)
      (funcall func config (file-name-as-directory working-dir)))))

(defun package-build--get-commit-git (config working-dir)
  "Return a commit identifier.
Works for Git repositories with CONFIG under WORKING-DIR."
  (package-build--git-head-sha working-dir))
(defalias 'package-build--get-commit-github #'package-build--get-commit-git)
(defalias 'package-build--get-commit-gitlab #'package-build--get-commit-git)

(defun package-build-add-to-archive (archive-entry prop value)
  "Add to ARCHIVE-ENTRY property PROP with VALUE.
ARCHIVE-ENTRY is destructively modified."
  (push (cons prop value) (elt (cdr archive-entry) 4))
  archive-entry)

;;; Building

;;;###autoload
(defun package-build-archive (name)
  "Build a package archive for the package named NAME."
  (interactive (list (package-build--package-name-completing-read)))
  (let ((rcp (or (cdr (assoc (intern name)
                             (package-build-recipe-alist)))
                 (error "Cannot find package %s" name)))
        (pkg-working-dir
         (file-name-as-directory
          (expand-file-name name package-build-working-dir))))

    (unless (file-exists-p package-build-archive-dir)
      (package-build--message "Creating directory %s" package-build-archive-dir)
      (make-directory package-build-archive-dir))

    (package-build--message "\n;;; %s\n" name)
    (let* ((version (package-build-checkout name rcp pkg-working-dir))
           (commit (package-build-get-commit rcp pkg-working-dir))
           (default-directory package-build-working-dir)
           (start-time (current-time)))
      (if (package-build--up-to-date-p name version)
          (package-build--message "Package %s is up to date - skipping." name)
        (progn
          (let ((archive-entry (package-build-package
                                name version
                                (package-build--config-file-list rcp)
                                pkg-working-dir
                                package-build-archive-dir)))
            (when commit
              (package-build-add-to-archive archive-entry :commit commit))
            (package-build--dump archive-entry
                                 (package-build--entry-file-name archive-entry)))
          (when package-build-write-melpa-badge-images
            (package-build--write-melpa-badge-image
             name version package-build-archive-dir))
          (package-build--message "Built %s in %.3fs, finished at %s"
                                  name
                                  (float-time (time-since start-time))
                                  (current-time-string))))
      (list name version))))

(defun package-build-archive-ignore-errors (name)
  "Build archive for the package named NAME, ignoring any errors."
  (interactive (list (package-build--package-name-completing-read)))
  (let* ((debug-on-error t)
         (debug-on-signal t)
         (package-build--debugger-return nil)
         (debugger (lambda (&rest args)
                     (setq package-build--debugger-return
                           (with-output-to-string (backtrace))))))
    (condition-case err
        (package-build-archive name)
      (error
       (package-build--message "%s" (error-message-string err))
       nil))))

;;;###autoload
(defun package-build-package (name version file-specs source-dir target-dir)
  "Create version VERSION of the package named NAME.

The information in FILE-SPECS is used to gather files from
SOURCE-DIR.

The resulting package will be stored as a .el or .tar file in
TARGET-DIR, depending on whether there are multiple files.

Argument FILE-SPECS is a list of specs for source files, which
should be relative to SOURCE-DIR.  The specs can be wildcards,
and optionally specify different target paths.  They extended
syntax is currently only documented in the MELPA README.  You can
simply pass `package-build-default-files-spec' in most cases.

Returns the archive entry for the package."
  (let ((files (package-build-expand-file-specs source-dir file-specs)))
    (unless (equal file-specs package-build-default-files-spec)
      (when (equal files (package-build-expand-file-specs
                          source-dir package-build-default-files-spec nil t))
        (package-build--message
         "Note: %s :files spec is equivalent to the default." name)))
    (cond
     ((not version)
      (error "Unable to check out repository for %s" name))
     ((= 1 (length files))
      (package-build--build-single-file-package
       name version (caar files) source-dir target-dir))
     ((< 1 (length  files))
      (package-build--build-multi-file-package
       name version files source-dir target-dir))
     (t (error "Unable to find files matching recipe patterns")))))

(defun package-build--build-single-file-package
    (package-name version file source-dir target-dir)
  (let* ((pkg-source (expand-file-name file source-dir))
         (pkg-target (expand-file-name
                      (concat package-name "-" version ".el")
                      target-dir))
         (pkg-info (package-build--merge-package-info
                    (package-build--get-package-info pkg-source)
                    package-name
                    version)))
    (unless (string-equal (downcase (concat package-name ".el"))
                          (downcase (file-name-nondirectory pkg-source)))
      (error "Single file %s does not match package name %s"
             (file-name-nondirectory pkg-source) package-name))
    (if (file-exists-p pkg-target)
        (package-build--message "Skipping rebuild of %s" pkg-target)
      (copy-file pkg-source pkg-target)
      (let ((enable-local-variables nil)
            (make-backup-files nil))
        (with-current-buffer (find-file pkg-target)
          (package-build--update-or-insert-version version)
          (package-build--ensure-ends-here-line pkg-source)
          (write-file pkg-target nil)
          (condition-case err
              (package-build--package-buffer-info-vec)
            (error
             (package-build--message "Warning: %S" err)))
          (kill-buffer)))

      (package-build--write-pkg-readme
       target-dir
       (package-build--find-package-commentary pkg-source)
       package-name))
    (package-build--archive-entry pkg-info 'single)))

(defun package-build--build-multi-file-package
    (package-name version files source-dir target-dir)
  (let ((tmp-dir (file-name-as-directory (make-temp-file package-name t))))
    (unwind-protect
        (let* ((pkg-dir-name (concat package-name "-" version))
               (pkg-tmp-dir (expand-file-name pkg-dir-name tmp-dir))
               (pkg-file (concat package-name "-pkg.el"))
               (pkg-file-source (or (package-build--find-source-file pkg-file files)
                                    pkg-file))
               (file-source (concat package-name ".el"))
               (pkg-source (or (package-build--find-source-file file-source files)
                               file-source))
               (pkg-info (package-build--merge-package-info
                          (let ((default-directory source-dir))
                            (or (package-build--get-pkg-file-info pkg-file-source)
                                ;; some packages (like magit) provide name-pkg.el.in
                                (package-build--get-pkg-file-info
                                 (expand-file-name (concat pkg-file ".in")
                                                   (file-name-directory pkg-source)))
                                (package-build--get-package-info pkg-source)))
                          package-name
                          version)))
          (package-build--copy-package-files files source-dir pkg-tmp-dir)
          (package-build--write-pkg-file (expand-file-name
                                          pkg-file
                                          (file-name-as-directory pkg-tmp-dir))
                                         pkg-info)

          (package-build--generate-info-files files source-dir pkg-tmp-dir)
          (package-build--generate-dir-file files pkg-tmp-dir)

          (let ((default-directory tmp-dir))
            (package-build--create-tar
             (expand-file-name (concat package-name "-" version ".tar")
                               target-dir)
             pkg-dir-name))

          (let ((default-directory source-dir))
            (package-build--write-pkg-readme
             target-dir
             (package-build--find-package-commentary pkg-source)
             package-name))
          (package-build--archive-entry pkg-info 'tar))
      (delete-directory tmp-dir t nil))))

;;;###autoload
(defun package-build-all ()
  "Build all packages in the `package-build-recipe-alist'."
  (interactive)
  (let ((failed (cl-loop for pkg in (package-build-packages)
                         when (not (package-build-archive-ignore-errors pkg))
                         collect pkg)))
    (if (not failed)
        (princ "\nSuccessfully Compiled All Packages\n")
      (princ "\nFailed to Build the Following Packages\n")
      (princ (mapconcat #'symbol-name failed "\n"))
      (princ "\n")))
  (package-build-cleanup))

(defun package-build-cleanup ()
  "Remove previously-built packages that no longer have recipes."
  (interactive)
  (let* ((known-package-names (package-build-packages))
         (stale-archives (cl-loop for built in (package-build--archive-entries)
                                  when (not (memq (car built) known-package-names))
                                  collect built)))
    (mapc 'package-build--remove-archive-files stale-archives)
    (package-build-dump-archive-contents)))

(defun package-build-recipe-alist ()
  "Return the list of available package recipes."
  (or package-build--recipe-alist
      (setq package-build--recipe-alist
            (package-build--read-recipes-ignore-errors))))

(defun package-build-packages ()
  "Return the list of the names of available packages."
  (mapcar #'car (package-build-recipe-alist)))

(defun package-build-archive-alist ()
  "Return the archive list."
  (cdr (package-build--read-from-file
        (expand-file-name "archive-contents"
                          package-build-archive-dir))))

(defun package-build-reinitialize ()
  "Forget any information about packages which have already been built."
  (interactive)
  (setq package-build--recipe-alist nil))

(defun package-build-dump-archive-contents (&optional file pretty-print)
  "Dump the list of built packages to FILE.

If FILE-NAME is not specified, the default archive-contents file is used."
  (package-build--dump (cons 1 (package-build--archive-entries))
                       (or file
                           (expand-file-name "archive-contents"
                                             package-build-archive-dir))
                       pretty-print))

(defun package-build--archive-entries ()
  "Read all .entry files from the archive directory and return a list of all entries."
  (let ((entries '()))
    (dolist (new (mapcar 'package-build--read-from-file
                         (directory-files package-build-archive-dir t
                                          ".*\.entry$"))
                 entries)
      (let ((old (assq (car new) entries)))
        (when old
          (when (version-list-< (elt (cdr new) 0)
                                (elt (cdr old) 0))
            ;; swap old and new
            (cl-rotatef old new))
          (package-build--remove-archive-files old)
          (setq entries (remove old entries)))
        (add-to-list 'entries new)))))

;;; Exporting Data as Json

(defun package-build-recipe-alist-as-json (file)
  "Dump the recipe list to FILE as json."
  (interactive)
  (with-temp-file file
    (insert (json-encode (package-build-recipe-alist)))))

(defun package-build--pkg-info-for-json (info)
  "Convert INFO into a data structure which will serialize to JSON in the desired shape."
  (let ((ver (elt info 0))
        (deps (elt info 1))
        (desc (elt info 2))
        (type (elt info 3))
        (props (and (> (length info) 4)
                    (elt info 4))))
    (list :ver ver
          :deps (cl-mapcan (lambda (dep)
                             (list (intern (format ":%s" (car dep)))
                                   (cadr dep)))
                           deps)
          :desc desc
          :type type
          :props props)))

(defun package-build--archive-alist-for-json ()
  "Return the archive alist in a form suitable for JSON encoding."
  (cl-mapcan (lambda (entry)
               (list (intern (format ":%s" (car entry)))
                     (package-build--pkg-info-for-json (cdr entry))))
             (package-build-archive-alist)))

(defun package-build-archive-alist-as-json (file)
  "Dump the build packages list to FILE as json."
  (with-temp-file file
    (insert (json-encode (package-build--archive-alist-for-json)))))

(provide 'package-build)

;; For the time being just require all libraries that contain code
;; that was previously located in this library.

(require 'package-build-badges)
(require 'package-recipe-mode)

;; Local Variables:
;; coding: utf-8
;; checkdoc-minor-mode: 1
;; indent-tabs-mode: nil
;; End:
;;; package-build.el ends here
