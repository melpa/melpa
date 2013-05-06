;;; package-build.el --- Tools for curating the package archive

;; Copyright (C) 2011-2013 Donald Ephraim Curtis <dcurtis@milkbox.net>
;; Copyright (C) 2012-2013 Steve Purcell <steve@sanityinc.com>
;; Copyright (C) 2009 Phil Hagelberg <technomancy@gmail.com>

;; Author: Donald Ephraim Curtis <dcurtis@milkbox.net>
;; Created: 2011-09-30
;; Version: 0.1
;; Keywords: tools
;; Package-Requires: ((cl-lib "0.2"))

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

;; The archive is generated from an index, which contains a list of
;; projects and repositories from which to get them.  The term
;; "package" here is used to mean a specific version of a project that
;; is prepared for download and installation.

;;; Code:

;; Since this library is not meant to be loaded by users
;; at runtime, use of cl functions should not be a problem.
(require 'cl-lib)

(require 'package)
(require 'lisp-mnt)

(defconst pb/this-dir (file-name-directory (or load-file-name (buffer-file-name))))

(defcustom package-build-working-dir (expand-file-name "working/" pb/this-dir)
  "Directory in which to keep checkouts."
  :group 'package-build
  :type 'string)

(defcustom package-build-archive-dir (expand-file-name "packages/" pb/this-dir)
  "Directory in which to keep compiled archives."
  :group 'package-build
  :type 'string)

(defcustom package-build-recipes-dir (expand-file-name "recipes/" pb/this-dir)
  "Directory containing recipe files."
  :group 'package-build
  :type 'string)


;;; Internal Variables

(defvar pb/recipe-alist nil
  "Internal list of package build specs.

Do not use this directly. Use `package-build-recipe-alist'
function.")

(defvar pb/recipe-alist-initialized nil
  "Determines if `pb/recipe-alist` has been initialized.")

(defvar pb/archive-alist nil
  "Internal list of already-built packages, in the standard package.el format.

Do not use this directly. Use `package-build-archive-alist'
function for access to this function")

(defvar pb/archive-alist-initialized nil
  "Determines if pb/archive-alist has been initialized.")

(defconst pb/default-files-spec '("*.el" "dir" "*.info")
  "Default value for :files attribute in recipes.")


(defun pb/slurp-file (file-name)
  "Return the contents of FILE-NAME as a string, or nil if no such file exists."
  (when (file-exists-p file-name)
    (with-temp-buffer
      (insert-file-contents-literally file-name)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun pb/string-rtrim (str)
  "Remove trailing whitespace from `STR'."
  (replace-regexp-in-string "[ \t\n]*$" "" str))

(defun pb/parse-time (str)
  "Parse STR as a time, and format as a YYYYMMDD.HHMM string."
  ;; We remove zero-padding the HH portion, as it is lost
  ;; when stored in the archive-contents
  (let* ((s (substring-no-properties str))
         (time (condition-case nil
                   (date-to-time s)
                 (error
                  ;; Handle newer CVS formats like "2001/08/26 22:16:22"
                  ;; which break date-to-time
                  (date-to-time (replace-regexp-in-string "/" "-" s))))))
    (concat (format-time-string "%Y%m%d." time)
            (format "%d" (or (string-to-number (format-time-string "%H%M" time)) 0)))))

(defun pb/string-match-all (regex str &optional group)
  "Find every match for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let (result
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

(defun pb/find-parse-time (regex &optional bound)
  "Find REGEX in current buffer and format as a time version, optionally looking only as far as BOUND."
  (pb/parse-time (progn (re-search-backward regex bound)
                        (match-string-no-properties 1))))

(defun pb/find-parse-time-latest (regex &optional bound)
  "Find the latest timestamp matching REGEX, optionally looking only as far as BOUND."
  (let* ((text (buffer-substring-no-properties
                (or bound (point-min)) (point)))
         (times (mapcar 'pb/parse-time (pb/string-match-all regex text 1))))
    (car (nreverse (sort times 'string<)))))

(defun pb/run-process (dir prog &rest args)
  "In DIR (or `default-directory' if unset) run command PROG with ARGS.
Output is written to the current buffer."
  (let ((default-directory (or dir default-directory)))
    (let ((exit-code (apply 'process-file prog nil (current-buffer) t args)))
      (unless (zerop exit-code)
        (error "Program '%s' with args '%s' exited with non-zero status %d"
               prog args exit-code)))))

(defun pb/run-process-match (regex dir prog &rest args)
  "Find match for REGEX when - in DIR, or `default-directory' if unset - we run PROG with ARGS."
  (with-temp-buffer
    (apply 'pb/run-process dir prog args)
    (goto-char (point-min))
    (re-search-forward regex)
    (match-string-no-properties 1)))


(defun pb/checkout (name config cwd)
  "Check out source for package NAME with CONFIG under working dir CWD.
In turn, this function uses the :fetcher option in the config to
choose a source-specific fetcher function, which it calls with
the same arguments."
  (let ((repo-type (plist-get config :fetcher)))
    (message "Fetcher: %s" repo-type)
    (unless (eq 'wiki repo-type)
      (message "Source: %s\n" (or (plist-get config :repo) (plist-get config :url))))
    (funcall (intern (format "pb/checkout-%s" repo-type))
             name config cwd)))

(defvar pb/last-wiki-fetch-time 0
  "The time at which an emacswiki URL was last requested.
This is used to avoid exceeding the rate limit of 1 request per 2
seconds; the server cuts off after 10 requests in 20 seconds.")

(defvar pb/wiki-min-request-interval 2
  "The shortest permissible interval between successive requests for Emacswiki URLs.")

(defmacro pb/with-wiki-rate-limit (&rest body)
  "Rate-limit BODY code passed to this macro to match EmacsWiki's rate limiting."
  (let ((now (cl-gensym))
        (elapsed (cl-gensym)))
    `(let* ((,now (float-time))
            (,elapsed (- ,now pb/last-wiki-fetch-time)))
       (when (< ,elapsed pb/wiki-min-request-interval)
         (let ((wait (- pb/wiki-min-request-interval ,elapsed)))
           (message "Waiting %.2f secs before hitting Emacswiki again" wait)
           (sleep-for wait)))
       (unwind-protect
           (progn ,@body)
         (setq pb/last-wiki-fetch-time (float-time))))))

(defun pb/grab-wiki-file (filename)
  "Download FILENAME from emacswiki, returning its last-modified time."
  (let* ((download-url
          (format "http://www.emacswiki.org/emacs/download/%s" filename))
         (wiki-url
          (format "http://www.emacswiki.org/emacs/%s" filename)))
    (pb/with-wiki-rate-limit
     (url-copy-file download-url filename t))
    (when (zerop (nth 7 (file-attributes filename)))
      (error "Wiki file %s was empty - has it been removed?" filename))
    ;; The Last-Modified response header for the download is actually
    ;; correct for the file, but we have no access to that
    ;; header. Instead, we must query the non-raw emacswiki page for
    ;; the file.
    ;; Since those Emacswiki lookups are time-consuming, we maintain a
    ;; foo.el.stamp file containing ("SHA1" . "PARSED_TIME")
    (let* ((new-content-hash (secure-hash 'sha1 (pb/slurp-file filename)))
           (stamp-file (concat filename ".stamp"))
           (stamp-info (pb/read-from-file stamp-file))
           (prev-content-hash (car stamp-info)))
      (if (and prev-content-hash
               (string-equal new-content-hash prev-content-hash))
          ;; File has not changed, so return old timestamp
          (progn
            (message "%s is unchanged" filename)
            (cdr stamp-info))
        (message "%s has changed - checking mod time" filename)
        (let ((new-timestamp
               (with-current-buffer (pb/with-wiki-rate-limit
                                     (url-retrieve-synchronously wiki-url))
                 (pb/find-parse-time
                  "Last edited \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\} [A-Z]\\{3\\}\\)"))))
          (pb/dump (cons new-content-hash new-timestamp) stamp-file)
          new-timestamp)))))

(defun pb/checkout-wiki (name config dir)
  "Checkout package NAME with config CONFIG from the EmacsWiki into DIR."
  (with-current-buffer (get-buffer-create "*package-build-checkout*")
    (unless (file-exists-p dir)
      (make-directory dir))
    (let ((files (or (plist-get config :files)
                     (list (format "%s.el" name))))
          (default-directory dir))
      (car (nreverse (sort (mapcar 'pb/grab-wiki-file files) 'string-lessp))))))

(defun pb/darcs-repo (dir)
  "Get the current darcs repo for DIR."
  (pb/run-process-match "Default Remote: \\(.*\\)" dir "darcs" "show" "repo"))

(defun pb/checkout-darcs (name config dir)
  "Check package NAME with config CONFIG out of darcs into DIR."
  (let ((repo (plist-get config :url)))
    (with-current-buffer (get-buffer-create "*package-build-checkout*")
      (cond
       ((and (file-exists-p (expand-file-name "_darcs" dir))
             (string-equal (pb/darcs-repo dir) repo))
        (pb/princ-exists dir)
        (pb/run-process dir "darcs" "pull"))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t nil))
        (pb/princ-checkout repo dir)
        (pb/run-process nil "darcs" "get" repo dir)))
      (apply 'pb/run-process dir "darcs" "changes" "--max-count" "1"
             (pb/expand-source-file-list dir config))
      (pb/find-parse-time
       "\\([a-zA-Z]\\{3\\} [a-zA-Z]\\{3\\} \\( \\|[0-9]\\)[0-9] [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\} [A-Za-z]\\{3\\} [0-9]\\{4\\}\\)"))))

(defun pb/svn-repo (dir)
  "Get the current svn repo for DIR."
  (pb/run-process-match "URL: \\(.*\\)" dir "svn" "info"))

(defun pb/trim (str &optional chr)
  "Return a copy of STR without any trailing CHR (or space if unspecified)."
  (if (equal (elt str (1- (length str))) (or chr ? ))
      (substring str 0 (1- (length str)))
    str))

(defun pb/princ-exists (dir)
  "Print a message that the contents of DIR will be updated."
  (message "Updating %s" dir))

(defun pb/princ-checkout (repo dir)
  "Print a message that REPO will be checked out into DIR."
  (message "Cloning %s to %s" repo dir))

(defun pb/checkout-svn (name config dir)
  "Check package NAME with config CONFIG out of svn into DIR."
  (with-current-buffer (get-buffer-create "*package-build-checkout*")
    (let ((repo (pb/trim (plist-get config :url) ?/))
          (bound (goto-char (point-max))))
      (cond
       ((and (file-exists-p (expand-file-name ".svn" dir))
             (string-equal (pb/svn-repo dir) repo))
        (pb/princ-exists dir)
        (pb/run-process dir "svn" "up"))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t nil))
        (pb/princ-checkout repo dir)
        (pb/run-process nil "svn" "checkout" repo dir)))
      (apply 'pb/run-process dir "svn" "info"
             (pb/expand-source-file-list dir config))
      (or (pb/find-parse-time-latest "Last Changed Date: \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)" bound)
          (error "No valid timestamps found!")))))

(defun pb/cvs-repo (dir)
  "Get the current CVS root and repository for DIR.

Return a cons cell whose `car' is the root and whose `cdr' is the repository."
  (apply 'cons
         (mapcar (lambda (file)
                   (pb/string-rtrim (pb/slurp-file (expand-file-name file dir))))
                 '("CVS/Root" "CVS/Repository"))))

(defun pb/checkout-cvs (name config dir)
  "Check package NAME with config CONFIG out of csv into DIR."
  (with-current-buffer (get-buffer-create "*package-build-checkout*")
    (let ((root (pb/trim (plist-get config :url) ?/))
          (repo (or (plist-get config :module) (symbol-name name)))
          (bound (goto-char (point-max))))
      (cond
       ((and (file-exists-p (expand-file-name "CVS" dir))
             (equal (pb/cvs-repo dir) (cons root repo)))
        (pb/princ-exists dir)
        (pb/run-process dir "cvs" "update" "-dP"))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t nil))
        (pb/princ-checkout (format "%s from %s" repo root) dir)
        ;; CVS insists on relative paths as target directory for checkout (for
        ;; whatever reason), and puts "CVS" directories into every subdirectory
        ;; of the current working directory given in the target path. To get CVS
        ;; to just write to DIR, we need to execute CVS from the parent
        ;; directory of DIR, and specific DIR as relative path.  Hence all the
        ;; following mucking around with paths.  CVS is really horrid.
        (let* ((dir (directory-file-name dir))
               (working-dir (file-name-directory dir))
               (target-dir (file-name-nondirectory dir)))
          (pb/run-process working-dir "env" "TZ=UTC" "cvs" "-z3" "-d" root "checkout"
                          "-d" target-dir repo))))
      (apply 'pb/run-process dir "cvs" "log"
             (pb/expand-source-file-list dir config))
      (or (pb/find-parse-time-latest "date: \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\} \\+[0-9]\\{2\\}[0-9]\\{2\\}\\)")
          (pb/find-parse-time-latest "date: \\([0-9]\\{4\\}/[0-9]\\{2\\}/[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\);")
          (error "No valid timestamps found!"))
      )))

(defun pb/git-repo (dir)
  "Get the current git repo for DIR."
  (pb/run-process-match
   "Fetch URL: \\(.*\\)" dir "git" "remote" "show" "-n" "origin"))

(defun pb/git-head-branch (dir)
  "Get the current git repo for DIR."
  (or (ignore-errors
        (pb/run-process-match
         "HEAD branch: \\(.*\\)" dir "git" "remote" "show" "origin"))
      "master"))

(defun pb/checkout-git (name config dir)
  "Check package NAME with config CONFIG out of git into DIR."
  (let ((repo (plist-get config :url))
        (commit (plist-get config :commit)))
    (with-current-buffer (get-buffer-create "*package-build-checkout*")
      (goto-char (point-max))
      (cond
       ((and (file-exists-p (expand-file-name ".git" dir))
             (string-equal (pb/git-repo dir) repo))
        (pb/princ-exists dir)
        (pb/run-process dir "git" "remote" "update"))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t nil))
        (pb/princ-checkout repo dir)
        (pb/run-process nil "git" "clone" repo dir)))
      (pb/run-process dir "git" "reset" "--hard"
                      (or commit (concat "origin/" (pb/git-head-branch dir))))
      (apply 'pb/run-process dir "git" "log" "-n1" "--pretty=format:'\%ci'"
             (pb/expand-source-file-list dir config))
      (pb/find-parse-time
       "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)"))))

(defun pb/checkout-github (name config dir)
  "Check package NAME with config CONFIG out of github into DIR."
  (let* ((url (format "git://github.com/%s.git" (plist-get config :repo))))
    (pb/checkout-git name (plist-put (copy-sequence config) :url url) dir)))

(defun pb/bzr-expand-repo (repo)
  "Get REPO expanded name."
  (pb/run-process-match "\\(?:branch root\\|repository branch\\): \\(.*\\)" nil "bzr" "info" repo))

(defun pb/bzr-repo (dir)
  "Get the current bzr repo for DIR."
  (pb/run-process-match "parent branch: \\(.*\\)" dir "bzr" "info"))

(defun pb/checkout-bzr (name config dir)
  "Check package NAME with config CONFIG out of bzr into DIR."
  (let ((repo (pb/bzr-expand-repo (plist-get config :url))))
    (with-current-buffer (get-buffer-create "*package-build-checkout*")
      (goto-char (point-max))
      (cond
       ((and (file-exists-p (expand-file-name ".bzr" dir))
             (string-equal (pb/bzr-repo dir) repo))
        (pb/princ-exists dir)
        (pb/run-process dir "bzr" "merge"))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t nil))
        (pb/princ-checkout repo dir)
        (pb/run-process nil "bzr" "branch" repo dir)))
      (apply 'pb/run-process dir "bzr" "log" "-l1"
             (pb/expand-source-file-list dir config))
      (pb/find-parse-time
       "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)"))))

(defun pb/hg-repo (dir)
  "Get the current hg repo for DIR."
  (pb/run-process-match "default = \\(.*\\)" dir "hg" "paths"))

(defun pb/checkout-hg (name config dir)
  "Check package NAME with config CONFIG out of hg into DIR."
  (let ((repo (plist-get config :url)))
    (with-current-buffer (get-buffer-create "*package-build-checkout*")
      (goto-char (point-max))
      (cond
       ((and (file-exists-p (expand-file-name ".hg" dir))
             (string-equal (pb/hg-repo dir) repo))
        (pb/princ-exists dir)
        (pb/run-process dir "hg" "pull")
        (pb/run-process dir "hg" "update"))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t nil))
        (pb/princ-checkout repo dir)
        (pb/run-process nil "hg" "clone" repo dir)))
      (apply 'pb/run-process dir "hg" "log" "--style" "compact" "-l1"
             (pb/expand-source-file-list dir config))
      (pb/find-parse-time
       "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}\\)"))))

(defun pb/dump (data file)
  "Write DATA to FILE as a pretty-printed Lisp sexp."
  (write-region (concat (pp-to-string data) "\n") nil file))

(defun pb/write-pkg-file (pkg-file pkg-info)
  "Write PKG-FILE containing PKG-INFO."
  (pb/dump
   `(define-package
      ,(aref pkg-info 0)
      ,(aref pkg-info 3)
      ,(aref pkg-info 2)
      ',(mapcar
         (lambda (elt)
           (list (car elt)
                 (package-version-join (cadr elt))))
         (aref pkg-info 1)))
   pkg-file))

(defun pb/read-from-file (file-name)
  "Read and return the Lisp data stored in FILE-NAME, or nil if no such file exists."
  (when (file-exists-p file-name)
    (car (read-from-string (pb/slurp-file file-name)))))

(defun pb/create-tar (file dir &optional files)
  "Create a tar FILE containing the contents of DIR, or just FILES if non-nil.
The file is written to `package-build-working-dir'."
  (let* ((default-directory package-build-working-dir))
    (apply 'process-file
           "tar" nil
           (get-buffer-create "*package-build-checkout*")
           nil "-cvf"
           file
           "--exclude=.svn"
           "--exclude=CVS"
           "--exclude=.git*"
           "--exclude=_darcs"
           "--exclude=.bzr"
           "--exclude=.hg"
           (or (mapcar (lambda (fn) (concat dir "/" fn)) files)
               (list dir)))))


(defun pb/find-package-commentary (file-path)
  "Get commentary section from FILE-PATH."
  (when (file-exists-p file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (lm-commentary))))

(defun pb/write-pkg-readme (commentary file-name)
  "Write COMMENTARY to the FILE-NAME-readme.txt file."
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
                      (pb/readme-file-name file-name))))))

(defun pb/readme-file-name (file-name)
  "Name of the readme file for the package FILE-NAME."
  (expand-file-name (concat file-name "-readme.txt")
                    package-build-archive-dir))

(defun pb/update-or-insert-version (version)
  "Ensure current buffer has a \"Version: VERSION\" header."
  (goto-char (point-min))
  (if (re-search-forward "^;;;* *Version: *" nil t)
      (progn
        (move-beginning-of-line nil)
        (search-forward "V" nil t)
        (backward-char)
        (insert "X-Original-")
        (move-beginning-of-line nil))
    (forward-line))
  (insert (format ";; Version: %s" version))
  (newline))

(defun pb/ensure-ends-here-line (file-path)
  "Add a 'FILE-PATH ends here' trailing line if missing."
  (save-excursion
    (goto-char (point-min))
    (let* ((fname (file-name-nondirectory file-path))
           (trailer (concat ";;; " fname " ends here")))
      (unless (search-forward trailer nil t)
        (goto-char (point-max))
        (newline)
        (insert trailer)
        (newline)))))

(defun pb/get-package-info (file-path)
  "Get a vector of package info from the docstrings in FILE-PATH."
  (when (file-exists-p file-path)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents file-path)
        ;; next few lines are a hack for some packages that aren't
        ;; commented properly.
        (pb/update-or-insert-version "0")
        (pb/ensure-ends-here-line file-path)
        (cl-flet ((package-strip-rcs-id (str) "0"))
          (package-buffer-info))))))

(defun pb/get-pkg-file-info (file-path)
  "Get a vector of package info from \"-pkg.el\" file FILE-PATH."
  (when (file-exists-p file-path)
    (let ((package-def (pb/read-from-file file-path)))
      (if (eq 'define-package (car package-def))
          (let ((pkgfile-info (cdr package-def)))
            (vector
             (nth 0 pkgfile-info)
             (mapcar
              (lambda (elt)
                (list (car elt) (version-to-list (cadr elt))))
              (eval (nth 3 pkgfile-info)))
             (nth 2 pkgfile-info)
             (nth 1 pkgfile-info)))
        (error "No define-package found in %s" file-path)))))

(defun pb/merge-package-info (pkg-info name version config)
  "Return a version of PKG-INFO updated with NAME, VERSION and info from CONFIG.
If PKG-INFO is nil, an empty one is created."
  (let* ((merged (or (copy-sequence pkg-info)
                     (vector name nil "No description available." version))))
    (aset merged 0 name)
    (aset merged 2 (format "%s [source: %s]"
                           (aref merged 2) (plist-get config :fetcher)))
    (aset merged 3 version)
    merged))

(defun pb/dump-archive-contents ()
  "Dump the list of built packages back to the archive-contents file."
  (pb/dump (cons 1 (package-build-archive-alist))
           (expand-file-name "archive-contents"
                             package-build-archive-dir)))

(defun pb/add-to-archive-contents (pkg-info type)
  "Add the built archive with info PKG-INFO and TYPE to `package-build-archive-alist'."
  (let* ((name (intern (aref pkg-info 0)))
         (requires (aref pkg-info 1))
         (desc (or (aref pkg-info 2) "No description available."))
         (version (aref pkg-info 3))
         (existing (assq name (package-build-archive-alist))))

    (when existing (package-build-archive-alist-remove existing))
    (package-build-archive-alist-add
     (cons name
           (vector (version-to-list version)
                   requires
                   desc
                   type)))))

(defun pb/archive-file-name (archive-entry)
  "Return the path of the file in which the package for ARCHIVE-ENTRY is stored."
  (let* ((name (car archive-entry))
         (pkg-info (cdr archive-entry))
         (version (package-version-join (aref pkg-info 0)))
         (flavour (aref pkg-info 3)))
    (expand-file-name
     (format "%s-%s.%s" name version (if (eq flavour 'single) "el" "tar"))
     package-build-archive-dir)))

(defun pb/remove-archive (archive-entry)
  "Remove ARCHIVE-ENTRY from archive-contents, and delete associated file.
Note that the working directory (if present) is not deleted by
this function, since the archive list may contain another version
of the same-named package which is to be kept."
  (message "Removing archive: %s" archive-entry)
  (let ((archive-file (pb/archive-file-name archive-entry))
        (readme-file (pb/readme-file-name (symbol-name (car archive-entry)))))
    (when (file-exists-p archive-file)
      (delete-file archive-file))
    (when (file-exists-p readme-file)
      (delete-file readme-file)))
  (package-build-archive-alist-remove archive-entry))


(defun pb/read-recipe (file-name)
  (let ((pkg-info (pb/read-from-file file-name)))
    (if (string= (symbol-name (car pkg-info))
                 (file-name-nondirectory file-name))
        pkg-info
      (error "Recipe '%s' contains mismatched package name '%s'"
             (file-name-nondirectory file-name)
             (car pkg-info)))))

(defun pb/read-recipes ()
  "Return a list of data structures for all recipes in `package-build-recipes-dir'."
  (cl-loop for file-name in (directory-files  package-build-recipes-dir t "^[^.]")
           collect (pb/read-recipe file-name)))

(defun pb/read-recipes-ignore-errors ()
  "Return a list of data structures for all recipes in `package-build-recipes-dir'."
  (cl-loop for file-name in (directory-files  package-build-recipes-dir t "^[^.]")
           for pkg-info = (condition-case err (pb/read-recipe file-name)
                            (error (message (error-message-string err))
                                   nil))
           when pkg-info
           collect pkg-info))


(defun pb/expand-file-specs (dir specs &optional subdir)
  "In DIR, expand SPECS, optionally under SUBDIR.
The result is a list of (SOURCE . DEST), where SOURCE is a source
file path and DEST is the relative path to which it should be copied."
  (let ((default-directory dir)
        (prefix (if subdir
                    (format "%s/" subdir)
                  "")))
    (cl-mapcan
     (lambda (entry)
       (if (consp entry)
           (pb/expand-file-specs dir
                                 (cdr entry)
                                 (concat prefix (car entry)))
         (mapcar (lambda (f)
                   (cons f (concat prefix (file-name-nondirectory f))))
                 (file-expand-wildcards entry))))
     specs)))

(defun pb/expand-config-file-list (dir config)
  "In DIR, expand the :files for CONFIG using 'pb/expand-file-specs."
  (let* ((patterns (or (plist-get config :files) pb/default-files-spec))
         (files (pb/expand-file-specs dir patterns)))
    (or files
        (error "No matching file(s) found in %s: %s" dir patterns))))

(defun pb/expand-source-file-list (dir config)
  "Shorthand way to expand paths in DIR for source files listed in CONFIG."
  (mapcar 'car (pb/expand-config-file-list dir config)))

(defun pb/copy-package-files (files source-dir target-dir)
  "Copy FILES from SOURCE-DIR to TARGET-DIR.
FILES is a list of (SOURCE . DEST) relative filepath pairs."
  (cl-loop for (source-file . dest-file) in files
           do (pb/copy-file
               (expand-file-name source-file source-dir)
               (expand-file-name dest-file target-dir))))

(defun pb/copy-file (file newname)
  "Copy FILE to NEWNAME and create parent directories for NEWNAME if they don't exist."
  (let ((newdir (file-name-directory newname)))
    (unless (file-exists-p newdir)
      (make-directory newdir t)))
  (cond
   ((file-regular-p file)
    (message "%s -> %s" file newname)
    (copy-file file newname))
   ((file-directory-p file)
    (message "%s => %s" file newname)
    (copy-directory file newname))))


(defun pb/package-name-completing-read ()
  "Prompt for a package name, returning a symbol."
  (intern (completing-read "Package: " (package-build-recipe-alist))))

(defun pb/find-source-file (target files)
  "Search for source of TARGET in FILES."
  (let* ((entry (rassoc target files)))
    (when entry (car entry))))

(defun pb/find-package-file (name)
  "Return the filename of the most recently built package of NAME."
  (pb/archive-file-name (assoc name (package-build-archive-alist))))


;;; Public interface
;;;###autoload
(defun package-build-archive (name)
  "Build a package archive for package NAME."
  (interactive (list (pb/package-name-completing-read)))
  (let* ((file-name (symbol-name name))
         (cfg (or (cdr (assoc name (package-build-recipe-alist)))
                  (error "Cannot find package %s" file-name)))
         (pkg-cwd
          (file-name-as-directory
           (expand-file-name file-name package-build-working-dir))))


    (message "\n;;; %s\n" file-name)
    (let* ((version (pb/checkout name cfg pkg-cwd))
           (files (pb/expand-config-file-list pkg-cwd cfg))
           (default-directory package-build-working-dir)
           (start-time (current-time))
           (old-archive-entry (assq name (package-build-archive-alist))))

      ;; right before we create a new package, clean out the old one
      (when old-archive-entry (pb/remove-archive old-archive-entry))

      (cond
       ((not version)
        (message "Unable to check out repository for %s" name))
       ((= 1 (length files))
        (let* ((pkg-source (expand-file-name (caar files) pkg-cwd))
               (pkg-target (expand-file-name
                            (concat file-name "-" version ".el")
                            package-build-archive-dir))
               (pkg-info (pb/merge-package-info
                          (pb/get-package-info pkg-source)
                          file-name
                          version
                          cfg)))
          (when (file-exists-p pkg-target)
            (delete-file pkg-target t))
          (copy-file pkg-source pkg-target)
          (let ((enable-local-variables :safe)
                (make-backup-files nil))
            (with-current-buffer (find-file pkg-target)
              (pb/update-or-insert-version version)
              (pb/ensure-ends-here-line pkg-source)
              (write-file pkg-target nil)
              (condition-case err
                  (package-buffer-info)
                (error
                 (message "Warning: %S" err)))
              (kill-buffer)))

          (pb/write-pkg-readme (and (> (length pkg-info) 4) (aref pkg-info 4))
                               file-name)

          (pb/add-to-archive-contents pkg-info 'single)))
       ((< 1 (length  files))
        (let* ((pkg-dir (concat file-name "-" version))
               (pkg-file (concat file-name "-pkg.el"))
               (pkg-file-source (or (pb/find-source-file pkg-file files)
                                    pkg-file))
               (file-source (concat file-name ".el"))
               (pkg-source (or (pb/find-source-file file-source files)
                               file-source))
               (pkg-info
                (pb/merge-package-info
                 (let ((default-directory pkg-cwd))
                   (or (pb/get-pkg-file-info pkg-file-source)
                       ;; some packages (like magit) provide name-pkg.el.in
                       (pb/get-pkg-file-info
                        (expand-file-name (concat pkg-file ".in")
                                          (file-name-directory pkg-source)))
                       (pb/get-package-info pkg-source)))
                 file-name
                 version
                 cfg)))

          (let ((default-directory pkg-cwd))
            (pb/write-pkg-readme (pb/find-package-commentary pkg-source)
                                 file-name))

          (when (file-exists-p pkg-dir)
            (delete-directory pkg-dir t nil))

          (pb/copy-package-files files pkg-cwd pkg-dir)

          (pb/write-pkg-file (expand-file-name pkg-file
                                               (file-name-as-directory
                                                (expand-file-name
                                                 pkg-dir
                                                 package-build-working-dir)))
                             pkg-info)

          (pb/create-tar
           (expand-file-name
            (concat file-name "-" version ".tar") package-build-archive-dir)
           pkg-dir
           (delete-dups (append (mapcar 'cdr files) (list pkg-file))))

          (delete-directory pkg-dir t nil)
          (pb/add-to-archive-contents pkg-info 'tar)))

       (t (error "Unable to find files matching recipe patterns")))
      (pb/dump-archive-contents)
      (message "Built in %.3fs, finished at %s"
               (time-to-seconds (time-since start-time))
               (current-time-string))
      file-name)))


;;; Helpers for recipe authors

(defvar package-build-minor-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") 'package-build-current-recipe)
    m)
  "Keymap for `package-build-minor-mode'.")

(define-minor-mode package-build-minor-mode
  "Helpful functionality for building packages."
  nil
  " PBuild"
  package-build-minor-mode-map)

;;;###autoload
(defun package-build-create-recipe (name fetcher)
  "Create a new recipe for package NAME using FETCHER."
  (interactive
   (list (intern (read-string "Package name: "))
         (intern
          (let ((fetcher-types (mapcar #'symbol-name '(github git wiki bzr hg cvs svn))))
            (completing-read
             "Fetcher: "
             fetcher-types
             nil t nil nil (car fetcher-types))))))
  (let ((recipe-file (expand-file-name (symbol-name name) package-build-recipes-dir)))
    (when (file-exists-p recipe-file)
      (error "Recipe already exists"))
    (find-file recipe-file)
    (let* ((extra-params
            (cond
             ((eq 'github fetcher) '(:repo "USER/REPO"))
             ((eq 'wiki fetcher) '())
             (t '(:url "SCM_URL_HERE"))))
           (template `(,name :fetcher ,fetcher ,@extra-params)))
      (insert (pp-to-string template))
      (emacs-lisp-mode)
      (package-build-minor-mode)
      (beginning-of-buffer))))

;;;###autoload
(defun package-build-current-recipe ()
  "Build archive for the recipe defined in the current buffer."
  (interactive)
  (unless (and (buffer-file-name)
               (string-equal (file-name-directory (buffer-file-name))
                             package-build-recipes-dir))
    (error "Buffer is not visiting a recipe"))
  (when (buffer-modified-p)
    (if (y-or-n-p (format "Save file %s? " buffer-file-name))
        (save-buffer)
      (error "Aborting")))
  (package-build-reinitialize)
  (let ((pkg-name (intern (file-name-nondirectory (buffer-file-name)))))
    (package-build-archive pkg-name)
    (save-current-buffer
      (find-file-other-window
       (expand-file-name "archive-contents" package-build-archive-dir))
      (revert-buffer t t))
    (when (yes-or-no-p "Install new package? ")
      (package-install-file (pb/find-package-file pkg-name)))))

(defun package-build-archive-ignore-errors (pkg)
  "Build archive for package PKG, ignoring any errors."
  (interactive (list (pb/package-name-completing-read)))
  (let* ((debug-on-error t)
         (debug-on-signal t)
         (pb/debugger-return nil)
         (debugger (lambda (&rest args)
                     (setq pb/debugger-return (with-output-to-string
                                                (backtrace))))))
    (condition-case err
        (package-build-archive pkg)
      (error
       (message "%s" (error-message-string err))
       nil))))



;;;###autoload
(defun package-build-all ()
  "Build all packages in the `package-build-recipe-alist'."
  (interactive)
  (let ((failed (cl-loop for pkg in (mapcar 'car (package-build-recipe-alist))
                         when (not (package-build-archive-ignore-errors pkg))
                         collect pkg)))
    (if (not failed)
        (princ "\nSuccessfully Compiled All Packages\n")
      (princ "\nFailed to Build the Following Packages\n")
      (princ (mapconcat 'symbol-name failed "\n"))
      (princ "\n")))
  (package-build-cleanup))

(defun package-build-cleanup ()
  "Remove previously-built packages that no longer have recipes."
  (interactive)
  (let* ((known-package-names (mapcar 'car (package-build-recipe-alist)))
         (stale-archives (cl-loop for built in (package-build-archive-alist)
                                  when (not (memq (car built) known-package-names))
                                  collect built)))
    (mapc 'pb/remove-archive stale-archives)
    (pb/dump-archive-contents)))

(defun package-build-recipe-alist ()
  "Retun the list of avalailable packages."
  (unless pb/recipe-alist-initialized
    (setq pb/recipe-alist (pb/read-recipes-ignore-errors)
          pb/recipe-alist-initialized t))
  pb/recipe-alist)

(defun package-build-archive-alist-remove (elt)
  "Remove ELT from the archive list using `remove' and return the new value."
  (setq pb/archive-alist (remove elt pb/archive-alist)))

(defun package-build-archive-alist-add (elt)
  "Add ELT to the archive list if it isn't there yet and return the new value."
  (add-to-list 'pb/archive-alist elt))

(defun package-build-archive-alist ()
  "Return the archive list."
  (unless pb/archive-alist-initialized
    (setq pb/archive-alist
          (cdr (pb/read-from-file
                (expand-file-name "archive-contents"
                                  package-build-archive-dir)))
          pb/archive-alist-initialized t))
  pb/archive-alist)

(defun package-build-reinitialize ()
  (interactive)
  (setq pb/recipe-alist-initialized nil
        pb/archive-alist-initialized nil))


;; Utility functions
(require 'json)
(load (expand-file-name "json-fix" pb/this-dir))

(defun package-build-recipe-alist-as-json (fn)
  (interactive)
  (with-temp-file fn
    (insert (json-encode (package-build-recipe-alist)))))

(defun package-build-archive-alist-as-json (fn)
  (interactive)
  (with-temp-file fn
    (insert (json-encode (package-build-archive-alist)))))


(provide 'package-build)

;; Local Variables:
;; coding: utf-8
;; eval: (checkdoc-minor-mode 1)
;; End:

;;; package-build.el ends here
