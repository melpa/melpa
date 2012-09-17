;;; package-build.el --- Tools for curating the package archive

;; Copyright (C) 2011-2012 Donald Ephraim Curtis <dcurtis@milkbox.net>
;; Copyright (C) 2012 Steve Purcell <steve@sanityinc.com>
;; Copyright (C) 2009 Phil Hagelberg <technomancy@gmail.com>

;; Author: Donald Ephraim Curtis <dcurtis@milkbox.net>
;; Created: 2011-09-30
;; Version: 0.1
;; Keywords: tools

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
(require 'cl)

(require 'package)

(defcustom package-build-working-dir (expand-file-name "working/")
  "Directory in which to keep checkouts."
  :group 'package-build
  :type 'string)

(defcustom package-build-archive-dir (expand-file-name "packages/")
  "Directory in which to keep compiled archives."
  :group 'package-build
  :type 'string)

(defcustom package-build-recipes-dir (expand-file-name "recipes/")
  "Directory containing recipe files."
  :group 'package-build
  :type 'string)

(defvar package-build-alist nil
  "List of package build specs.")

(defvar package-build-archive-alist nil
  "List of already-built packages, in the standard package.el format.")

;;; Internal functions

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
  (let ((time (date-to-time (substring-no-properties str))))
    (concat (format-time-string "%Y%m%d." time)
            (format "%d" (or (parse-integer (format-time-string "%H%M" time)) 0)))))

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
    (message (format "%s " repo-type))
    (unless (eq 'wiki repo-type)
      (message (format "%s\n"
                       (or (plist-get config :repo) (plist-get config :url)))))
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
  (let ((now (gensym))
        (elapsed (gensym)))
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
    (with-current-buffer (pb/with-wiki-rate-limit
                          (url-retrieve-synchronously wiki-url))
      (message (format "%s\n" download-url))
      (pb/find-parse-time
       "Last edited \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\} [A-Z]\\{3\\}\\)"))))

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
  (message (format "updating %s\n" dir)))

(defun pb/princ-checkout (repo dir)
  "Print a message that REPO will be checked out into DIR."
  (message (format "cloning %s to %s\n" repo dir)))

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
          (pb/run-process working-dir "cvs" "-z3" "-d" root "checkout"
                          "-d" target-dir repo))))
      (apply 'pb/run-process dir "cvs" "log"
             (pb/expand-source-file-list dir config))
      (or (pb/find-parse-time-latest "date: \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\} \\+[0-9]\\{2\\}[0-9]\\{2\\}\\)")
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

(defun pb/get-package-info (file-path)
  "Get a vector of package info from the docstrings in FILE-PATH."
  (when (file-exists-p file-path)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents file-path)
        ;; next few lines are a hack for some packages that aren't
        ;; commented properly.
        (goto-char (point-min))
        (forward-line)
        (insert ";;; Version: 0")
        (newline)
        (goto-char (point-max))
        (newline)
        (insert ";;; " (file-name-nondirectory file-path) " ends here")
        (flet ((package-strip-rcs-id (str) "0"))
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
  (let* ((merged (or (copy-seq pkg-info)
                     (vector name nil "No description available." version))))
    (aset merged 0 name)
    (aset merged 2 (format "%s [source: %s]"
                           (aref merged 2) (plist-get config :fetcher)))
    (aset merged 3 version)
    merged))

(defun pb/dump-archive-contents ()
  "Dump the list of built packages back to the archive-contents file."
  (pb/dump (cons 1 package-build-archive-alist)
           (expand-file-name "archive-contents"
                             package-build-archive-dir)))

(defun pb/add-to-archive-contents (pkg-info type)
  "Add the built archive with info PKG-INFO and TYPE to `package-build-archive-alist'."
  (let* ((name (intern (aref pkg-info 0)))
         (requires (aref pkg-info 1))
         (desc (or (aref pkg-info 2) "No description available."))
         (version (aref pkg-info 3))
         (existing (assq name package-build-archive-alist)))
    (when existing
      (setq package-build-archive-alist
            (delq existing package-build-archive-alist)))
    (add-to-list 'package-build-archive-alist
                 (cons name
                       (vector
                        (version-to-list version)
                        requires
                        desc
                        type)))))

(defun pb/archive-file-name (archive-entry)
  "Return the path of the file in which the package for ARCHIVE-ENTRY is stored."
  (expand-file-name (format "%s-%s.%s"
                            (car archive-entry)
                            (car (aref (cdr archive-entry) 0))
                            (if (eq 'single (aref (cdr archive-entry) 3))
                                "el"
                              "tar"))
                    package-build-archive-dir))

(defun pb/remove-archive (archive-entry)
  "Remove ARCHIVE-ENTRY from archive-contents, and delete associated file.
Note that the working directory (if present) is not deleted by
this function, since the archive list may contain another version
of the same-named package which is to be kept."
  (print (format "Removing archive: %s" archive-entry))
  (let ((archive-file (pb/archive-file-name archive-entry)))
    (when (file-exists-p archive-file)
      (delete-file archive-file)))
  (setq package-build-archive-alist
        (remove archive-entry package-build-archive-alist))
  (pb/dump-archive-contents))

(defun pb/read-recipes ()
  "Return a list of data structures for all recipes in `package-build-recipes-dir'."
  (mapcar 'pb/read-from-file
          (directory-files package-build-recipes-dir t "^[^.]")))


(defun pb/expand-file-specs (dir specs &optional subdir)
  "In DIR, expand SPECS, optionally under SUBDIR.
The result is a list of (SOURCE . DEST), where SOURCE is a source
file path and DEST is the relative path to which it should be copied."
  (let ((default-directory dir)
        (prefix (if subdir
                    (format "%s/" subdir)
                  "")))
    (mapcan
     (lambda (entry)
       (if (consp entry)
           (pb/expand-file-specs dir
                                 (cdr entry)
                                 (concat prefix (car entry)))
         (mapcar (lambda (f)
                   (cons f (concat prefix (file-name-nondirectory f))))
                 (or (file-expand-wildcards entry)
                     (error "No matching file(s) found in %s: %s"
                            dir
                            entry)))))
     specs)))

(defun pb/expand-config-file-list (dir config)
  "In DIR, expand the :files for CONFIG using 'pb/expand-file-specs."
  (pb/expand-file-specs dir (or (plist-get config :files) (list "*.el"))))

(defun pb/expand-source-file-list (dir config)
  "Shorthand way to expand paths in DIR for source files listed in CONFIG."
  (mapcar 'car (pb/expand-config-file-list dir config)))

(defun pb/copy-package-files (files source-dir target-dir)
  "Copy FILES from SOURCE-DIR to TARGET-DIR.
FILES is a list of (SOURCE . DEST) relative filepath pairs."
  (loop for (source-file . dest-file) in files
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
  (intern (completing-read "Package: " package-build-alist)))

(defun pb/find-source-file (target files)
  "Search for source of TARGET in FILES."
  (let* ((entry (rassoc target files)))
    (when entry (car entry))))

;;; Public interface
;;;###autoload
(defun package-build-archive (name)
  "Build a package archive for package NAME."
  (interactive (list (pb/package-name-completing-read)))
  (let* ((file-name (symbol-name name))
         (cfg (or (cdr (assoc name package-build-alist))
                  (error "Cannot find package %s" file-name)))
         (pkg-cwd
          (file-name-as-directory
           (expand-file-name file-name package-build-working-dir))))

    (message (format "\n;;; %s\n" file-name))
    (let* ((version (pb/checkout name cfg pkg-cwd))
           (files (pb/expand-config-file-list pkg-cwd cfg))
           (default-directory package-build-working-dir))
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
          (pb/add-to-archive-contents pkg-info 'single)))
       ((< 1 (length  files))
        (let* ((pkg-dir (concat file-name "-" version))
               (pkg-file (concat file-name "-pkg.el"))
               (pkg-file-source (or (pb/find-source-file pkg-file files)
                                    pkg-file))
               (pkg-info
                (pb/merge-package-info
                 (let ((default-directory pkg-cwd))
                   (or (pb/get-pkg-file-info pkg-file-source)
                       ;; some packages (like magit) provide name-pkg.el.in
                       (pb/get-pkg-file-info (concat pkg-file ".in"))
                       (pb/get-package-info (concat file-name ".el"))))
                 file-name
                 version
                 cfg)))

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
      file-name)))

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
  (package-build-initialize)
  (package-build-archive (intern (file-name-nondirectory (buffer-file-name)))))

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
      ('error
       (message "%s" (error-message-string err))
       nil))))

;;;###autoload
(defun package-build-all ()
  "Build all packages in the `package-build-alist'."
  (interactive)
  (let ((failed (loop for pkg in (mapcar 'car package-build-alist)
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
  (let* ((known-package-names (mapcar 'car package-build-alist))
         (stale-archives (loop for built in package-build-archive-alist
                               when (not (memq (car built) known-package-names))
                               collect built)))
    (mapc 'pb/remove-archive stale-archives)))

(defun package-build-initialize ()
  "Load the recipe and archive-contents files."
  (interactive)
  (setq package-build-alist (pb/read-recipes)
        package-build-archive-alist
        (cdr (pb/read-from-file
              (expand-file-name "archive-contents"
                                package-build-archive-dir)))))

(package-build-initialize)

;; Utility functions
(autoload 'json-encode "json")
(eval-after-load 'json '(load (expand-file-name "json-fix")))

(defun package-build-alist-as-json (fn)
  (interactive)
  (with-temp-file fn
    (insert (json-encode package-build-alist))))

(defun package-build-archive-alist-as-json (fn)
  (interactive)
  (with-temp-file fn
    (insert (json-encode package-build-archive-alist))))


(provide 'package-build)

;; Local Variables:
;; coding: utf-8
;; eval: (checkdoc-minor-mode 1)
;; End:

;;; package-build.el ends here
