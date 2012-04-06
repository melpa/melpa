;;; package-build.el --- Tools for curating the package archive

;; Copyright (C) 2011 Donald Ephraim Curtis <dcurtis@milkbox.net>
;; Copyright (C) 2009 Phil Hagelberg <technomancy@gmail.com>

;; Author: Donald Ephraim Curtis <dcurtis@milkbox.net>
;; Created: 2011-09-30
;; Version: 0.1
;; Keywords: tools

;;
;; Credits:
;;   Steve Purcell
;;

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
;; projects and repositories from which to get them. The term
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

(defun pb/find-parse-time (regex)
  "Find REGEX in current buffer and format as a proper time version."
  (format-time-string
   "%Y%m%d"
   (date-to-time
    (print (progn (re-search-backward regex)
                  (match-string-no-properties 1))))))

(defun pb/run-process (dir prog &rest args)
  "In DIR (or `default-directory' if unset) run command PROG with ARGS.
Output is written to the current buffer."
  (let ((default-directory (or dir default-directory)))
    (let ((exit-code (apply 'process-file prog nil (current-buffer) t args)))
      (unless (zerop exit-code)
        (error "Program '%s' with args '%s' exited with non-zero status %d"
               prog args exit-code)))))


(defun pb/checkout (name config cwd)
  "Check out source for package NAME with CONFIG under working dir CWD.
In turn, this function uses the :fetcher option in the config to
choose a source-specific fetcher function, which it calls with
the same arguments."
  (let ((repo-type (plist-get config :fetcher)))
    (print repo-type)
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
           (message "Waiting %s secs before hitting Emacswiki again" wait)
           (sleep-for wait)))
       (unwind-protect
           (progn ,@body)
         (setq pb/last-wiki-fetch-time (float-time))))))

(defun pb/grab-wiki-file (filename)
  "Download FILENAME from emacswiki, returning its last-modified time."
  (let* ((download-url (format "http://www.emacswiki.org/emacs/download/%s" filename))
         (wiki-url (format "http://www.emacswiki.org/emacs/%s" filename)))
    (pb/with-wiki-rate-limit
     (url-copy-file download-url filename t))
    (with-current-buffer (pb/with-wiki-rate-limit
                          (url-retrieve-synchronously wiki-url))
      (pb/find-parse-time
       "Last edited \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\} [A-Z]\\{3\\}\\)"))))

(defun pb/checkout-wiki (name config dir)
  "Checkout package NAME with config CONFIG from the EmacsWiki into DIR."
  (with-current-buffer (get-buffer-create "*package-build-checkout*")
    (message dir)
    (unless (file-exists-p dir)
      (make-directory dir))
    (let ((files (or (plist-get config :files)
                     (list (format "%s.el" name))))
          (default-directory dir))
      (car (nreverse (sort (mapcar 'pb/grab-wiki-file files) 'string-lessp))))))

(defun pb/darcs-repo (dir)
  "Get the current darcs repo for DIR."
  (with-temp-buffer
    (pb/run-process dir "darcs" "show" "repo")
    (goto-char (point-min))
    (re-search-forward "Default Remote: \\(.*\\)")
    (match-string-no-properties 1)))

(defun pb/checkout-darcs (name config dir)
  "Check package NAME with config CONFIG out of darcs into DIR."
  (let ((repo (plist-get config :url)))
    (with-current-buffer (get-buffer-create "*package-build-checkout*")
      (cond
       ((and (file-exists-p (expand-file-name "_darcs" dir))
             (string-equal (pb/darcs-repo dir) repo))
        (print "checkout directory exists, updating...")
        (pb/run-process dir "darcs" "pull"))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t nil))
        (print "cloning repository")
        (pb/run-process nil "darcs" "get" repo dir)))
      (pb/run-process dir "darcs" "changes" "--last" "1")
      (pb/find-parse-time
       "\\([a-zA-Z]\\{3\\} [a-zA-Z]\\{3\\} \\( \\|[0-9]\\)[0-9] [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\} [A-Za-z]\\{3\\} [0-9]\\{4\\}\\)"))))

(defun pb/svn-repo (dir)
    "Get the current svn repo for DIR."
  (with-temp-buffer
    (pb/run-process dir "svn" "info")
    (goto-char (point-min))
    (re-search-forward "URL: \\(.*\\)")
    (match-string-no-properties 1)))

(defun pb/checkout-svn (name config dir)
  "Check package NAME with config CONFIG out of svn into DIR."
  (let ((repo (plist-get config :url)))
    (with-current-buffer (get-buffer-create "*package-build-checkout*")
      (goto-char (point-max))
      (cond
       ((and (file-exists-p (expand-file-name ".svn" dir))
             (string-equal (pb/svn-repo dir) repo))
        (print "checkout directory exists, updating...")
        (pb/run-process dir "svn" "up"))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t nil))
        (print "cloning repository")
        (pb/run-process nil "svn" "checkout" repo dir)))
      (pb/run-process dir "svn" "info")
      (pb/find-parse-time
       "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)"))))

(defun pb/git-repo (dir)
  "Get the current git repo for DIR."
  (with-temp-buffer
    (pb/run-process dir "git" "remote" "show" "origin")
    (goto-char (point-min))
    (re-search-forward "Fetch URL: \\(.*\\)")
    (match-string-no-properties 1)))

(defun pb/checkout-git (name config dir)
  "Check package NAME with config CONFIG out of git into DIR."
  (let ((repo (plist-get config :url))
        (commit (plist-get config :commit)))
    (with-current-buffer (get-buffer-create "*package-build-checkout*")
      (goto-char (point-max))
      (cond
       ((and (file-exists-p (expand-file-name ".git" dir))
             (string-equal (pb/git-repo dir) repo))
        (print "checkout directory exists, updating...")
        (pb/run-process dir "git" "pull"))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t nil))
        (print (format "cloning %s to %s" repo dir))
        (pb/run-process nil "git" "clone" repo dir)))
      (when commit
        (pb/run-process dir "git" "checkout" commit))
      (pb/run-process dir "git" "show" "-s" "--format='\%ci'" "HEAD")
      (pb/find-parse-time
       "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)"))))

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
    (with-temp-buffer
      (insert-file-contents-literally file-name)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun pb/get-config (pkg-name)
  "Get the configuration information for the given PKG-NAME."
  (pb/read-from-file (format "epkgs/%s/.config" pkg-name)))

(defun pb/get-master (pkg-name)
  "Get the configuration information for the given PKG-NAME."
  (pb/read-from-file (format "epkgs/%s/master" pkg-name)))


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
           "--exclude=.git*"
           "--exclude=_darcs"
           (or (mapcar (lambda (fn) (concat dir "/" fn)) files)
               (list dir)))))

(defun pb/get-package-info (file-path)
  "Get a vector of package info from the docstrings in FILE-PATH."
  (when (file-exists-p file-path)
    (ignore-errors
      (save-window-excursion
        (find-file file-path)
        ;; next two lines are a hack for some packages that aren't
        ;; commented properly.
        (goto-char (point-max))
        (insert (concat "\n;;; "
                        (file-name-nondirectory file-path) " ends here"))
        (flet ((package-strip-rcs-id (str) "0"))
          (package-buffer-info))))))

(defun pb/get-pkg-file-info (file-path)
  "Get a vector of package info from \"-pkg.el\" file FILE-PATH."
  (when (file-exists-p file-path)
    (let ((pkgfile-info (cdr (pb/read-from-file file-path))))
      (vector
       (nth 0 pkgfile-info)
       (mapcar
        (lambda (elt)
          (list (car elt) (version-to-list (cadr elt))))
        (eval (nth 3 pkgfile-info)))
       (nth 2 pkgfile-info)
       (nth 1 pkgfile-info)))))


(defun pb/expand-file-list (dir files)
  "In DIR, expand FILES, some of which may be shell-style wildcards."
  (let ((default-directory dir))
    (mapcan 'file-expand-wildcards files)))

(defun pb/merge-package-info (pkg-info name version)
  "Return a version of PKG-INFO updated with NAME and VERSION.
If PKG-INFO is nil, an empty one is created."
  (let ((merged (or (copy-seq pkg-info)
                    (vector name nil "No description available." version))))
    (aset merged 3 version)
    (aset merged 0 (downcase name))
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

(defun pb/read-recipes ()
  "Return a list of data structures for all recipes in `package-build-recipes-dir'."
  (mapcar 'pb/read-from-file
          (directory-files package-build-recipes-dir t "^[^.]")))

;;; Public interface

(defun package-build-archive (file-name)
  "Build a package archive for package FILE-NAME."
  (interactive (list (completing-read "Package: "
                                      (mapc 'car package-build-alist))))

  (let* ((name (intern file-name))
         (cfg (or (cdr (assoc name package-build-alist))
                  (error "Cannot find package %s" file-name)))
         (pkg-cwd
          (file-name-as-directory
           (expand-file-name file-name package-build-working-dir))))

    (let* ((version (pb/checkout name cfg pkg-cwd))
           (files (pb/expand-file-list pkg-cwd
                                       (or (plist-get cfg :files)
                                           (list "*.el"))))
           (default-directory package-build-working-dir))
      (cond
       ((not version)
        (print (format "Unable to check out repository for %s" name)))
       ((= 1 (length files))
        (let* ((pkgsrc (expand-file-name (car files) pkg-cwd))
               (pkgdst (expand-file-name
                        (concat file-name "-" version ".el")
                        package-build-archive-dir))
               (pkg-info (pb/merge-package-info
                          (pb/get-package-info pkgsrc)
                          file-name
                          version)))
          (print pkg-info)
          (when (file-exists-p pkgdst)
            (delete-file pkgdst t))
          (copy-file pkgsrc pkgdst)
          (pb/add-to-archive-contents pkg-info 'single)))
       (t
        (let* ((pkg-dir (concat file-name "-" version))
               (pkg-file (concat file-name "-pkg.el"))
               (pkg-info
                (pb/merge-package-info
                 (let ((default-directory pkg-cwd))
                   (or (pb/get-pkg-file-info pkg-file)
                       ;; some packages (like magit) provide name-pkg.el.in
                       (pb/get-pkg-file-info (concat pkg-file ".in"))
                       (pb/get-package-info (concat file-name ".el"))))
                 file-name version)))

          (print pkg-info)
          (copy-directory file-name pkg-dir)

          (pb/write-pkg-file (expand-file-name
                              pkg-file
                              (file-name-as-directory
                               (expand-file-name
                                pkg-dir
                                package-build-working-dir)))
                             pkg-info)

          (when files
            (add-to-list 'files pkg-file))

          (pb/create-tar
           (expand-file-name
            (concat file-name "-" version ".tar") package-build-archive-dir)
           pkg-dir
           files)

          (delete-directory pkg-dir t nil)
          (pb/add-to-archive-contents pkg-info 'tar))))
      (pb/dump-archive-contents))))

(defun package-build-archives (&rest pkgs)
  "Build archives for packages PKGS."
  (interactive)
  (mapc 'package-build-archive pkgs))

(defun package-build-archives-ignore-errors (&rest pkgs)
  "Build archives for packages PKGS.  Ignore errors."
  (interactive)
  (mapc (lambda (pkg) (ignore-errors (package-build-archive pkg))) pkgs))

(defun package-build-all ()
  "Build all packages in the `package-build-alist'."
  (interactive)
  (apply 'package-build-archives-ignore-errors
         (mapcar 'symbol-name (mapcar 'car package-build-alist))))

(defun package-build-initialize ()
  "Load the recipe and archive-contents files."
  (interactive)
  (setq
   package-build-alist (pb/read-recipes)
   package-build-archive-alist (cdr (pb/read-from-file
                                     (expand-file-name "archive-contents"
                                                       package-build-archive-dir)))))


(package-build-initialize)

(provide 'package-build)
;;; package-build.el ends here
