;;; package-build.el --- Tools for curating the package archive

;; Copyright (C) 2011 Donald Ephraim Curtis <dcurtis@milkbox.net>
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
;; projects and repositories from which to get them. The term
;; "package" here is used to mean a specific version of a project that
;; is prepared for download and installation.

;; Currently only supports single-file projects stored in git.

;;; Code:

;; Since this library is not meant to be loaded by users
;; at runtime, use of cl functions should not be a problem.
(require 'cl)

(require 'package)

(defvar package-build-working-dir (expand-file-name "working/")
  "Directory in which to keep checkouts.")

(defvar package-build-archive-dir (expand-file-name "packages/")
  "Directory in which to keep compiled archives.")

(defvar package-build-alist-file (expand-file-name "pkglist")
  "File containing pkg alist")

(defun package-build-checkout-darcs (repo dir)
  "checkout an svn package"
  (with-current-buffer (get-buffer-create "*package-build-checkout*")
    (cond
     ((file-exists-p dir)
      (print "checkout directory exists, updating...")
      (let ((default-directory dir))
        (process-file
         "darcs" nil
         (current-buffer) nil "pull")))
     (t
      (print "cloning repository")
      (process-file
       "darcs" nil
       (current-buffer)
       nil "get" repo dir)))
    (let ((default-directory dir))
      (process-file
       "darcs" nil
       (current-buffer)
       t "changes" "--last" "1"))
    (print
     (format-time-string
      "%Y%m%d"
      (date-to-time
       (print (progn
                  (re-search-backward
                   "\\([a-zA-Z]\\{3\\} [a-zA-Z]\\{3\\} \\( \\|[0-9]\\)[0-9] [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\} [A-Za-z]\\{3\\} [0-9]\\{4\\}\\)")
                  (match-string-no-properties 1))))))))

(defun package-build-checkout-svn (repo dir)
  "checkout an svn repo"
  (with-current-buffer (get-buffer-create "*package-build-checkout*")
    (goto-char (point-max))
    (cond
     ((file-exists-p dir)
      (print "checkout directory exists, updating...")
      (let ((default-directory dir))
        (process-file
         "svn" nil
         (current-buffer) nil "up")))
     (t
      (print "cloning repository")
      (process-file
       "svn" nil
       (current-buffer)
       nil "checkout" (concat repo "/trunk") dir)))
    (let ((default-directory dir))
      (process-file
       "svn" nil
       (current-buffer)
       t "info"))
    (print
     (format-time-string
      "%Y%m%d"
      (date-to-time
       (print (progn
                  (re-search-backward
                   "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)")
                  (match-string-no-properties 1))))))))

(defun package-build-checkout-git (repo dir)
  "checkout an git repo"
  (with-current-buffer (get-buffer-create "*package-build-checkout*")
    (goto-char (point-max))
    (cond
     ((file-exists-p dir)
      (print "checkout directory exists, updating...")
      (let ((default-directory dir))
        (process-file
         "git" nil
         (current-buffer) nil "pull"))
      )
     (t
      (print (format "cloning %s to %s" repo dir))
      (process-file
       "git" nil
       (current-buffer)
       nil "clone" repo dir)))
    (let ((default-directory dir))
      (process-file
       "git" nil
       (current-buffer)
       t "show" "-s" "--format='\%ci'" "HEAD"))
    (print
     (format-time-string
      "%Y%m%d"
      (date-to-time
       (print (progn
                  (re-search-backward
                   "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)")
                  (match-string-no-properties 1))))))))

(defun package-change-list-elt (lst idx newval)
  (if (zerop idx)
      (cons newval (cdr lst))
    (cons (car lst) (package-change-list-elt (cdr lst) (1- idx) newval))))

(defun package-build-pkg-file (pkg-file file-name version homepage)
  "build the pkg file"
  (let ((print-level nil)
        (print-length nil)
        (pkglst
         (or (package-read-from-file pkg-file)
             (list 'define-package
                   file-name
                   version
                   homepage
                   (list 'quote (mapcar
                                 (lambda (elt)
                                   (list (car elt)
                                         (package-version-join (cadr elt))))
                                 nil))))))

    ;; set the packages version
    (setq pkglst (package-change-list-elt pkglst 2 version))

    (write-region
     (concat
      (prin1-to-string
       pkglst
       )
      "\n")
     nil
     pkg-file
     nil nil nil nil)
    pkglst))

(defun package-read-from-file (file-name)
  "read one lisp expression from a file"
  (cond
   ((file-exists-p file-name)
    (with-temp-buffer
      (insert-file-contents-literally file-name)
      (goto-char (point-min))
      (car
       (read-from-string
        (buffer-substring-no-properties (point-min) (point-max))))))))

(defun package-build-get-config (file-name)
  "get the configuration information for the given file-name"
  (package-read-from-file (format "epkgs/%s/.config" file-name)))

(defun package-build-get-master (file-name)
  "get the configuration information for the given file-name"
  (package-read-from-file (format "epkgs/%s/master" file-name)))


(defun package-build-create-tar (dir file &optional files)
  "create a tar for the file-name with version"
  (let* ((default-directory package-build-working-dir))
    (if files
        (setq files (mapcar (lambda (fn) (concat dir "/" fn)) files))
      (setq files (list dir)))
    (print files)
    (print default-directory)
    (apply 'process-file
           "tar" nil
           (get-buffer-create "*package-build-checkout*")
           nil "-cvf"
           file
           "--exclude=.svn"
           "--exclude=.git*"
           "--exclude=_darcs"
           files)))

(defun package-build-archives (&rest pkgs)
  "build archives"
  (interactive)
  (mapc 'package-build-archive pkgs)
  (package-build-dump-archive-contents))

(defun package-build-archive (file-name)
  "build a git package archive"
  (interactive)

  (let* ((name (intern file-name))
         (cfg (cdr (assoc name package-build-alist)))
         (pkg-cwd
          (file-name-as-directory
           (expand-file-name file-name package-build-working-dir))))

    (when cfg

      (let* ((repo-type (plist-get cfg :fetcher))
             (repo-url (plist-get cfg :url))
             (summary (plist-get cfg :summary))
             (files (plist-get cfg :files))
             (pkglst)
             (pkgdeps)
             (version
              (cond
               ((eq repo-type 'svn)
                (print 'Subversion)
                (package-build-checkout-svn repo-url pkg-cwd))
               ((eq repo-type 'git)
                (print 'Git)
                (package-build-checkout-git repo-url pkg-cwd))
               ((eq repo-type 'darcs)
                (print 'Darcs)
                (package-build-checkout-darcs repo-url pkg-cwd))))
             (pkg-base-dir (concat file-name "-" version))
             (pkg-file (concat file-name "-pkg.el"))
             (default-directory package-build-working-dir))

        (cond
         ((= 1 (length files))
          (copy-file (expand-file-name (car files) pkg-cwd)
                     (expand-file-name (concat file-name "-" version ".el")
                                       package-build-archive-dir))
          (package-build-add-to-archive-contents
           name version pkgdeps summary 'single))

         (t
          (copy-directory file-name pkg-base-dir)
          (setq pkglst
                (package-build-pkg-file
                 (expand-file-name
                  pkg-file
                  (concat
                   (file-name-as-directory package-build-working-dir)
                   (file-name-as-directory pkg-base-dir)))
                 file-name version summary))
          (setq pkgdeps (mapcar
                         (lambda (elt)
                           (list (car elt) (version-to-list (cadr elt))))
                         (eval (nth 4 pkglst))))
          (when files
            (add-to-list 'files pkg-file))
          (package-build-create-tar
           pkg-base-dir
           (expand-file-name
            (concat file-name "-" version ".tar") package-build-archive-dir)
           files)
          (delete-directory pkg-base-dir t nil)
          (package-build-add-to-archive-contents
           name version pkgdeps summary 'tar))))
      (package-build-dump-archive-contents))))


(defvar package-build-alist
  (let ((pkg-file package-build-alist-file))
    (when (file-exists-p pkg-file)
      (with-temp-buffer
        (insert-file-contents-literally pkg-file)
        (goto-char (point-min))
        (read (current-buffer))))))

(defvar package-build-archive-alist
  (let ((archive-file
         (expand-file-name "archive-contents" package-build-archive-dir)))
    (when (file-exists-p archive-file)
      (with-temp-buffer
        (insert-file-contents-literally archive-file)
        (goto-char (point-min))
        (let ((contents (read (current-buffer))))
          (cdr contents))))))


(defun package-build-dump-archive-contents ()
  "dump the archive contents back to the file"
  (write-region
   (concat
    (pp-to-string
     (cons 1 package-build-archive-alist))
    "\n")
   nil
   (expand-file-name "archive-contents" package-build-archive-dir)
   nil nil nil nil))


(defun package-build-add-to-archive-contents (name version deps homepage type)
  "add an archive to the package-build-archive-contents"
  (let ((existing (assq name package-build-archive-alist)))
    (when existing
      (setq package-build-archive-alist
            (delq existing package-build-archive-alist)))
    (add-to-list 'package-build-archive-alist
                 (cons name
                       (vector
                        (version-to-list version)
                        deps
                        homepage
                        type)))))
