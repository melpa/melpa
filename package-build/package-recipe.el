;;; package-recipe.el --- Package recipes as EIEIO objects  -*- lexical-binding: t -*-

;; Copyright (C) 2018  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>

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

;; Package recipes as EIEIO objects.

;;; Code:

(require 'eieio)

(declare-function package-build-recipe-alist "package-build" ())
(defvar package-build-working-dir)

;;; Classes

(defclass package-recipe ()
  ((url-format      :allocation :class       :initform nil)
   (repopage-format :allocation :class       :initform nil)
   (tag-regexp      :allocation :class       :initform nil)
   (stable-p        :allocation :class       :initform nil)
   (name            :initarg :name           :initform nil)
   (url             :initarg :url            :initform nil)
   (repo            :initarg :repo           :initform nil)
   (repopage        :initarg :repopage       :initform nil)
   (files           :initarg :files          :initform nil)
   (branch          :initarg :branch         :initform nil)
   (commit          :initarg :commit         :initform nil)
   (version-regexp  :initarg :version-regexp :initform nil)
   (old-names       :initarg :old-names      :initform nil))
  :abstract t)

(defun package-recipe-lookup (name)
  (let ((plist (cdr (assq (intern name) (package-build-recipe-alist)))))
    (if plist
        (let (key val args (fetcher (plist-get plist :fetcher)))
          (while (and (setq key (pop plist))
                      (setq val (pop plist)))
            (unless (eq key :fetcher)
              (push val args)
              (push key args)))
          (apply (intern (format "package-%s-recipe" fetcher))
                 name :name name args))
      (error "Cannot find valid recipe for package %s" name))))

(defmethod package-recipe--working-tree ((rcp package-recipe))
  (file-name-as-directory
   (expand-file-name (oref rcp name) package-build-working-dir)))

(defmethod package-recipe--upstream-url ((rcp package-recipe))
  (or (oref rcp url)
      (format (oref rcp url-format)
              (oref rcp repo))))

;;;; Git

(defclass package-git-recipe (package-recipe)
  ((tag-regexp      :initform "\
\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \
[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\( [+-][0-9]\\{4\\}\\)?\\)")))

(defclass package-github-recipe (package-git-recipe)
  ((url-format      :initform "https://github.com/%s.git")
   (repopage-format :initform "https://github.com/%s")))

(defclass package-gitlab-recipe (package-git-recipe)
  ((url-format      :initform "https://gitlab.com/%s.git")
   (repopage-format :initform "https://gitlab.com/%s")))

;;;; Mercurial

(defclass package-hg-recipe (package-recipe)
  ((tag-regexp      :initform "\
\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \
[0-9]\\{2\\}:[0-9]\\{2\\}\\( [+-][0-9]\\{4\\}\\)?\\)")))

(defclass package-bitbucket-recipe (package-hg-recipe)
  ((url-format      :initform "https://bitbucket.org/%s")
   (repopage-format :initform "https://bitbucket.org/%s")))

(provide 'package-recipe)
;; Local Variables:
;; coding: utf-8
;; checkdoc-minor-mode: 1
;; indent-tabs-mode: nil
;; End:
;;; package-recipe.el ends here
