;; This is an example of how you could set up this file. This setup
;; requires a directory called util in the project root and that the
;; util directory contains the testing tools ert and espuds.

(let* ((features-directory
        (file-name-directory
         (directory-file-name (file-name-directory load-file-name))))
       (project-directory
        (file-name-directory
         (directory-file-name features-directory))))
  (setq melpa-root-path project-directory)
  (setq melpa-util-path (expand-file-name "util" melpa-root-path)))

(add-to-list 'load-path (expand-file-name "package-build" melpa-root-path))
(add-to-list 'load-path (expand-file-name "espuds" melpa-util-path))

(require 'package)
(package-initialize)
(require 'package-build)
(require 'ert)
(require 'espuds)


(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 (defmacro collect-messages-to (var)
   (let ((varsym (intern (eval var))))
     (set varsym '())
     `(defadvice message (before message (format-string &rest args) activate)
        (add-to-list ',varsym (ignore-errors (format format-string args))))
     ))
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
