(require 'ert)

(require 'cl-lib)
(require 'epg)
(require 'mail-utils)
(require 'message)
(require 'mm-archive)
(require 'network-stream)
(require 'outline)
(require 'url-auth)
(require 'url-cache)
(require 'url-handlers)
(require 'url-http)

(require 'package)
(require 'package-build)
(package-initialize)

(ert-deftest persistent-versions-stable ()
  (let ( (package-build-stable t)
         (package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")))
         (package-build--recipe-alist (package-build--read-recipes-ignore-errors)))

    (package-refresh-contents)

    (cl-loop for recipe in (cdr package-archive-contents)
             do
             (setq recipe (car recipe))
             (should (equal
                      (version-to-list (cadr (package-build-archive recipe)))
                      (package-desc-version (cadr (assoc recipe package-archive-contents))))))))

(ert-deftest persistent-versions ()
  (let ( (package-build-stable nil)
         (package-archives '(("melpa" . "https://melpa.org/packages/")))
         (package-build--recipe-alist (package-build--read-recipes-ignore-errors)))

    (package-refresh-contents)

    (cl-loop for recipe in (cdr package-archive-contents)
             do
             (setq recipe (car recipe))
             (should (equal
                      (version-to-list (cadr (package-build-archive recipe)))
                      (package-desc-version (cadr (assoc recipe package-archive-contents))))))))
