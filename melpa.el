;;; melpa.el --- special handling for the MELPA repository
;;
;; Copyright 2012 Donald Ephraim Curtis
;;
;; Author: Donald Ephraim Curtis <dcurtis@milkbox.net>
;; URL: https://github.com/milkypostman/melpa
;; Version: 0.1
;;
;; Installation:
;;
;; (progn
;;   (switch-to-buffer
;;    (url-retrieve-synchronously
;;     "https://raw.github.com/milkypostman/melpa/master/melpa.el"))
;;   (package-install-from-buffer  (package-buffer-info) 'single))
;;
;;
;;
;; Code goes here
;;

;;;###autoload
 (defcustom melpa-package-enable nil
   "Exclusive list of package symbols enabled for MELPA versions.
Empty list enables all packages."
   :group 'melpa
   :type '(repeat symbol))

;;;###autoload
(defcustom melpa-package-exclude nil
   "List of package symbols excluded from the MELPA repo.
Trumps `melpa-package-enable'."
   :group 'melpa
   :type '(repeat symbol))

;;;###autoload
(defadvice package-compute-transaction
  (before
   package-compute-transaction-reverse (package-list requirements)
   activate compile)
  "reverse the requirements"
  (setq requirements (reverse requirements))
  (print requirements))

;;;###autoload
(defadvice package-download-tar
  (after package-download-tar-initialize activate compile)
  "initialize the package after compilation"
  (package-initialize))

;;;###autoload
(defadvice package-download-single
  (after package-download-single-initialize activate compile)
  "initialize the package after compilation"
  (package-initialize))

;;;###autoload
(defadvice package--add-to-archive-contents
  (around melpa--add-to-archive-contents (package archive) activate compile)
  "For the `melpa' archive:

Ignore any packages in `melpa-package-exclude'.

If `melpa-package-enable' is nil, accept any packages from the
`melpa' archive, otherwise only accpet packages given in
`melpa-package-enable'.

Both `melpa-package-enable' and `melpa-package-exclude' are lists
of symbolp."
  ;; (message (symbol-name (car package)))
  (when  (or (not (equal archive "melpa"))
             (and
              (not (memq (car package) melpa-package-exclude))
              (or (not melpa-package-enable)
                  (memq (car package) melpa-package-enable))))
    ad-do-it))



(provide 'melpa)

;;; melpa.el ends here
