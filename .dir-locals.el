(("recipes"
  (nil (eval . (and buffer-file-name
		    (not (eq major-mode 'package-recipe-mode))
		    (or (require 'package-recipe-mode nil t)
			(let ((load-path (cons "../package-build" load-path)))
			  (require 'package-recipe-mode nil t)))
		    (package-recipe-mode))))))
