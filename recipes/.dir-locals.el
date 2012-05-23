((nil . ((eval . (when (and (buffer-file-name)
                          (file-regular-p (buffer-file-name))
                          (string-match-p "^[^.]" (buffer-file-name)))
                 (emacs-lisp-mode))))))
