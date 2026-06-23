((nil
  (indent-tabs-mode . nil))
 (emacs-lisp-mode
  (lisp-indent-local-overrides . ((cond . 0) (interactive . 0)))
  (mode . checkdoc-minor))
 (git-commit-mode
  (git-commit-major-mode . git-commit-elisp-text-mode))
 (makefile-mode
  (indent-tabs-mode . t)
  (mode . outline-minor)
  (outline-regexp . "#\\(#+\\)")))
