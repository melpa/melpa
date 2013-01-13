(Given "^initialization$"
       (lambda ()
         (package-build-initialize)))

(Given "^read recipes$"
       (lambda ()
         (pb/read-recipes)))

(Given "^capture messages to \\(.+\\)$"
       (lambda (varby)
         (message "****** %s" varby)
         (collect-messages-to varby)))

(Given "^package initialization$"
       (lambda ()
         (package-initialize)))

(Then "^\\(.+\\) should be empty"
      (lambda (var)
        (message "//////// %s" (eval (intern var)))
        (let ((varsym (intern var)))
          (assert (not (eval varsym)) nil "Variable %s is not empty." var))))
