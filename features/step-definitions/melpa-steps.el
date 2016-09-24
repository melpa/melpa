(require 'cl)

(Given "^read recipes$"
       (lambda ()
         (package-build--read-recipes)))

(Given "^capture messages to \\(.+\\)$"
       (lambda (varby)
         (message "****** %s" varby)
         (collect-messages-to varby)))

(Given "^set \\(.+\\) to archive alist$"
       (lambda (var)
         (set (intern var) (package-build-archive-alist))))

(Then "^\\(.+\\) should be empty"
      (lambda (var)
        (let ((varsym (intern var)))
          (assert (not (eval varsym)) nil "Variable %s is not empty: %s" var (eval varsym)))))

(Given "add \"\\(.+\\)\" to archive alist"
       (lambda (var)
         (let ((varval (car (read-from-string var))))
           (package-build-archive-alist-add var))))

(Given "remove \"\\(.+\\)\" from archive alist"
       (lambda (var)
         (let ((varval (car (read-from-string var))))
           (package-build-archive-alist-remove var))))

(Then "archive alist should be \\(.+\\)$"
      (lambda (var)
        (let ((varval (car (read-from-string var))))
          (assert (equal varval (package-build-archive-alist))
                  nil "package-build-archive-alist = %s is not equal to %s."
                  (package-build-archive-alist) varval))))

(Then "archive alist should be \\(.+\\)$"
      (lambda (var)
        (let ((varval (car (read-from-string var))))
          (assert (equal varval (package-build-archive-alist))
                  nil "package-build-archive-alist = %s is not equal to %s."
                  (package-build-archive-alist) varval))))
