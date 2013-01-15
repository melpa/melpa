#!/bin/sh 
:;exec emacs --script "$0" "$@"

(defun difference (left right)
  "compare two lists"
  (let ((caleft (car left))
        (caright (car right)))
    (cond
     ((not left) right)
     ((not right) left)
     ((string< caleft caright)
      (cons caleft (difference (cdr left) right)))
     ((string< caright caleft)
      (cons caright (difference left (cdr right))))
     (t (difference (cdr left) (cdr right))))))

(defun stripstuff (fn)
  "strip the date and extension"
  (string-match "\\\(.*\\\)-[0-9]+\.\\\(el$\\\|tar$\\\)" fn)
  (match-string 1 fn))

(mapc 'message
      (difference
       (sort (directory-files "recipes/" nil "[^.].*") 'string<)
       (sort (mapcar 'stripstuff (directory-files "packages/" nil "[^.].*\\\(el$\\\|tar$\\\)")) 'string<)))



