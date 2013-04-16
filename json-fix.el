;;; Fixes for json.el such that integer plist / alist keys are rendered as strings, in order to comply with the json spec

(require 'json)
(require 'cl-lib)

(defun json-encode-key-value-pair (pair)
  "Encode a (key . value) PAIR as JSON, ensuring that key is encoded into a string."
  (let ((encoded-key (json-encode (car pair))))
    (format "%s:%s"
            (if (string-match "^\"" encoded-key)
                encoded-key
              (json-encode-string encoded-key))
            (json-encode (cdr pair)))))

(defun json-encode-hash-table (hash-table)
  "Return a JSON representation of HASH-TABLE."
  (json-encode-alist (maphash 'cons hash-table)))

;; List encoding (including alists and plists)

(defun json-encode-alist (alist)
  "Return a JSON representation of ALIST."
  (format "{%s}"
          (json-join (mapcar 'json-encode-key-value-pair
                             alist) ", ")))

(defun json-encode-plist (plist)
  "Return a JSON representation of PLIST."
  (json-encode-alist
   (cl-loop while plist
            collect (cons (car plist) (cadr plist))
            do (setf plist (cddr plist)))))


(provide 'json-fix)
;;; json-fix.el ends here
