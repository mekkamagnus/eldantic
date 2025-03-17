;;; eldantic.el --- A pydantic-inspired schema validation library for Emacs Lisp

;;; Code:

;; Built-in type checkers
(defun check-string (v)
  "Return t if V is a string."
  (stringp v))

(defun check-integer (v)
  "Return t if V is an integer."
  (integerp v))

(defun check-boolean (v)
  "Return t if V is a boolean (t or nil)."
  (memq v '(t nil)))

(defun check-list (v)
  "Return t if V is a list."
  (listp v))

;; Schema definition macro
(defmacro defschema (name fields)
  "Define a schema NAME with FIELDS and generate validation functions."
  (let ((validator-name (intern (concat "validate-" (symbol-name name))))
        (constructor-name (intern (concat "new-" (symbol-name name)))))
  `(progn
     (defun ,validator-name (data)
       ,(format "Validate DATA against the %s schema." (symbol-name name))
       (let ((errors '()))
         ,@(mapcar (lambda (field-def)
                     (let* ((field (car field-def))
                       (list 'let (list (list 'key (intern (concat ":" (symbol-name field))))
                             `(progn
                                (unless (plist-member data key)
                                  (push ,(format "Missing required key: :%s" field) errors))
                                (when (plist-member data key)
                                  (let ((value (plist-get data key)))
                                    (unless (,(intern (concat "check-" (symbol-name (cadr field-def)))) value)
                                      (push (format ,(concat "Invalid type for :%s: expected %s, got %%s" 
                                                           (symbol-name field) (symbol-name (cadr field-def)))
                                            (type-of value))
                                      errors)))))))
                   fields)
         (when errors
           (error (mapconcat 'identity (reverse errors) "\n")))))
     (defun ,constructor-name (data)
       ,(format "Create a new %s instance from DATA after validation." (symbol-name name))
       (,validator-name data)
       data)))

(provide 'eldantic)
;;; eldantic.el ends here
