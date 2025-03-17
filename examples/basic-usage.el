;;; basic-usage.el --- Basic usage example for eldantic -*- lexical-binding: t; -*-

(require 'eldantic)

;; 1. Define a user schema
(defschema user
  ((name string)
   (age integer)
   (active boolean)))

;; 2. Validate good data
(let ((valid-user (new-user '(:name "Alice" :age 30 :active t))))
  (message "Valid user created: %S" valid-user))

;; 3. Handle validation errors
(condition-case err
    (new-user '(:name "Bob" :age "thirty" :active nil))
  (error (message "Validation error: %s" (error-message-string err))))

;; 4. Allow extra fields (current behavior)
(let ((user-with-extra (new-user '(:name "Charlie" :age 25 :active t :email "charlie@example.com"))))
  (message "User with extra fields: %S" user-with-extra))

(provide 'basic-usage)
;;; basic-usage.el ends here