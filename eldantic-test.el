;;; eldantic-test.el --- Tests for eldantic.el            -*- lexical-binding: t; -*-

(require 'eldantic)
(require 'ert)

;; Test built-in type checkers
(ert-deftest test-check-string ()
  (should (check-string "valid"))
  (should-not (check-string 42))
  (should-not (check-string nil)))

(ert-deftest test-check-integer ()
  (should (check-integer 42))
  (should-not (check-integer "42"))
  (should-not (check-integer t)))

(ert-deftest test-check-boolean ()
  (should (check-boolean t))
  (should (check-boolean nil))
  (should-not (check-boolean 'other))
  (should-not (check-boolean 0)))

(ert-deftest test-check-list ()
  (should (check-list '(a b c)))
  (should-not (check-list "not a list"))
  (should-not (check-list 42)))

;; Test schema definition and validation
(ert-deftest test-defschema-validation ()
  (defschema user
    ((name string)
     (age integer)
     (active boolean)))
  
  ;; Test valid construction
  (should (new-user '(:name "Alice" :age 30 :active t)))
  
  ;; Test missing required field
  (should-error (new-user '(:age 30 :active t)))
  
  ;; Test invalid types
  (should-error (new-user '(:name "Bob" :age "30" :active nil)))
  (should-error (new-user '(:name "Charlie" :age 25 :active 'maybe)))
  
  ;; Test extra fields (should be allowed)
  (should (new-user '(:name "Dave" :age 40 :active nil :extra "data"))))

(provide 'eldantic-test)
;;; eldantic-test.el ends here