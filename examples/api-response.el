;;; api-response.el --- Eldantic API response validation example -*- lexical-binding: t; -*-

(require 'eldantic)
(require 'json)
(require 'url)

;; Define API response schema
(defschema ApiResponse
  ((id check-string)
   (model check-string)
   (created check-integer)
   (choices (lambda (v) (and (check-list v) (seq-every-p #'Choice-p v))))
   (usage (lambda (v) (and (check-list v)
                          (seq-every-p (lambda (u) (and (plist-member u :prompt_tokens)
                                                       (plist-member u :completion_tokens)
                                                       (plist-member u :total_tokens))) v)))))

(defschema Choice
  ((index check-integer)
   (message (lambda (v) (and (check-list v) (seq-every-p #'Message-p v)))))

(defschema Message
  ((role check-string)
   (content check-string)
   (finish_reason check-string)))

(defun deepseek-api-request (prompt)
  "Send PROMPT to DeepSeek API and validate response with Eldantic."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")
            ("Authorization" . ,(format "Bearer %s" DEEPSEEK_API_KEY)))))
         (url-request-data
          (json-encode `(("model" . "deepseek-chat")
                         ("messages" . [((role . "user") (content . ,prompt))]))))
         (response-buffer (url-retrieve-synchronously "https://api.deepseek.com/v1/chat/completions")))
         
    (when response-buffer
      (unwind-protect
          (with-current-buffer response-buffer
            (goto-char url-http-end-of-headers)
            (let* ((json-data (json-read-from-string 
                              (buffer-substring (point) (point-max))))
                   (response-plist (json-to-plist json-data)))
              
              ;; Validate response structure
              (condition-case err
                  (progn
                    (new-ApiResponse response-plist)
                    (message "API response validated successfully!")
                    (message "Response content: %s" 
                            (plist-get (plist-get (elt (plist-get response-plist :choices) 0) :message) :content)))
                (error 
                 (message "Validation failed: %s" (error-message-string err)))))
        (kill-buffer response-buffer)))))

;; Example usage:
;; (setq DEEPSEEK_API_KEY "your-api-key-here")  ; Set your API key first
;; (deepseek-api-request "Explain quantum computing in simple terms")