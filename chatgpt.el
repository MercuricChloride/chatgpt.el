;;; chatgpt.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Alexander Gusev
;;
;; Author: Alexander Gusev <goose@soulbound.xyz>
;; Maintainer: Alexander Gusev <goose@soulbound.xyz>
;; Created: March 20, 2023

;; Modified: March 20, 2023
;; Version: 0.0.1

;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/MercuricChloride/chatgpt.el
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;;
;;  Description
;; This package provides an interface to use the chatgpt API. I am building this for my own use, but I am open to pull requests.
;;; Code:
;;
;; (defvar chatgpt-sample-request
;;   (json-encode
;;    '(:model "gpt-3.5-turbo"
;;      :messages [
;;       (:role "system"
;;       :content "You are a helpful assistant.")
;;       (:role "user"
;;       :content "Hello")]))
;;    "Sample request to test the API")

(require 'request)
(require 'json)
(require 'markdown-mode)

;; Define package-level variables and constants
(defcustom chatgpt-api-key
  nil
  "The API key for the ChatGPT API."
  :type 'string
  :group 'chatgpt)

(defvar chatgpt-model
  "gpt-4"
  "The model to use. \"gpt-3.5-turbo\" or \"gpt-4\"")

(defvar chatgpt-api-url
  "https://api.openai.com/v1/chat/completions"
  "URL for ChatGPT API. ")

(defvar chatgpt-latest-response nil
  "Latest response from the ChatGPT API.")

(defvar chatgpt-history []
  "History of messages sent to the ChatGPT API.")

(defun chatgpt-format-request (user-message &optional gpt-4?)
  "Format a USER-MESSAGE to the ChatGPT API."
  (json-encode
   `(:model ,(if gpt-4? "gpt-4" "gpt-3.5-turbo")
     :messages ,(cl-concatenate 'vector
                                chatgpt-history
                                `((:role "user"
                                   :content ,user-message))))))

(defun chatgpt--open-dialogue (message &optional response?)
  "Opens the *gpt-dialogue* buffer or creates a new one if it doesn't exist"
  (let ((original-window (selected-window))
        (buf (get-buffer-create "*gpt-dialogue*"))
        (width 75))
    (display-buffer-in-side-window buf `((side . right) (window-width . ,width)))
    (with-current-buffer buf
      (goto-char (point-max))
      (markdown-mode)
      (if response?
        (insert (format "\nRESPONSE: %s\n----------------\n\n\n" message))
        (insert (format "\nPROMPT: %s\n----------------\n\n\n" message))))
    (select-window original-window)))


(defun chatgpt-clear-dialogue ()
  "Deletes all the text in the *gpt-dialogue* buffer and resets the history."
  (interactive)
  (with-current-buffer (get-buffer-create "*gpt-dialogue*")
    (erase-buffer)
    (chatgpt-reset-history)))

(defun chatgpt-reset-history ()
  "Resets the history of messages sent to the ChatGPT API."
  (interactive)
  (setq chatgpt-history [])
  (setq chatgpt-latest-response nil))


;; Define utility functions
(defun chatgpt--api-request (user-message &optional gpt-4?)
  "Send a USER-MESSAGE to the ChatGPT API and return the response.
If (GPT-4) is non-nil, return the response synchronously."
  (chatgpt--open-dialogue user-message)
  (request chatgpt-api-url
    :type "POST"
    :headers `(("Authorization" . ,(format "Bearer %s" chatgpt-api-key))
               ("Content-Type" . "application/json"))
    :data (chatgpt-format-request user-message gpt-4?)
    :parser 'json-read

    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (chatgpt--parse-response data)))
    :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                          (chatgpt--open-dialogue "Error: API request failed. Error pasted in *chat-gpt-error* buffer." t)
                          (switch-to-buffer (get-buffer-create "*scratch*"))
                          (insert (format "%s" error-thrown))))))

(defun chatgpt--parse-response (response)
  "Parse the RESPONSE from the ChatGPT API and return the text."
  (let ((choices (assoc-default 'choices response)) (response-message nil))
    (setq response-message
          (assoc-default 'content
                         (assoc-default 'message
                                        (elt choices 0))))
    (setq chatgpt-history (cl-concatenate 'vector chatgpt-history (list (assoc-default 'message (elt choices 0)))))
    (setq chatgpt-latest-response response-message)
    (chatgpt--open-dialogue response-message t)
    response-message))

(defun chatgpt-paste-response ()
  "Paste the latest response from the ChatGPT API into the current buffer."
  (interactive
   (insert chatgpt-latest-response)))

;; Define interactive functions
(defun chatgpt-reply (&optional gpt-4?)
  "Continues conversation. If GPT-4? is non-nil, use the gpt-4 model."
  (interactive)
  (let ((message (read-string "Message: ")))
    (chatgpt--api-request message gpt-4?)))

(defun chatgpt-new-reply (&optional gpt-4?)
  "Get a new reply from chatgpt. If GPT-4? is non-nil, use the gpt-4 model."
  (interactive)
  (let ((message (read-string "New Message: ")))
    (chatgpt-clear-dialogue)
    (chatgpt--api-request message gpt-4?)))

(defun chatgpt-explain-region (&optional gpt-4?)
  "Explain the selected region using the ChatGPT API. \ If GPT-4? is non-nil, use the gpt-4 model."
  (interactive)
  (let ((message (concat "Explain the following code: \n" (buffer-substring-no-properties (region-beginning) (region-end)))))
    (chatgpt--api-request message gpt-4?)))

(defun chatgpt-refactor-region (&optional gpt-4?)
  "Refactor the selected region using the ChatGPT API."
  (interactive)
  (let ((message (concat "Refactor the following code." (read-string "How should we refactor this code?") "\n" (buffer-substring-no-properties (region-beginning) (region-end)))))
    (chatgpt--api-request message gpt-4?)))

(defun chatgpt-document-region (&optional gpt-4?)
  "Document the selected region using the ChatGPT API."
  (interactive)
  (let ((message (concat "Return this code, but with documentation added as comments to the code: \n" (buffer-substring-no-properties (region-beginning) (region-end)))))
    (chatgpt--api-request message gpt-4?)))

;; write a function to get what's wrong with a region of code
(defun chatgpt-debug-region (&optional gpt-4?)
  "Debug the selected region using the ChatGPT API."
  (interactive)
  (let ((message (concat "Debug the following code: \n" (buffer-substring-no-properties (region-beginning) (region-end)))))
    (chatgpt--api-request message gpt-4?)))

(defun chatgpt-prompt-region (&optional gpt-4?)
  "Prompt the user for a message and send the selected region as a context."
  (interactive)
  (let ((message (read-string "Message: ")))
    (chatgpt--api-request (concat message "\n" (buffer-substring-no-properties (region-beginning) (region-end))) gpt-4?)))

(defun chatgpt-reply-gpt-4 ()
  "Get a reply from GPT-4."
  (interactive)
  (chatgpt-reply t))

(defun chatgpt-new-reply-gpt-4 ()
  "Get a reply from GPT-4. Starts a new conversation."
  (interactive)
  (chatgpt-new-reply t))

(defun chatgpt-explain-region-gpt-4 ()
  "Explain the selected region using GPT-4."
  (interactive)
  (chatgpt-explain-region t))

(defun chatgpt-refactor-region-gpt-4 ()
  "Refactor the selected region using GPT-4."
  (interactive)
  (chatgpt-refactor-region t))

(defun chatgpt-document-region-gpt-4 ()
  "Document the selected region using GPT-4."
  (interactive)
  (chatgpt-document-region t))

(defun chatgpt-debug-region-gpt-4 ()
  "Debug the selected region using GPT-4."
  (interactive)
  (chatgpt-debug-region t))

(defun chatgpt-prompt-region-gpt-4 ()
  "Prompt the user for a message and send the selected region as a context."
  (interactive)
  (chatgpt-prompt-region t))

;; Define mode
(define-minor-mode chatgpt-mode
  "Minor mode for integrating ChatGPT into Emacs."
  :lighter " ChatGPT")

(provide 'chatgpt)
;;; chatgpt.el ends here
