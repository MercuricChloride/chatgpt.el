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
;; Homepage: https://github.com/MercuricChloride/chat-gpt-emacs
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
;; Define package-level variables and constants
(defcustom chatgpt-api-key
  nil
  "The API key for the ChatGPT API"
  :type 'variable
  :group 'chatgpt)

(defvar chatgpt-api-url "https://api.openai.com/v1/chat/completions"
  "URL for ChatGPT API")

(defvar chatgpt-latest-response nil
  "Latest response from the ChatGPT API")

(defun chatgpt-format-request (user-message)
  "Format a request to the ChatGPT API."
  (json-encode
   `(:model "gpt-3.5-turbo"
     :messages [
      (:role "system"
      :content "You are a helpful assistant.")
      (:role "user"
      :content ,user-message)])))

;; Define utility functions
(defun chatgpt--api-request (user-message &optional sync)
  "Send a text message to the ChatGPT API and return the response."
        (chatgpt--open-window "Loading Response...")
        (request chatgpt-api-url
        :type "POST"
        :headers `(("Authorization" . ,(format "Bearer %s" chatgpt-api-key))
                   ("Content-Type" . "application/json"))
        :data (chatgpt-format-request user-message)
        :parser 'json-read
        :sync sync
        :success (cl-function
                (lambda (&key data &allow-other-keys)
                (chatgpt--parse-response data)))
        :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                        (chatgpt--open-window "Error: API request failed.")))))

(defun chatgpt--parse-response (response)
  (let ((choices (assoc-default 'choices response)) (response-message nil))
        (setq response-message
         (assoc-default 'content
         (assoc-default 'message
                        (elt choices 0))))
        (setq chatgpt-latest-response response-message)
        (chatgpt--open-window response-message)
        response-message)
  "Parse the response from the ChatGPT API and return the text.")

(defun chatgpt-paste-response ()
  "Paste the latest response from the ChatGPT API into the current buffer."
  (interactive
  (insert chatgpt-latest-response)))

(defun chatgpt--open-window (message)
      (if (< (length (window-list)) 2)
        (split-window-right -75))
      (select-window (window-in-direction 'right))
      (let ((gpt-buffer (get-buffer "ChatGPT Dialogue")))
        (if gpt-buffer
                (progn
                (switch-to-buffer gpt-buffer)
                (erase-buffer))

                (generate-new-buffer "ChatGPT Dialogue"))
        (visual-line-mode 1)
        (insert message)
        ;(switch-to-prev-buffer)
        (select-window (window-in-direction 'left))))

;; Define interactive functions
(defun chatgpt-reply ()
  "Get a reply from ChatGPT"
  (interactive)
  (let ((message (read-string "Message: ")))
    (chatgpt--api-request message)))

(defun chatgpt-reply-and-paste ()
  "Get a reply from ChatGPT and insert it at point."
  (interactive)
  (let ((message (read-string "Message: ")))
    (message "Sending message...")
    (chatgpt--api-request message t)
    (insert chatgpt-latest-response)))

;; Define mode
(define-minor-mode chatgpt-mode
  "Minor mode for integrating ChatGPT into Emacs"
  :lighter " ChatGPT")

(provide 'chatgpt)
;;; chatgpt.el ends here