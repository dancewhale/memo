;;; memo-api.el --- Use for memo (interactive "P") -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 whale
;;
;; Author: whale <whale@MacBook-Pro-4.local>
;; Maintainer: whale <whale@MacBook-Pro-4.local>
;; Created: 6月 17, 2024
;; Modified: 6月 17, 2024
;; Keywords: memo space repetition
;; Homepage: https://github.com/dancewhale/memo
;; Package-Requires: ((emacs "24.3") (cl-lib "0.6.1") (org "9.6.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;  Use for memo fsrs library.
;;
;;; Code:

(require 'cl-lib)
(require 'org-element)

;;; Properties and Env setting
(defconst memo-prop-note-type  "MEMO_NOTE_TYPE"
  "Property used to store the cards type;
and used for backend to indentify memo head.")

(defconst memo-prop-note-id  "ID"
  "Property used to store the cards id; 
and used for backend to indentify memo head.")

(defvar  memo-log-level "0"
  "Setting dynamic module log level, -1 debug, 0 info, 1 warn, 2 error.")


;;; hook for memo-api call.
;;; Setting env before call go module api.
(defun memo--before-api-call ()
  "Use to prepare some action like env setting before call backend api."
  (setenv "MEMO_TYPE_PROVERTY" memo-prop-note-type)
  (setenv "MEMO_ID_PROVERTY" memo-prop-note-id)
  (setenv "MEMO_LOG_LEVEL" memo-log-level))



;;; parse api call result; store value and throught err.
(cl-defstruct memo-api--return
  err value)

(defvar  memo-api-result  nil
"Store result return from memo-api call.")

(defvar  memo-api-result-err  nil
"Store err message of result return from memo-api call.")

(defvar  memo-api-result-value  nil
"Store value of result return from memo-api call.")

(defun memo--parse-result (result)
"To parse value return from backend memo-api call;
pare value like (err (value value))"
  (setq memo-api-result  (make-memo-api--return  :err (car result) 
                                                 :value (cadr result)))
  (setq memo-api-result-err (memo-api--return-err memo-api-result))
  (setq memo-api-result-value (memo-api--return-value  memo-api-result))
  (if  (car result)
    (user-error (car result))))


;; get note for review.
(cl-defstruct memo-note
  id type content file)

(defvar memo--review-note nil
  "The memo-note object which store note info wait for review.")

(defun memo--get-review-note-object ()
  "Return memo-note object which need review from server;
memo-note is (orgid  type  content)."
  (memo--before-api-call)
  (memo--parse-result (memo-api--get-next-review-note))
  (let* ((note-id (car memo-api-result-value))
	 (note-type (cadr memo-api-result-value))
	 (note-content (caddr memo-api-result-value))
	 (note-file (cadddr memo-api-result-value)))
    (setq memo--review-note (make-memo-note :id note-id
					     :type note-type
					     :content note-content
					     :file note-file))))

;; setting memo scan dir and function.
(defvar  memo-org-directory nil
"Setting memo dir to scan org file.")

(defvar  memo-db-path nil
"Setting memo db dir path to store database, default is user home dir.")


(defun memo-sync-db ()
"Synchronize the db state with the current Org files on-disk."
  (interactive)
  (memo--parse-result (memo-api--sync-dir memo-org-directory "false"))
  (if (not  (memo-api--return-err memo-api-result) )
      (message "Sync dir is success complete.")))

(defun memo-sync-db-force ()
"Synchronize the db state with the current Org files on-disk."
  (interactive)
  (memo--parse-result (memo-api--sync-dir memo-org-directory "true"))
  (if (not  (memo-api--return-err memo-api-result) )
      (message "Sync dir is success complete.")))


(defun memo-sync-file ()
"Synchronize current org-file to db."
  (interactive)
  (save-buffer)
  (memo--parse-result (memo-api--sync-file (buffer-file-name) "false"))
  (if (not  (memo-api--return-err memo-api-result) )
      (message "Push file is success complete.")))



(provide 'memo-api)
;;; memo-api.el ends here
