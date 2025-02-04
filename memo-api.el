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

;;; Properties and Value setting.
(defconst memo-prop-note-schedule  "MEMO_NOTE_SCHEDULE"
  "Property used to store the cards type;
and used for backend to indentify memo head.")

(defconst memo-prop-note-id  "ID"
  "Property used to store the cards id; 
and used for backend to indentify memo head.")

(defconst memo-prop-note-weight  "MEMO_NOTE_WEIGHT"
  "Property used to store the cards type;
and used for backend to indentify memo head.")


(defconst memo-prop-note-source  "MEMO_NOTE_SOURCE"
  "Property used to store the cards SOURCE.")

(defvar memo-log-level "0"
  "Setting dynamic module log level, -1 debug, 0 info, 1 warn, 2 error.")

(defvar memo-db-max-idle-conn  "10"
  "Property used to store the cards type;
and used for backend to indentify memo head.")

(defvar memo-db-max-open-conn  "100"
  "Property used to store the cards type;
and used for backend to indentify memo head.")

(defvar memo-db-directory nil
"Setting memo db dir to store database;
if nil will default use user home dir.")

(defvar memo-org-directory nil
"Setting memo dir to scan org file.")




;;; parse api call result; store value and throught err.
(defvar  memo-api-return-err  nil
"Store err message of result return from memo-api call.")

(defvar  memo-api-return-value  nil
"Store value of result return from memo-api call.")

(defun memo--parse-result (api-call)
"To parse value return from 'API-CALL;
catch error to  memo-api-return-err, value to memo-api-return-value"
  (let ((result (catch 'error (eval api-call))))
    (if (stringp result)
        (progn (setq memo-api-return-err result) (setq memo-api-return-value nil) (user-error result))
        (progn (setq memo-api-return-err nil) (setq memo-api-return-value result)))))

;; get note for review.
(cl-defstruct memo-note
  id weight content file source)

(defvar memo--review-note nil
  "The memo-note object which store note info wait for review.")

(defun memo--get-review-note-object ()
  "Return memo-note object which need review from server;
memo-note is (orgid  type  content)."
  (memo--parse-result '(memo-api--get-next-review-note))
  (let* ((note-id (car memo-api-return-value))
	 (note-weight (cadr memo-api-return-value))
	 (note-content (caddr memo-api-return-value))
	 (note-file (cadddr memo-api-return-value))
         (note-source (car (cddddr memo-api-return-value))))
    (setq memo--review-note (make-memo-note :id note-id
					     :weight note-weight
					     :content note-content
					     :file note-file
					     :source note-source))))

(defun memo-sync-db ()
"Synchronize the db state with the current Org files on-disk."
  (interactive)
  (memo--parse-result '(memo-api--sync-dir memo-org-directory "false"))
  (if (not  memo-api-return-err)
      (message "Sync dir is success complete.")))

(defun memo-sync-db-force ()
"Synchronize the db state with the current Org files on-disk."
  (interactive)
  (memo--parse-result '(memo-api--sync-dir memo-org-directory "true"))
  (if (not  memo-api-return-err)
      (message "Sync dir is success complete.")))


(defun memo-sync-file ()
"Synchronize current org-file to db."
  (interactive)
  (save-buffer)
  (memo--parse-result '(memo-api--sync-file (buffer-file-name) "false"))
  (if (not  memo-api-return-err)
      (message "Push file is success complete.")))

;; auto sync file after save buffer.
(defun memo-sync-file-after-save ()
  "Sync file to database after save file."
  (let ((path (buffer-file-name)))
    (if (and  (f-ext-p path "org") (f-ancestor-of-p memo-org-directory path))
	(memo--parse-result `(memo-api--sync-file ,path "false")))))


(provide 'memo-api)
;;; memo-api.el ends here
