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
(require 'memo-bridge-epc)
(require 'memo-bridge)

;;; util function
(defun memo-alist-get (x key)
  "Function to get value from alist X by KEY."
  (cdr (assoc (read key) x)))

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


;;; parse api call result; store value and throught err.
(defvar  memo-api-return-err  nil
"Store err message of result return from memo-api call.")

(defvar  memo-api-return-value  nil
"Store value of result return from memo-api call.")

(defun memo--parse-result (result)
"To parse value return from 'API-CALL;
catch error to  memo-api-return-err, value to memo-api-return-value"
  (unless (eq (plist-get result 'epc-error) nil)
    (user-error (plist-get result 'epc-error)))
  (unless (eq (plist-get result 'application-error) nil)
    (user-error (plist-get result 'application-error)))
  (setq memo-api-return-value (memo-alist-get result "Data"))
  (setq memo-api-return-err (memo-alist-get result "Err"))
  (unless (eq memo-api-return-err 'null)
    (user-error (memo-alist-get memo-api-return-err "s")))
  memo-api-return-value)

;; get note for review.
(cl-defstruct memo-note
  id weight content filepath source)

(defvar memo--review-note nil
  "The memo-note object which store note info wait for review.")

(defun memo--set-review-note (x)
 (setq memo--review-note (make-memo-note :id (memo-alist-get x "ID")
					  :weight (memo-alist-get x "Weight")
					  :content (memo-alist-get x "Content")
					  :filepath (memo-alist-get x "FilePath")
					  :source (memo-alist-get x "Source"))))

(defun memo--get-review-note-object ()
  "Return memo-note object which need review from server;
memo-note is (orgid  type  content)."
  (let ((result (memo-bridge-call-sync "GetNextReviewNote")))
    (memo--parse-result result)
    (memo--set-review-note memo-api-return-value)))

(defun memo-api--update-content (id content)
  "Update the content of head."
  (let ((result (memo-bridge-call-sync "UpdateOrgHeadContent" id content)))
    (memo--parse-result result)
    t))

(defun memo-api--update-property (id key value)
  "Update the content of head."
  (let ((result (memo-bridge-call-sync "UpdateOrgHeadProperty" id key value)))
    (memo--parse-result result))
  (message "Content Update finish."))

(defun memo-api--create-virt-head (id title content)
  "Create virt head with TITLE and CONTENT under head with ID."
  (let ((result (memo-bridge-call-sync "CreateVirtualHead" id title content)))
    (memo--parse-result result)
    t))

(defun memo-api--get-virt-heads-by-parentid (parentid)
  "Get virt heads by PARENTID."
  (let ((result (memo-bridge-call-sync "GetVirtualHeadByParentID" parentid)))
    (memo--parse-result result)
    ))

;; sync org file under dir.
(defun memo-sync-db ()
  "Synchronize the db state with the current Org files on-disk."
  (interactive)
  (let ((result (memo-bridge-call-sync "SyncOrgDir" memo-org-directory 0)))
    (memo--parse-result result)
    (message "Push all org file under %s complete." memo-org-directory)))

(defun memo-sync-db-force ()
"Synchronize the db state with the current Org files on-disk."
  (interactive)
  (let ((result (memo-bridge-call-sync "SyncOrgDir" memo-org-directory 1)))
    (memo--parse-result result)
    (message "Force Push all org file under %s complete." memo-org-directory)))

(defun memo-api-sync-file (filePath)
"Synchronize current org-file to db."
  (save-buffer)
  (let ((result (memo-bridge-call-sync "UploadFile" filePath  0  0)))
    (memo--parse-result result)
    (message "Push file is success complete.")))

;; auto sync file after save buffer.
(defun memo-sync-file ()
  "Sync the current buffer file to database."
  (interactive)
  (let ((path (buffer-file-name)))
    (if (and path (f-ext-p path "org") (f-ancestor-of-p memo-org-directory path))
	(memo-api-sync-file path))))



(provide 'memo-api)
;;; memo-api.el ends here
