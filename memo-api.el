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
  (setq memo-api-return-value (cdr (assoc 'Data result)))
  (setq memo-api-return-err (cdr (assoc 'Err result)))
  (unless (eq memo-api-return-err "")
        (user-error memo-api-return-err)))

;; get note for review.
(cl-defstruct memo-note
  id weight content file source)

(defvar memo--review-note nil
  "The memo-note object which store note info wait for review.")

(defun memo--set-review-note (x)
 (setq memo--review-note (make-memo-note :id (nth 0 x)
					  :weight (nth 1 x)
					  :content (nth 2 x)
					  :file (nth 3 x)
					  :source (nth 4 x))))

(defun memo--get-review-note-object ()
  "Return memo-note object which need review from server;
memo-note is (orgid  type  content)."
  (let ((result (memo-bridge-call-sync "GetNextReviewNote")))
    (memo--parse-result result)
    (memo--set-review-note memo-api-return-value)))

;; sync org file under dir.
(defun memo-sync-db ()
  "Synchronize the db state with the current Org files on-disk."
  (interactive)
  (let ((result (memo-bridge-call-sync "SyncOrgDir" memo-org-directory "false")))
    (memo--parse-result result)
    (message "Push all org file under dir complete.")))

(defun memo-sync-db-force ()
"Synchronize the db state with the current Org files on-disk."
  (interactive)
  (let ((result (memo-bridge-call-sync "SyncOrgDir" memo-org-directory "true")))
    (memo--parse-result result)
    (message "Force Push all org file under dir complete.")))

(defun memo-sync-file ()
"Synchronize current org-file to db."
  (interactive)
  (save-buffer)
  (let ((result (memo-bridge-call-sync "SyncFile" "false")))
    (memo--parse-result result)
    (message "Push file is success complete.")))

;; auto sync file after save buffer.
(defun memo-sync-file-after-save ()
  "Sync file to database after save file."
  (let ((path (buffer-file-name)))
    (if (and  (f-ext-p path "org") (f-ancestor-of-p memo-org-directory path))
	(memo--parse-result `(memo-api--sync-file ,path "false")))))


(provide 'memo-api)
;;; memo-api.el ends here
