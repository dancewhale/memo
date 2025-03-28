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
(defconst memo-note-normal-type  1
  "Memo virt type.")

(defconst memo-note-virt-type  2
  "Memo virt type.")

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
  (unless (eq (plist-get result 'error) nil)
    (user-error (plist-get result 'error)))
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
  id path weight source scheduledtype type title hash
  parentid fileid level order status priority
  stability difficulty elapseddays scheduleddays
  reps lapses  state needreview totalcards totalvirtcards
  expiredcards waitingcards reviewingcards)


(defvar memo--review-note nil
  "The memo-note object which store note info wait for review.")

(defun memo-make-note-from-alist (x)
  "Generate memo note object from alist X."
  (if (memo-alist-get x "ID")
      (make-memo-note :id (memo-alist-get x "ID")
		      :path (memo-alist-get x "Path")
		      :weight (memo-alist-get x "Weight")
		      :source (memo-alist-get x "Source")
		      :scheduledtype (memo-alist-get x "ScheduledType")
		      :type (memo-alist-get x "Type")
		      :title (memo-alist-get x "Title")
		      :hash (memo-alist-get x "Hash")
		      :parentid (memo-alist-get x "ParentID")
		      :fileid (memo-alist-get x "FileID")
		      :level (memo-alist-get x "Level")
		      :order (memo-alist-get x "Order")
		      :status (memo-alist-get x "Status")
		      :priority (memo-alist-get x "Priority")
		      :stability (memo-alist-get x "Stability")
		      :difficulty (memo-alist-get x "Difficulty")
		      :elapseddays (memo-alist-get x "ElapsedDays")
		      :scheduleddays (memo-alist-get x "ScheduledDays")
		      :reps (memo-alist-get x "Reps")
		      :lapses (memo-alist-get x "Lapses")
		      :state (memo-alist-get x "State")
		      :needreview (memo-alist-get x "NeedReview")
		      :totalcards (memo-alist-get x "TotalCards")
		      :totalvirtcards (memo-alist-get x "TotalVirtCards")
		      :expiredcards (memo-alist-get x "ExpiredCards")
		      :waitingcards (memo-alist-get x "WaitingCards")
		      :reviewingcards (memo-alist-get x "ReviewingCards"))))

(defun memo-make-note-from-return (y)
  "Try to parser note object from return value Y."
  (if (memo-alist-get y "ID")
      (memo-make-note-from-alist y)
    (if (memo-alist-get (car y) "ID")
        (-map #'memo-make-note-from-alist y)
      nil)))

(cl-defstruct memo-file
  fileid filepath hash totalcards totalvirtcards
  expiredcards waitingcards reviewingcards)

(defun memo-make-file-from-alist (x)
  "Generate memo file object from alist X."
  (if (memo-alist-get x "FilePath")
      (make-memo-file :fileid (memo-alist-get x "FileID")
		      :filepath (memo-alist-get x "FilePath")
		      :hash (memo-alist-get x "Hash")
		      :totalcards (memo-alist-get x "TotalCards")
		      :totalvirtcards (memo-alist-get x "TotalVirtCards")
		      :expiredcards (memo-alist-get x "ExpiredCards")
		      :waitingcards (memo-alist-get x "WaitingCards")
		      :reviewingcards (memo-alist-get x "ReviewingCards"))))

(defun memo-make-file-from-return (y)
  "Try to parser file object from return value Y."
  (if (memo-alist-get y "FilePath")
      (memo-make-file-from-alist y)
    (if (memo-alist-get (car y) "FilePath")
        (-map #'memo-make-file-from-alist y)
      nil)))


(defun memo-api--get-review-note-object ()
  "Return memo-note object which need review from server."
  (let ((result (memo-bridge-call-sync "GetNextReviewNote")))
    (memo--parse-result result))
  (setq memo--review-note (memo-make-note-from-alist memo-api-return-value)))

(defun memo-api--get-content-byid (headid)
  "Get the content of head by HEADID and FILEID."
  (let ((result (memo-bridge-call-sync "GetHeadContentByID" headid)))
    (memo--parse-result result)))

(defun memo-api--update-content (id content)
  "Update the content of head by ID CONTENT."
  (let ((result (memo-bridge-call-sync "UpdateOrgHeadContent" id content)))
    (memo--parse-result result)
    t))

(defun memo-api--review-note (id rate)
  "Review current review-note with ID RATE."
  (let ((result (memo-bridge-call-sync "ReviewNote" id rate)))
    (memo--parse-result result)
    t))

(defun memo-api--update-property (fileid headid key value)
  "Update the content of head by FILEID HEADID KEY VALUE."
  (let ((result (memo-bridge-call-sync "UpdateOrgHeadProperty" fileid headid key value)))
    (memo--parse-result result))
  (message "Update finish."))

(defun memo-api--create-virt-head (id title content)
  "Create virt head with TITLE and CONTENT under head with ID."
  (let ((result (memo-bridge-call-sync "CreateVirtHead" id title content)))
    (memo--parse-result result)
    t))

(defun memo-api--update-virt-file (headid content)
  "Get the content of file by HEADID."
  (let ((result (memo-bridge-call-sync "UploadVirtFile" headid content 1)))
    (memo--parse-result result)))

(defun memo-api--get-virt-file-byid (headid)
  "Get the content of file by HEADID."
  (let ((result (memo-bridge-call-sync "GetVirtFile" headid)))
    (memo--parse-result result)))

(defun memo-api--get-read-files ()
  "Get file that need to read and not finish read yet."
  (let ((result (memo-bridge-call-sync "GetFileHasNewCard")))
    (memo-make-file-from-return (memo--parse-result result))))

(defun memo-api--get-file-children-heads (fileID)
  "Get first heads of file by FILEID."
  (let ((result (memo-bridge-call-sync "GetFileChildrenCard" fileID)))
    (memo-make-note-from-return (memo--parse-result result))))

(defun memo-api--get-head-children-heads (headID fileID type)
  "Get first child heads under head by HEADID FILEID TYPE."
  (let ((result (memo-bridge-call-sync "GetHeadChildrenCard" headID fileID type)))
    (memo-make-note-from-return (memo--parse-result result))))

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
