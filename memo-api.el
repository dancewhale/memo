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

(defconst memo-prop-annotation-color  "MEMO_NOTE_COLOR"
  "Property used to store the color of annotation.")


(defvar memo--review-note nil
  "The memo-note object which store note info wait for review.")

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

;; Setting note object.
(cl-defstruct memo-note
  id path weight source scheduledtype title hash
  parentid fileid headlineid virtfilehash level order
  status priority due stability difficulty elapseddays
  scheduleddays reps lapses  state needreview lastreview
  childvirtcards totalcards totalvirtcards expiredcards
  waitingcards reviewingcards)


(defun memo-make-note-from-alist (x)
  "Generate memo note object from alist X."
  (if (memo-alist-get x "ID")
      (make-memo-note :id (memo-alist-get x "ID")
		      :path (memo-alist-get x "Path")
		      :weight (memo-alist-get x "Weight")
		      :source (memo-alist-get x "Source")
		      :scheduledtype (memo-alist-get x "ScheduledType")
		      :title (memo-alist-get x "Title")
		      :hash (memo-alist-get x "Hash")
		      :parentid (memo-alist-get x "ParentID")
		      :fileid (memo-alist-get x "FileID")
		      :headlineid (memo-alist-get x "HeadlineID")
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
		      :lastreview (memo-alist-get x "LastReview")
		      :childvirtcards (memo-alist-get x "ChildVirtCards")
		      :totalcards (memo-alist-get x "TotalCards")
		      :totalvirtcards (memo-alist-get x "TotalVirtCards")
		      :expiredcards (memo-alist-get x "ExpiredCards")
		      :waitingcards (memo-alist-get x "WaitingCards")
		      :reviewingcards (memo-alist-get x "ReviewingCards"))))

(defun memo-make-note-from-return (y)
  "Try to parser note object from return value Y."
  (catch 'value
    (if (equal 'null y)
	(throw 'value nil))
    (if (memo-alist-get y "ID")
	(throw 'value (memo-make-note-from-alist y))
      (if (memo-alist-get (car y) "ID")
          (throw 'value (-map #'memo-make-note-from-alist y))
	(throw 'value  nil)))))

;;; --------------------------------------------------
;;;  annotation relate function
;;; --------------------------------------------------
(cl-defstruct memo-annotation id start end  parentheadid childheadid text face type)

(defun memo-make-annotation-from-alist (x)
  "Generate memo annotation object from alist X."
  (if (memo-alist-get x "ID")
      (make-memo-annotation :id (memo-alist-get x "ID")
		      :start (memo-alist-get x "Start")
		      :end (memo-alist-get x "End")
		      :parentheadid (memo-alist-get x "ParentHeadlineID")
		      :childheadid (memo-alist-get x "ChildHeadlineID")
		      :text (memo-alist-get x "CommentText")
		      :type (memo-alist-get x "Type")
		      :face (memo-alist-get x "Face"))))

(defun memo-make-annotation-from-return (y)
  "Try to parser annotation object from return value Y."
  (if (memo-alist-get y "ID")
      (memo-make-annotation-from-alist y)
    (if (memo-alist-get (car y) "ID")
        (-map #'memo-make-annotation-from-alist y)
      nil)))

(defun memo-make-alist-from-annotation (annotation)
  "Convert ANNOTATION object back to an alist format."
  (list (cons "ID" (memo-annotation-id annotation))
        (cons "Start" (memo-annotation-start annotation))
        (cons "End" (memo-annotation-end annotation))
        (cons "ParentHeadlineID" (memo-annotation-parentheadid annotation))
        (cons "ChildHeadlineID" (memo-annotation-childheadid annotation))
        (cons "CommentText" (memo-annotation-text annotation))
        (cons "Type" (memo-annotation-type annotation))
        (cons "Face" (memo-annotation-face annotation))))

(defun memo-make-list-from-annotations-table (hash)
  "Convert HASH table of ANNOTATIONS objects to a list of alists."
  (unless (hash-table-p hash)
    (user-error "The arg you pass is not hash table"))
  (let ((anno-list nil)
	(ids (hash-table-keys hash)))
    (when ids
      (dolist (id ids)
	(let ((anno (gethash id hash)))
	  (setq anno-list (append anno-list (list (memo-make-alist-from-annotation anno)))))))
    anno-list))


;;; --------------------------------------------------
;;;  memo file relate function
;;; --------------------------------------------------
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


;;; --------------------------------------------------
;;;  memo api function
;;; --------------------------------------------------
(defun memo-api--get-note-path (id)
  "Get current note with ID."
  (let ((result (memo-bridge-call-sync "GetHeadFilePath" id)))
    (memo--parse-result result)))

(defun memo-api--get-content-byid (headid)
  "Get the content of head by HEADID and FILEID."
  (let ((result (memo-bridge-call-sync "GetHeadContentByID" headid)))
    (memo--parse-result result)))

(defun memo-api--update-content (id content)
  "Update the content of head by ID CONTENT."
  (let ((result (memo-bridge-call-sync "UpdateOrgHeadContent" id content)))
    (memo--parse-result result)
    t))

(defun memo-api--update-title (id title)
  "Update the content of head by ID TITLE."
  (let ((result (memo-bridge-call-sync "UpdateOrgHeadTitle" id title)))
    (memo--parse-result result)
    t))

(defun memo-api--review-note (id rate)
  "Review current review-note with ID RATE."
  (let ((result (memo-bridge-call-sync "ReviewNote" id rate)))
    (memo--parse-result result)
    t))

(defun memo-api--update-property (headid key value)
  "Update the property of head by HEADID KEY VALUE."
  (let ((result (memo-bridge-call-sync "UpdateOrgHeadProperty" headid key value)))
    (memo--parse-result result))
  (message "Update finish."))

(defun memo-api--get-property (headid key)
  "Get the property of head by HEADID KEY VALUE."
  (let ((result (memo-bridge-call-sync "GetOrgHeadProperty" headid key)))
    (memo--parse-result result)))

;; api used by treemacs and head relative code.
(defun memo-api--create-virt-head (id title content)
  "Create virt head with TITLE and CONTENT under head with ID.
Returns the ID of the created virt head."
  (let ((result (memo-bridge-call-sync "CreateVirtHead" id title content)))
    (memo--parse-result result)))

(defun memo-api--get-first-file-head (id)
  "Get first file head by ID."
  (let ((result (memo-bridge-call-sync "GetFirstFileHeadCard" id)))
    (memo-make-note-from-return (memo--parse-result result))))


(defun memo-api--get-children-virt-head (headid)
  "Get children heads under head by HEADID."
  (let ((result (memo-bridge-call-sync "GetHeadChildrenVirtCard" headid)))
    (memo-make-note-from-return (memo--parse-result result))))

(defun memo-api--update-virt-file (headid content)
  "Upload the CONTENT of file by HEADID."
  (let ((result (memo-bridge-call-sync "UploadVirtFile" headid content 1)))
    (memo--parse-result result)))

(defun memo-api--get-virt-file-byid (headid)
  "Get the content of file by HEADID."
  (let ((result (memo-bridge-call-sync "GetVirtFile" headid)))
    (memo--parse-result result)))

(defun memo-api--get-file-by-filid (fileid)
  "Get file info by FILEID."
  (let ((result (memo-bridge-call-sync "GetFileByFileID" fileid)))
    (memo-make-file-from-return (memo--parse-result result))))

(defun memo-api--get-read-file-byid (fileid)
  "Get the file info by FILEID."
  (let ((result (memo-bridge-call-sync "GetFileByID" fileid)))
    (memo-make-file-from-return (memo--parse-result result))))

(defun memo-api--get-file-children-heads (fileid)
  "Get first level heads of file by FILEID."
  (let ((result (memo-bridge-call-sync "GetFileChildrenCard" fileid)))
    (memo-make-note-from-return (memo--parse-result result))))

(defun memo-api--get-head-children-heads (headid fileid)
  "Get first child heads under head by HEADID FILEID."
  (let ((result (memo-bridge-call-sync "GetHeadChildrenCard" headid fileid)))
    (memo-make-note-from-return (memo--parse-result result))))

;;;; annotation api
(defun memo-api--annotation-get-list-by-parentheadid (headid)
  "Get the annotations by HEADID."
  (let ((result (memo-bridge-call-sync "GetAnnotationsByHeadlineID" headid)))
    (memo-make-annotation-from-return (memo--parse-result result))))

(defun memo-api--annotation-get-by-id (id)
  "Get the annotation by Annotation ID."
  (let ((result (memo-bridge-call-sync "GetAnnotationByID" id)))
    (memo-make-annotation-from-return (memo--parse-result result))))

(defun memo-api--annotation-create (headid startPos endPos text face type)
  "Create annotation in HEADID and region from STARTPOS to ENDPOS with TEXT FACE TYPE."
  (let ((result (memo-bridge-call-sync "CreateAnnotation" headid startPos endPos text face type)))
    (memo-make-annotation-from-return (memo--parse-result result))))

(defun memo-api--annotation-update (annotation)
  "Update the annotation by ANNOTATION object."
  (let ((result (memo-bridge-call-sync "UpdateAnnotation"
				       (memo-annotation-id annotation)
				       (memo-annotation-start annotation)
				       (memo-annotation-end annotation)
				       (memo-annotation-text annotation)
				       (memo-annotation-face annotation)
				       (memo-annotation-type annotation))))
    (memo--parse-result result)))

(defun memo-api--annotation-update-region (id start end)
  "Update the region annotation by ID START END."
  (let ((result (memo-bridge-call-sync "UpdateAnnotationRegion" id start end)))
    (memo--parse-result result)))

(defun memo-api--annotation-update-region-by-list (annotationList)
  "Update the region of annotation in ANNOTATIONLIST."
  (let ((result (memo-bridge-call-sync "UpdateAnnotationsRegion" annotationList)))
    (memo--parse-result result)))

(defun memo-api--annotation-delete-by-id (id)
  "Delete the annotation by Annotation ID."
  (let ((result (memo-bridge-call-sync "DeleteAnnotationByID" id)))
    (memo--parse-result result)))

(defun memo-api--annotation-update-comment (id comment)
  "Update the annotation COMMENT text by Annotation ID."
  (let ((result (memo-bridge-call-sync "UpdateAnnotationComment" id comment)))
    (memo--parse-result result)))

(defun memo-api--annotation-get-comment (id)
  "Get the annotation comment text by Annotation ID."
  (let ((result (memo-bridge-call-sync "GetAnnotationComment" id)))
    (memo--parse-result result)))


(defun memo-api--annotation-get-original-text (id)
  "Get the text that annotation mark in original card by ID."
  (let ((result (memo-bridge-call-sync "GetAnnotationOriginalText" id)))
    (memo--parse-result result)))


;; get card api
;; custom card get api
(defun memo-api--get-note (querylist)
  "Get note by query string list QUERYLIST."
  (let ((result (memo-bridge-call-sync "FindNote" querylist)))
    (memo-make-note-from-return (memo--parse-result result))))

(defun memo-api--get-note-list (querylist)
  "Get note list by query string list QUERYLIST."
  (let ((result (memo-bridge-call-sync "FindNoteList" querylist)))
    (memo-make-note-from-return (memo--parse-result result))))

;; get review card api
(defun memo-api--get-next-review-card ()
  "Get next review note."
  (let ((result (memo-bridge-call-sync "GetNextReviewCard")))
    (memo-make-note-from-return (memo--parse-result result))))

(defun memo-api--get-previous-review-card ()
  "Get previous review note."
  (let ((result (memo-bridge-call-sync "GetPreviousReviewCard")))
    (memo-make-note-from-return (memo--parse-result result))))

;; get read new card api
(defun memo-api--get-next-new-card ()
  "Get next new note."
  (let ((result (memo-bridge-call-sync "GetNextNewCard")))
    (memo-make-note-from-return (memo--parse-result result))))

(defun memo-api--get-previous-new-card ()
  "Get previous new note."
  (let ((result (memo-bridge-call-sync "GetPreviousNewCard")))
    (memo-make-note-from-return (memo--parse-result result))))

;; select file for new card.
(defun memo-api--get-next-file-for-read ()
  "Get next file for new note."
  (let ((result (memo-bridge-call-sync "GetNextFileNewCard")))
    (memo-make-note-from-return (memo--parse-result result))))

(defun memo-api--get-previous-file-for-read ()
  "Get previous file for new note."
  (let ((result (memo-bridge-call-sync "GetPreviousFileNewCard")))
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


