;;; memo-note.el --- Use for memo (interactive "P") -*- lexical-binding: t; -*-
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


(defun memo-note-suspend-current-note ()
  "Suspend current note."
  (interactive)
  (if memo--buffer-local-note
    (progn (memo-api--update-property
	    (memo-note-id memo--buffer-local-note) "MEMO_NOTE_SCHEDULE" "suspend")
	   (memo-treemacs-buffer-update))))

(defun memo-note-resume-current-note ()
  "Resume current note from suspend."
  (interactive)
  (if memo--buffer-local-note
    (progn (memo-api--update-property
	    (memo-note-id memo--buffer-local-note) "MEMO_NOTE_SCHEDULE" "normal")
	   (memo-treemacs-buffer-update))))


(defun memo-note-review (rate)
  "Review note with score: RATE."
  (if memo--buffer-local-note
      (memo-api--review-note (memo-note-id memo--buffer-local-note) rate ))
  (if (equal (buffer-name (current-buffer)) memo--review-buffer-name)
    (memo-treemacs-buffer-update)))

(defun memo-note-review-easy ()
  "Review note with score: Easy."
  (interactive)
  (memo-note-review "Easy"))

(defun memo-note-review-good ()
  "Review note with score: Good."
  (interactive)
  (memo-note-review "Good"))

(defun memo-note-review-hard ()
  "Review note with score: Hard."
  (interactive)
  (memo-note-review "Hard")
  )

(defun memo-note-review-again ()
  "Review note with score: Again."
  (interactive)
  (memo-note-review "Again")
  )

(defun memo-note-cloze-toggle ()
  "Toggle the all cloze overlay in current review note."
  (interactive)
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (eq (overlay-get ov 'memo-annotation-type) memo-annotation-type-cloze)
      (if (overlay-get ov 'memo-cloze-is-hidden)
          (memo-annotation-cloze-unhide ov)
        (memo-annotation-cloze-hide ov)))))


;;;------------------------------------------------------------------
;;; note find relative function.
;;;------------------------------------------------------------------
(defvar memo-note-query-next '("filter:dueBefore:0" "order:random")
  "Setting the query string to deter the method get next card.
The query is like  operator:type:field:value,
Operator is -/+, type is order/filter,
When type is order field can be weight/due/level/seq/random and value can be asc/desc.
When type is filter field can be fileid/ancestorid/dueAt/dueBefore/dueAfter/parentid/type/limit/state/tag/property.
like filter:fileid:13089182-9F1D-4583-9076-A2B94998A030, filter:dueAt:-3, filter:tag:work,
filter:property:id:123.
Default Operator is +/- means:")

(defun memo-query-next-note ()
  "Find the next note by query in MEMO-NEXT-NOTE-QUERY."
  (memo-api--get-note memo-note-query-next))

(defun memo-read-next-note ()
  "Open next new note in buffer and open side window."
  (interactive)
  (let* ((note (memo-api--get-next-new-card)))
    (if note
	(memo-treemacs-display-note-context note)
      (let* ((note (memo-api--get-next-file-for-read)))
	(memo-treemacs-display-note-context note)))))

(defun memo-read-previous-note ()
  "Open previous note in buffer and open side window."
  (interactive)
  (let* ((note (memo-api--get-previous-new-card)))
    (if note
	(memo-treemacs-display-note-context note)
      (let* ((note (memo-api--get-previous-file-for-read)))
	(memo-treemacs-display-note-context note)))))

(defun memo-read-next-file ()
  "Open the next file has new card and return the note visited before."
  (interactive)
  (let* ((note (memo-api--get-next-file-for-read)))
    (if note
	(memo-treemacs-display-note-context note))))

(defun memo-read-previous-file ()
  "Open the previous file has new card and return the note visited before."
  (interactive)
  (let* ((note (memo-api--get-previous-file-for-read)))
    (if note
	(memo-treemacs-display-note-context note))))


;;;--------------------------------------------------
;;;  util function
;;;--------------------------------------------------
(defun memo-first-nonblank-chars (content num)
  "Return the first nonblank  NUM chars from the first nonempty line of CONTENT."
  (let* ((lines (split-string content "\n"))
         (first-nonempty
          (seq-find (lambda (line)
                      (not (string-match-p "^\\s-*$" line)))
                    lines)))
    (when first-nonempty
      (let* ((trimmed (replace-regexp-in-string "^[ \t]+" "" first-nonempty)))
        (substring trimmed 0 (min num (length trimmed)))))))

(provide 'memo-note)
;;; memo-note.el ends here
