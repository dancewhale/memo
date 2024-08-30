;;; memo.el --- Use for memo (interactive "P") -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 whale
;;
;; Author: whale <whale@MacBook-Pro-4.local>
;; Maintainer: whale <whale@MacBook-Pro-4.local>
;; Created: 6月 17, 2024
;; Modified: 6月 17, 2024
;; Version: 0.0.1
;; Keywords: memo space repetition
;; Homepage: https://github.com/dancewhale/memo
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;  Thanks  Author of anki-editor, many code borrow from his package.
;;  Use for memo (interactive "P")
;;
;;; Code:

(require 'cl-lib)
(require 'org-element)

(require 'memo-core)

(setq load-path (append load-path (list (file-truename default-directory))))
(load-file (concat default-directory "memo.so"))

;;; Core primitives

(defconst memo-prop-note-type "MEMO_NOTE_TYPE")
(defconst memo-prop-note-id   "ID")
(defconst memo--review-buffer-name "*memo-review*"
  "The memo buffer for review note show and flip."
  )

(cl-defstruct memo-note
  id type content finish)

(defvar memo--review-note nil
  "The memo-note object which store note info wait for review.")


(defun memo-push-note-at-point ()
  "Push note at point to memo db.
If heading without an `MEMO_NOTE_TYPE' property push failed.
If heading without an `ID' property create it."
  (interactive)
  (save-excursion
    (let* ((note-type  (org-entry-get nil memo-prop-note-type))
	   (note-content  (memo--note-contents-current-heading))
	   (note-id (org-id-get-create)))
      (if (not note-type)
	  (user-error "Missing note type")
	(if (memo--create-note note-id note-type note-content)
	    (message "Create note to memo successful.")
	  (message "Create note to memo failed."))))))

(defun memo-review-note()
  "Review note."
  (interactive)
  (setq memo--review-note (memo--get-review-note-object-from-server))
  (memo--review-show memo--review-note)
)

(defun memo--review-show (mnote)
  "Show note in review buffer, MNOTE is memo-note object."
  (if (not mnote)
      (user-error "Review memo-note object is nil"))
  (let* ((buf (get-buffer-create memo--review-buffer-name))
	 answer-start answer-end)
    (with-current-buffer buf
	(memo-remove-overlays)
	(erase-buffer)
	(insert (memo-note-content mnote))
	(goto-char (point-min))
	(if (re-search-forward "^-+$" t t)
	  (forward-line)
	  (beginning-of-line)
	  (setq answer-start (point))
	  (goto-char (point-max))
	  (setq answer-end (point))
	  (memo-hide-region answer-start answer-end))
	(switch-to-buffer buf)
	(org-mode)
      )
   )
 )

(defun memo-flip-note()
  "Flip review note."
  (interactive)
  (let* ((buf (get-buffer  memo--review-buffer-name)))
    (if buf
	(with-current-buffer buf
	  (memo-remove-overlays)
	  )))
  )

(defun memo-goto-org ()
  (interactive)
  (org-id-goto (memo-note-id memo--review-note)))

(defun memo--get-review-note-object-from-server ()
  "Return memo-note object which need review from server."
  (let* ((memo-note-object (memo--get-next-review-note))
	 (note-id (car memo-note-object))
	 (note-type (cadr memo-note-object))
	 (note-content (caddr memo-note-object)))
    (make-memo-note :id note-id
		    :type note-type
		    :content note-content
		    :finish nil)))

(defun memo--get-note-object-from-point ()
  "Make and return memo-note object from current note."
  (let* ((note-id (org-id-get-create))
	 (note-content  (memo--note-contents-current-heading))
	 (note-type (org-entry-get nil memo-prop-note-type))
    (unless note-type (user-error "Missing note type"))
    (make-memo-note :id note-id
		    :type note-type
		    :content note-content
		    :finish nil)))
  )




(defun memo--note-contents-current-heading ()
  "Get content between heading at point and next sub/heading.
Leading whitespace, drawers, and planning content is skipped."
  (save-excursion
    (let* ((element (org-element-at-point))
	   (begin (cl-loop for eoh = (org-element-property
				      :contents-begin element)
			   then (org-element-property :end subelem)
			   while eoh
			   for subelem = (progn
					   (goto-char eoh)
					   (org-element-context))
			   while (memq (org-element-type subelem)
				       '(drawer planning property-drawer))
			   finally return (and eoh (org-element-property
						    :begin subelem))))
	   (end (cl-loop for eoh = (org-element-property
				    :contents-begin element)
			 then (org-element-property :end nextelem)
			 while eoh
			 for nextelem = (progn
					  (goto-char eoh)
					  (org-element-at-point))
			 while (not (or (memq (org-element-type nextelem)
					      '(headline))
					(eobp)))
			 finally return (and eoh (if (eobp)
						     (org-element-property :end nextelem)
						   (org-element-property :begin nextelem)))))
	   (contents-raw (or (and begin
				  end
				  (buffer-substring-no-properties
				   begin
				   ;; in case the buffer is narrowed,
				   ;; e.g. by `org-map-entries' when
				   ;; scope is `tree'
				   (min (point-max) end)))
			     "")))
      contents-raw)))

(provide 'memo)
;;; memo.el ends here
