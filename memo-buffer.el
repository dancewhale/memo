;;; memo-buffer.el --- Use for memo (interactive "P") -*- lexical-binding: t; -*-
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
(require 'thunk)
(require 'org-element)

;; custom var
(defcustom memo-inherit-tags t
  "Inherit tags, set to nil to turn off."
  :type 'boolen
  :group 'memo)

(defcustom memo-default-tags nil
  "Default tags.

This variable will be used if none is set on the org item nor as
a global property."
  :type '(repeat string)
  :group 'memo)

(defcustom memo-default-note-type "default"
  "Default note type.

This variable will be used if none is set on the org item nor as
a global property."
  :type 'string
  :group 'memo)


;; review releat var and function
(defconst memo--review-buffer-name "*memo-review*"
  "The memo buffer for review note show and flip.")



;;; Org Tags / Properties

(defconst memo-prop-note-type  "MEMO_NOTE_TYPE"
  "Property used to store the cards type.")

(defconst memo-prop-note-hash  "MEMO_NOTE_HASH"
"Used to determine whether the note has been modified.")

(defconst memo-prop-global-tags "MEMO_TAGS"
  "Specify Memo note tags.")

;;; error operator
(defun memo--parse-result (result)
  (if  (car result)
    (user-error (car result))
    (cadr result)
  ))


;; get note for review.
(cl-defstruct memo-note
  id type content hash)

(defvar memo--review-note nil
  "The memo-note object which store note info wait for review.")

(defun memo--get-review-note-object ()
  "Return memo-note object which need review from server;
memo-note is (orgid  type  content)."
  (let* ((memo-note-object (memo--parse-result (memo-api--get-next-review-note)))
	 (note-id (car memo-note-object))
	 (note-type (cadr memo-note-object))
	 (note-content (caddr memo-note-object)))
    (setq memo--review-note (make-memo-note :id note-id
					     :type note-type
					     :content note-content
					     :hash nil))))


;; review note.
(defun memo-review-show ()
  "Show note in review buffer, MNOTE is memo-note object."
  (if (not (memo-note-id memo--review-note))
      (user-error "Review memo-note object is nil"))
  (let* ((buf (get-buffer-create memo--review-buffer-name))
	 answer-start answer-end)
    (with-current-buffer buf
	(memo-remove-overlays)
	(erase-buffer)
	(insert (memo-note-content mnote))
	(goto-char (point-min))
	(if (re-search-forward "^-+$" nil t)
	    (progn
	       (forward-line)
	       (beginning-of-line)
	       (setq answer-start (point))
	       (goto-char (point-max))
	       (setq answer-end (point))
	       (memo-hide-region answer-start answer-end)))
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

(defun memo-review-easy()
  "Review note with score: Easy."
  (interactive)
  (memo-api--review-note (memo-note-id memo--review-note) "Easy")
  (memo-review-note)
  )

(defun memo-review-good()
  "Review note with score: Good."
  (interactive)
  (memo-api--review-note (memo-note-id memo--review-note) "Good")
  (memo-review-note)
  )

(defun memo-review-hard()
  "Review note with score: Hard."
  (interactive)
  (memo-api--review-note (memo-note-id memo--review-note) "Hard")
  (memo-review-note)
  )

(defun memo-review-again()
  "Review note with score: Again."
  (interactive)
  (memo-api--review-note (memo-note-id memo--review-note) "Again")
  (memo-review-note)
  )


(provide 'memo-buffer)
;;; memo-buffer.el ends here
