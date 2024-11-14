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
(require 'org-element)

;; review releat var and function
(defconst memo--review-buffer-name "*memo-review*"
  "The memo buffer for review note show and flip.")


;; review note.
(defun memo--review-show ()
  "Show note in review buffer, MNOTE is memo-note object."
  (if (not (memo-note-id memo--review-note))
      (user-error "Review memo-note object is nil"))
  (let* ((buf (get-buffer-create memo--review-buffer-name))
	 answer-start answer-end)
    (with-current-buffer buf
	(read-only-mode -1)
	(memo-remove-overlays)
	(erase-buffer)
	(insert (memo-note-content memo--review-note))
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
	(read-only-mode)
      )
   )
 )

(defun memo-review-note()
  "Review note."
  (interactive)
  (memo--get-review-note-object)
  (memo--review-show ))


(defun memo-review-easy()
  "Review note with score: Easy."
  (interactive)
  (memo--before-api-call)
  (memo-api--review-note (memo-note-id memo--review-note) "Easy")
  (memo-review-note)
  )

(defun memo-review-good()
  "Review note with score: Good."
  (interactive)
  (memo--before-api-call)
  (memo-api--review-note (memo-note-id memo--review-note) "Good")
  (memo-review-note)
  )

(defun memo-review-hard()
  "Review note with score: Hard."
  (interactive)
  (memo--before-api-call)
  (memo-api--review-note (memo-note-id memo--review-note) "Hard")
  (memo-review-note)
  )

(defun memo-review-again()
  "Review note with score: Again."
  (interactive)
  (memo--before-api-call)
  (memo-api--review-note (memo-note-id memo--review-note) "Again")
  (memo-review-note)
  )

(defun memo-flip-note()
  "Flip current review note."
  (interactive)
  (let* ((buf (get-buffer  memo--review-buffer-name)))
    (if buf
	(with-current-buffer buf
	  (memo-remove-overlays)
	  )))
  )


;; jump to org and enable editor.
(defun memo-goto-org ()
  "Jump to source point from review buffer."
  (interactive)
  (let* ((file (memo-note-file memo--review-note))
        (id  (memo-note-id memo--review-note))
	(position (org-id-find-id-in-file id file 'markerp)))
      (pop-to-buffer-same-window (marker-buffer  position))
      (goto-char position)
      (move-marker position nil)
      (org-fold-show-context)))




(provide 'memo-buffer)
;;; memo-buffer.el ends here
