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
(require 'org-tidy)
(require 'org-element)
(require 'f)

;; review releat var and function
(defconst memo--review-buffer-name "*memo-review*"
  "The memo buffer for review note show and flip.")

(defconst memo--view-buffer-name "*memo-view*"
  "The memo buffer for view child virtual head.")

(defvar-local memo--buffer-local-note nil
  "The memo note object store in current buffer.")


(defconst memo--source-buffer-name "*memo-source*"
  "The memo buffer for review note show and flip.")

(defun memo-skip-current-review-note ()
  "Skip current review note and review next note."
  (interactive)
  (when (and memo--review-note (equal (buffer-name (current-buffer)) memo--review-buffer-name))
    (progn (memo-api--update-property (memo-note-id memo--review-note) "MEMO_NOTE_SCHEDULE" "postpone")
	   (memo-review-note))))

(defun memo-suspend-current-review-note ()
  "Skip current review note and review next note."
  (interactive)
  (when (and memo--review-note (equal (buffer-name (current-buffer)) memo--review-buffer-name))
    (progn (memo-api--update-property (memo-note-id memo--review-note) "MEMO_NOTE_SCHEDULE" "suspend")
	   (memo-review-note))))

(defun memo-update-current-note-content ()
  "Skip current review note and review next note."
  (interactive)
  (when memo--buffer-local-note
    (progn (memo-api--update-content (memo-note-id memo--buffer-local-note)
				     (buffer-substring-no-properties (point-min) (point-max)))
           (set-buffer-modified-p nil) t)))

(defun memo-review-note()
  "Get next review note in review buffer."
  (interactive)
  (memo--get-review-note-object)
  (if (not (memo-note-id memo--review-note))
      (user-error "Review memo-note object is nil"))
  (let* ((buf (get-buffer-create memo--review-buffer-name))
	 answer-start answer-end)
    (with-current-buffer buf
      (memo-card-remove-overlays)
      (erase-buffer)
      (insert (memo-note-content memo--review-note))
      (memo-card-hidden)
      (memo-cloze-hidden)
      (set-buffer-modified-p nil)
      (org-mode))
    (switch-to-buffer buf)
    (setq memo--buffer-local-note memo--review-note)
    (setq write-contents-functions '(memo-update-current-note-content))
    (save-excursion  (memo-treemacs-update))))


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

(defun memo-flip-note()
  "Flip current review note."
  (interactive)
  (let* ((buf (get-buffer  memo--review-buffer-name)))
    (if buf
	(with-current-buffer buf
	  (memo-remove-all-overlays)
	  ))))

;; virtual head
(defun memo-create-virt-head ()
  "Create a virtual head with user input title and content under head with ID."
  (interactive)
  (let* ((title (read-string "Enter title: "))
         (content (read-string "Enter content: "))
         (id  (memo-note-id memo--review-note)))
    (when (and title content)
      (memo-api--create-virt-head id title content)
      (message "Virtual head created successfully."))
    (when (or (string-empty-p title) (string-empty-p content))
      (user-error "Title and content cannot be empty"))))


(defun memo-get-current-note-id ()
  "Get current note id."
  (memo-note-id memo--buffer-local-note)
)

(defun memo-get-review-note ()
  "Get Current Review note."
  (let* ((buf (get-buffer-create memo--review-buffer-name)))
    (with-current-buffer buf
      memo--buffer-local-note)))

(defun memo-open-head-in-view-buffer(head)
  "Open HEAD in view buffer."
  (if (not (memo-note-id head))
      (user-error "Memo-note object is nil"))
  (let* ((buf (get-buffer-create memo--view-buffer-name)))
    (pop-to-buffer buf)
    (with-current-buffer buf
      (erase-buffer)
      (insert (memo-note-content head))
      (set-buffer-modified-p nil)
      (org-mode)
      (setq memo--buffer-local-note head)
      (setq write-contents-functions '(memo-update-current-note-content)))))




;; jump to org and enable editor.
(defun memo-goto-org ()
  "Jump to source point from review buffer."
  (interactive)
  (let* ((file (memo-note-filepath memo--review-note))
        (id  (memo-note-id memo--review-note))
	(position (org-id-find-id-in-file id file 'markerp)))
      (pop-to-buffer-same-window (marker-buffer  position))
      (goto-char position)
      (move-marker position nil)
      (widen)
      (org-fold-show-context)
      (memo-narrow-to-org-subtree-content)
      (org-tidy-mode 1)))


;; jump to the source of node.
(defun memo-goto-source-direct ()
  "Jump to source of node."
  (interactive)
  (if (not (memo-note-id memo--review-note))
      (user-error "Review memo-note object is nil"))
  (let* ((source (memo-note-source memo--review-note))
	 (buf (get-buffer-create memo--source-buffer-name)))
    (save-excursion
      (with-current-buffer buf
	(org-link-open-from-string source)))))

;; wait for use.
(defun memo-goto-source ()
"Open an org-mode LINK in a new buffer on the right side."
  (interactive)
  (if (not (memo-note-id memo--review-note))
      (user-error "Review memo-note object is nil"))
  (let ((source (memo-note-source memo--review-note))
	(buffer (get-buffer-create memo--source-buffer-name)))
    (display-buffer-in-side-window buffer '((side . right) (window-width . 0.5)))
    (set-window-dedicated-p (get-buffer-window buffer) t)  
    (with-current-buffer buffer
      (org-link-open-from-string source)
      (org-mode)
      (goto-char (point-min)))))


(provide 'memo-buffer)
;;; memo-buffer.el ends here
