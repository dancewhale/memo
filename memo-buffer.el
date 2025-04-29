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

(defconst memo--source-buffer-name "*memo-source*"
  "The memo buffer for review note show and flip.")

(defvar-local memo--buffer-local-note nil
  "The memo note object store in current buffer.")

(defvar-local memo--buffer-local-note-path nil
  "The memo note path store in current buffer, use to locate node in treemacs.")

(defun memo-skip-current-note ()
  "Skip current review note and review next note."
  (interactive)
  (if memo--buffer-local-note
    (progn (memo-api--update-property
	    (memo-note-id memo--buffer-local-note) "MEMO_NOTE_SCHEDULE" "postpone")
	   (memo-treemacs-refresh))))

(defun memo-suspend-current-note ()
  "Suspend current note."
  (interactive)
  (if memo--buffer-local-note
    (progn (memo-api--update-property
	    (memo-note-id memo--buffer-local-note) "MEMO_NOTE_SCHEDULE" "suspend")
	   (memo-treemacs-refresh))))

(defun memo-resume-current-note ()
  "Resume current note from suspend."
  (interactive)
  (if memo--buffer-local-note
    (progn (memo-api--update-property
	    (memo-note-id memo--buffer-local-note) "MEMO_NOTE_SCHEDULE" "normal")
	   (memo-treemacs-refresh))))

(defun memo-update-current-virt-note-content ()
  "Skip current review note and review next note."
  (interactive)
  (if memo--buffer-local-note
    (progn (memo-api--update-virt-file (memo-note-id memo--buffer-local-note)
				     (buffer-substring-no-properties (point-min) (point-max)))
           (set-buffer-modified-p nil) t)))



(defun memo-update-current-note-content ()
  "Skip current review note and review next note."
  (interactive)
  (if memo--buffer-local-note
    (progn (memo-api--update-content (memo-note-id memo--buffer-local-note)
				     (buffer-substring-no-properties (point-min) (point-max)))
           (set-buffer-modified-p nil) t)))

(defun memo-show-review-note()
  "Get next review note in review buffer."
  (interactive)
  (memo-api--get-review-note-object)
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
    (setq write-contents-functions '(memo-save-buffer))
    (save-excursion  (memo-treemacs-update))))


(defun memo-review-note (rate)
  "Review note with score: RATE."
  (if memo--buffer-local-note
      (memo-api--review-note (memo-note-id memo--buffer-local-note) rate ))
  (if (equal (buffer-name (current-buffer)) memo--review-buffer-name)
      (progn  (memo-review-note)
	      (memo-treemacs-refresh))))

(defun memo-review-easy()
  "Review note with score: Easy."
  (interactive)
  (memo-review-note "Easy"))

(defun memo-review-good()
  "Review note with score: Good."
  (interactive)
  (memo-review-note "Good"))

(defun memo-review-hard()
  "Review note with score: Hard."
  (interactive)
  (memo-review-note "Hard")
  )

(defun memo-review-again()
  "Review note with score: Again."
  (interactive)
  (memo-review-note "Again")
  )

(defun memo-flip-note()
  "Flip current review note."
  (interactive)
  (let* ((buf (get-buffer  memo--review-buffer-name)))
    (if buf
	(with-current-buffer buf
	  (memo-remove-all-overlays)
	  ))))

(defun memo-get-current-note-id ()
  "Get current note id."
  (memo-note-id memo--buffer-local-note)
)

(defun memo-get-review-note ()
  "Get Current Review note."
  (let* ((buf (get-buffer-create memo--review-buffer-name)))
    (with-current-buffer buf
      memo--buffer-local-note)))

;; jump to org and enable editor.
(defun memo-goto-org ()
  "Jump to source point from review buffer."
  (interactive)
  (let* ((id  (memo-note-id memo--buffer-local-note))
	 (file (memo-api--get-note-path id))
	 (position (org-id-find-id-in-file id file 'markerp)))
    (if (not position)
        (user-error "Can't find id '%s' in file '%s'" id file))
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
  (if (not (memo-note-p memo--buffer-local-note))
      (user-error "Review memo-note object is nil"))
  (let* ((source (memo-note-source memo--buffer-local-note))
	 (buf (get-buffer-create memo--source-buffer-name)))
    (save-excursion
      (with-current-buffer buf
	(org-link-open-from-string source)))))

;; wait for use.
(defun memo-goto-source ()
  "Open an ORG-MODE LINK in a new buffer on the right side."
  (interactive)
  (if (not (memo-note-p memo--buffer-local-note))
      (user-error "Local buffer memo-note object is nil"))
  (let ((source (memo-note-source memo--buffer-local-note))
	(buffer (get-buffer-create memo--source-buffer-name)))
    (display-buffer-in-side-window buffer '((side . right) (window-width . 0.5)))
    (set-window-dedicated-p (get-buffer-window buffer) t)
    (with-current-buffer buffer
      (org-link-open-from-string source)
      (org-mode)
      (goto-char (point-min)))))

(defun memo-buffer-undo-refresh ()
  "Refresh buffer undo list to stop previce change."
  (buffer-disable-undo)
  (buffer-enable-undo))

(defun memo-save-buffer ()
  "Save current buffer content and annotation to db."
  (let ((result (memo-update-current-note-content)))
    (if result
        (memo-annotation-overlays-save-batch)
      nil)))

(defconst memo-posframe-edit-buffer-name "*memo-content-input*")

(defun memo-posframe-edit-window-create (buffer &optional content)
  "Create posframe for edit, when exit need recursive-edit-exit."
  (when (posframe-workable-p)
    (posframe-show buffer
		   :string content
		   :border-color "#ee7b29"
		   :border-width 2
		   :poshandler 'posframe-poshandler-frame-center
		   :height (round(* (frame-height) 0.90))
		   :width (round(* (frame-width) 0.75))
		   :override-parameters '((cursor-type box))
		   :respect-header-line t
		   :accept-focus t
		   )
      (setq-local cursor-type t)
      (setq-local cursor-in-non-selected-windows 'box)))

(defun memo-get-content-from-posframe (&optional content)
  "Show posframe for edit."
  (interactive)
  (let ((memo--temp-content nil)
	(temp-buffer (generate-new-buffer memo-posframe-edit-buffer-name)))
    (with-current-buffer temp-buffer
      (org-mode)
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c C-c")
          (lambda ()
            (interactive)
            (let ((content (buffer-substring-no-properties (point-min) (point-max))))
              (setq memo--temp-content content)
              (exit-recursive-edit))))
        (define-key map (kbd "C-c C-k")
          (lambda ()
            (interactive)
            (setq memo--temp-content nil)
            (exit-recursive-edit)))
        (use-local-map map))
      (setq-local header-line-format "Use C-c C-c to commit, C-c C-k to exit.")
      (memo-posframe-edit-window-create temp-buffer content)
      (recursive-edit)
      (posframe-delete temp-buffer))
   memo--temp-content))


(defun memo-set-local-header-line-face ()
  "设置当前缓冲区的 header-line face 样式。"
  (interactive)
  (setq-local face-remapping-alist
              (append face-remapping-alist
                      '((header-line :height 1.5
                                     :background "#333"
                                     :foreground "#ddd"
                                     :box (:line-width 3 :color "#444"))))))


;;;------------------------------------------------------------------
;;; note find relative function.
;;;------------------------------------------------------------------
(defvar memo-next-note-query '["filter:dueBefore:0" "order:random"]
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
  (memo-api--get-note memo-next-note-query))

(defun memo-read-next-note ()
  "Open note in buffer and open side window."
  (interactive)
  (memo-treemacs-display-note-context (memo-query-next-note)))

(provide 'memo-buffer)
;;; memo-buffer.el ends here
