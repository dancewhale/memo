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

(defconst memo--virt-buffer-name "*memo-virt*"
  "The memo buffer for view child virtual head.")

(defconst memo--read-buffer-name "*memo-read*"
  "The memo buffer for read resource.")

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
    (setq write-contents-functions '(memo-update-current-note-content))
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

(defun memo-open-head-in-read-buffer (head path buffer)
  "Open HEAD in view buffer, PATH is local var for update node in BUFFER."
  (if (not (memo-note-id head))
      (user-error "Memo-note object is nil"))
  (let* ((buf (get-buffer-create memo--read-buffer-name)))
    (pop-to-buffer buf)
    (with-current-buffer buf
      (erase-buffer)
      (insert (memo-api--get-content-byid (memo-note-id head)))
      (set-buffer-modified-p nil)
      (org-mode)
      (setq-local header-line-format (memo-note-title head))
      (memo-set-local-header-line-face)
      (goto-char (point-min))
      (setq memo--buffer-local-note head)
      (setq memo--buffer-local-note-path path)
      (setq memo--buffer-local-note-buffer buffer)
      (setq write-contents-functions '(memo-update-current-note-content)))))

(defun memo-open-file-from-treemacs (file)
  "Open FILE in view buffer and jump to TITLE."
  (if (not (memo-file-filepath file))
      (user-error "Memo-file object is nil"))
  (let ((filepath (memo-file-filepath file)))
    (find-file filepath)))

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
"Open an org-mode LINK in a new buffer on the right side."
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

;; annotation head
(defun memo-get-content-from-input-buffer (&optional default)
  "Get content from a temporary input buffer with org-mode.
DEFAULT is the initial content to insert in the buffer.
Return the content entered by the user."
  (let ((memo--window-config (current-window-configuration))
        (memo--temp-content nil)
        (temp-buffer (generate-new-buffer "*memo-content-input*")))
    (with-current-buffer temp-buffer
      (org-mode)
      (when default
        (insert default))
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c C-c")
          (lambda ()
            (interactive)
            (let ((content (buffer-substring-no-properties (point-min) (point-max))))
              (kill-buffer)
              (set-window-configuration memo--window-config)
              (setq memo--temp-content content)
              (exit-recursive-edit))))
        (use-local-map map))
      (switch-to-buffer-other-window temp-buffer)
      (message "Enter content, then press C-c C-c to confirm")
      (recursive-edit))
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

;;;-------------------------------------------------
;;  treemacs action
;;;-------------------------------------------------
(defun memo-treemacs-read-next-note ()
  "Read Next node."
  (interactive)
  (unless (equal (buffer-name (current-buffer)) memo--read-buffer-name)
    (user-error "You should execute this command in buffer *memo-read*")
    (let* ((memo-treemacs-buffer (get-buffer memo-treemacs-buffer-name)))
      ())))





(provide 'memo-buffer)
;;; memo-buffer.el ends here
