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
(require 'olivetti)
(require 'f)

;; review releat var and function
(defconst memo--review-buffer-name "*memo-review*"
  "The memo buffer for review note show and flip.")

(defconst memo--source-buffer-name "*memo-source*"
  "The memo buffer for review note show and flip.")

(defvar-local memo-buffer--local-note nil
  "The memo note object store in current buffer.")

(defvar memo-buffer--review-frame nil
  "The var store memo review frame.")

;; jump to org and enable editor.
(defun memo-buffer-goto-org ()
  "Jump to source point from review buffer."
  (interactive)
  (let* ((id  (memo-note-id memo-buffer--local-note))
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
(defun memo-buffer-goto-source-direct ()
  "Jump to source of node."
  (interactive)
  (if (not (memo-note-p memo-buffer--local-note))
      (user-error "Review memo-note object is nil"))
  (let* ((source (memo-note-source memo-buffer--local-note))
	 (buf (get-buffer-create memo--source-buffer-name)))
    (save-excursion
      (with-current-buffer buf
	(org-link-open-from-string source)))))

;; wait for use.
(defun memo-buffer-goto-source ()
  "Open an ORG-MODE LINK in a new buffer on the right side."
  (interactive)
  (if (not (memo-note-p memo-buffer--local-note))
      (user-error "Local buffer memo-note object is nil"))
  (let ((source (memo-note-source memo-buffer--local-note))
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

(defun memo-buffer-save-buffer ()
  "Save current buffer content and annotation to db."
  (-if-let* ((result (memo-buffer-update-note-content)))
      (progn (set-buffer-modified-p nil)
	     (memo-annotation-overlays-save-batch))
      nil))

;;;--------------------------------------------------------------------------
;;; posframe relative function and variable
;;;--------------------------------------------------------------------------

(defvar memo-buffer--posframe-init-phase 0
"When 1 mean posframe is in init phase.")

(defconst memo-buffer--posframe-buffer-name "*memo-content-input*")

(defvar memo-buffer--posframe-frame nil
  "The memo posframe for edit.")

(defun memo-buffer--posframe-edit-window-create (buffer &optional content)
  "Create posframe for BUFFER with default CONTENT.
when exit need recursive-edit-exit.
Return the created frame."
  (when (posframe-workable-p)
    (let ((frame (posframe-show buffer
                                :string content
                                :border-color "#ee7b29"
                                :border-width 2
                                :poshandler 'posframe-poshandler-frame-center
                                :height (round(* (frame-height) 0.50))
                                :width (round(* (frame-width) 0.50))
                                :override-parameters '((cursor-type t))
                                :respect-header-line t
				:min-height 2
                                :accept-focus t)))
      (with-selected-frame frame
        (setq-local cursor-type t)
        (setq-local cursor-in-non-selected-windows 'nil))
      (setq memo-buffer--posframe-frame frame)
      (setq memo-buffer--posframe-init-phase 1)
      (select-frame-set-input-focus memo-buffer--posframe-frame)
     frame)))

(defun memo-buffer--posframe-hide-window (buffer-name)
  "Hiden memo posframe by BUFFER-NAME."
  (when  (and (buffer-live-p (get-buffer buffer-name))
	      (frame-live-p memo-buffer--posframe-frame))
    (posframe-delete memo-buffer--posframe-buffer-name)))

(defun memo-buffer--posframe-hidehandler-when-window-switch (window)
  "MEMO Posframe hidehandler function with WINDOW.
This function hide posframe and clear temp-content when user switch window."
  (-if-let* ((posframe-buffer (get-buffer memo-buffer--posframe-buffer-name))
	     (cubuffer (current-buffer))
	     (cuwindow (selected-window))
	     (cuframe (selected-frame)))
      (if (and (buffer-live-p posframe-buffer)
	       (frame-live-p memo-buffer--posframe-frame)
	       (frame-visible-p memo-buffer--posframe-frame)
	       (not (minibufferp))
	       (not (eq memo-buffer--posframe-frame cuframe)))
	  (if (< memo-buffer--posframe-init-phase 1)
	      (progn (with-current-buffer posframe-buffer
		       (message "Start Exit posframe:")
		       (setq memo--temp-content nil)
		       (exit-recursive-edit)))
	    (setq memo-buffer--posframe-init-phase (- memo-buffer--posframe-init-phase 1))))))


(defun memo-buffer-get-content-from-posframe (&optional content)
  "Show posframe for edit with optional CONTENT."
  (interactive)
  (let ((posframe-buffer (get-buffer-create memo-buffer--posframe-buffer-name)))
    (setq memo-buffer--review-frame (selected-frame))
    (with-current-buffer posframe-buffer
      (org-mode)
      (setq-local header-line-format "Use C-c C-c to commit, C-c C-k to exit.")
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

      (if (frame-live-p (memo-buffer--posframe-edit-window-create posframe-buffer content))
          (unwind-protect
              (progn
                ;; Add hook just before recursive-edit, make it buffer-local
                (add-hook 'window-selection-change-functions #'memo-buffer--posframe-hidehandler-when-window-switch 0 t)
                (recursive-edit))
            ;; Ensure hook is removed even if recursive-edit exits abnormally, make it buffer-local
            (remove-hook 'window-selection-change-functions #'memo-buffer--posframe-hidehandler-when-window-switch t)
            (memo-buffer--posframe-hide-window memo-buffer--posframe-buffer-name)
	    (select-frame-set-input-focus memo-buffer--review-frame))
	(message "Posframe could not be created.")))
    memo--temp-content))


(defun memo-buffer-set-local-header-line-face ()
  "设置当前缓冲区的 header-line face 样式。"
  (interactive)
  (setq-local face-remapping-alist
              (append face-remapping-alist
                      '((header-line :height 1.5
                                     :background "#333"
                                     :foreground "#ddd"
                                     :box (:line-width 3 :color "#444"))))))

;;;------------------------------------------------------------------
;;; virt note operator function.
;;;------------------------------------------------------------------
(defun memo-buffer-open-note (note)
  "Open NOTE in view buffer."
  (if (not (memo-note-id note))
      (user-error "Memo-note object is nil"))
  (let* ((buf (get-buffer-create memo--review-buffer-name)))
    (pop-to-buffer buf)
    (with-current-buffer buf
      (erase-buffer)
      (insert (memo-api--get-content-byid (memo-note-id note)))
      (memo-buffer-undo-refresh)
      (set-buffer-modified-p nil)
      (org-mode)
      (setq-local header-line-format (memo-note-title note))
      (memo-buffer-set-local-header-line-face)
      (goto-char (point-min))
      (setq memo-buffer--local-note note)
      (olivetti-mode)
      (memo-annotation-mode)
      (setq write-contents-functions '(memo-buffer-save-buffer)))))


(defun memo-buffer-create-child-virt-note ()
  "Create the virt child note for current review note.
If a region is active, its content is used as initial content."
  (interactive)
  (let ((initial-content (when (use-region-p)
                           (buffer-substring-no-properties
                            (region-beginning) (region-end))))
        content note id title)
    (-if-let* ((content (memo-buffer-get-content-from-posframe initial-content))
               (note memo-buffer--local-note)
               (id (memo-note-id memo-buffer--local-note))
               (title (memo-first-nonblank-chars content  20)))
        (memo-api--create-virt-head id title content))
    (memo-treemacs-note-buffer-update-node (memo-note-path memo-buffer--local-note))))

(defun memo-buffer-update-note-title ()
  "Update the title of current note which opened."
  (interactive)
  (-if-let* ((note memo-buffer--local-note)
	     (title (memo-note-title note))
	     (path (memo-note-path note))
	     (id (memo-note-id note)))
      (-if-let* ((title (memo-buffer-get-content-from-posframe title)))
	  (progn (memo-api--update-title id title)
		 (setq-local header-line-format title)
		 (setf (memo-note-title memo-buffer--local-note) title)
		 (memo-treemacs-note-buffer-update-node path)
		 (memo-treemacs-note-buffer-goto-node note)))))

(defun memo-buffer-update-note-content ()
  "Update the content of current note which opened."
  (interactive)
  (-if-let* ((note memo-buffer--local-note)
	     (id (memo-note-id note))
	     (content (buffer-substring-no-properties
			(point-min) (point-max))))
      (progn (memo-api--update-content id content))))

(provide 'memo-buffer)
;;; memo-buffer.el ends here
