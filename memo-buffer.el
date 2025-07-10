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

;; review releated var and function
(defconst memo--review-buffer-name "*memo-review*"
  "The memo buffer for review note show and flip.")

(defconst memo--source-buffer-name "*memo-source*"
  "The memo buffer for review note show and flip.")

(defvar-local memo-buffer--local-note nil
  "The memo note object store in current buffer.")

(defvar memo-buffer--review-frame nil
  "The var store memo review frame.")
(defun memo-buffer-undo-refresh ()
  "Refresh buffer undo list to stop previous change."
  (buffer-disable-undo)
  (buffer-enable-undo))

(defun memo-buffer-save-buffer ()
  "Save current buffer content and annotation to db."
  (-if-let* ((result (memo-buffer-update-note-content)))
      (progn (set-buffer-modified-p nil)
	     (memo-annotation-overlays-save-batch))
      nil))


;;;--------------------------------------------------------------------------
;;; review note status display
;;;--------------------------------------------------------------------------
(defvar memo-buffer--note-status-content nil
  "The note status content displayed in float overlay.")

;; 状态字段样式配置
(defgroup memo-buffer-status nil
  "Customization group for memo-buffer status field faces."
  :group 'memo)

(defface memo-buffer-status-new-card-face
  '((t :background "#4f8cff" :foreground "#000000" :weight bold))
  "Face for New Card status."
  :group 'memo-buffer-status)

(defface memo-buffer-status-need-review-face
  '((t :background "#ff4f4f" :foreground "#000000" :weight bold))
  "Face for Need Review status."
  :group 'memo-buffer-status)

(defface memo-buffer-status-reviewed-face
  '((t :background "#4fff4f" :foreground "#000000" :weight bold))
  "Face for Reviewed status."
  :group 'memo-buffer-status)

(defface memo-buffer-status-suspend-face
  '((t :background "#ee7b29" :foreground "#000000" :weight bold))
  "Face for Suspend status."
  :group 'memo-buffer-status)

(defvar memo-buffer-status--field-style-alist
  '((new-card    . memo-buffer-status-new-card-face)
    (need-review . memo-buffer-status-need-review-face)
    (reviewed    . memo-buffer-status-reviewed-face)
    (suspend     . memo-buffer-status-suspend-face))
  "Alist mapping status field type to face symbol.")

(defun memo-buffer--get-status-field (note)
  "Return a plist: (:label LABEL :face FACE) for the first status field of NOTE."
  (cond
   ((equal (memo-note-scheduledtype note) "suspend")
    (list :label "Suspend" :face (alist-get 'suspend memo-buffer-status--field-style-alist)))
   ((= (memo-note-state note) 0)
    (list :label "New Card" :face (alist-get 'new-card memo-buffer-status--field-style-alist)))
   ((and (not (= (memo-note-state note) 0)) (memo-note-needreview note))
    (list :label "Need Review" :face (alist-get 'need-review memo-buffer-status--field-style-alist)))
   ((and (not (= (memo-note-state note) 0)) (not (memo-note-needreview note)))
    (list :label "Reviewed" :face (alist-get 'reviewed memo-buffer-status--field-style-alist)))
   (t (list :label "Unknown" :face 'default))))

(defun memo-buffer--format-status-fields (fields)
  "Format a list of FIELDS (each a plist with :label and :face) into a single propertized string."
  (mapconcat (lambda (field)
               (propertize (plist-get field :label) 'face (plist-get field :face)))
             fields
             "    "))

(defun memo-buffer--generate-note-status-content (note-object)
  "Generate note status content by NOTE-OBJECT. Set `memo-buffer--note-status-content'."
  (when (memo-note-p note-object)
    (let* ((fields (list (memo-buffer--get-status-field note-object)))
           ;; 这里可后续扩展更多字段
           (content (concat (memo-buffer--format-status-fields fields) "\n")))
      (setq memo-buffer--note-status-content content)
      content)))

(defvar memo-buffer--note-status-float-overlay nil
  "The overlay to display note review info.")

(defun memo-buffer--create-float-overlay (note-object)
  "Create float overlay and display CONTENT in float overlay.
If CONTENT is nil, generate it by the NOTE-OBJECT."
  (when memo-buffer--note-status-float-overlay
    (delete-overlay memo-buffer--note-status-float-overlay))
  (let* ((status-content (and note-object (memo-buffer--generate-note-status-content note-object))))
    (save-excursion
      (goto-char (window-start))
      (setq memo-buffer--note-status-float-overlay (make-overlay (point) (point)))
      (overlay-put memo-buffer--note-status-float-overlay 'before-string status-content)
      (overlay-put memo-buffer--note-status-float-overlay 'window (selected-window)))))

;; 在滚动时更新位置
(defun memo-buffer--update-float-overlay-content ()
  "Update float overlay content."
  (when (and memo-buffer--note-status-float-overlay
	     (ov-buf memo-buffer--note-status-float-overlay))
    (overlay-put memo-buffer--note-status-float-overlay 'before-string nil)
    (overlay-put memo-buffer--note-status-float-overlay 'after-string nil)
    (overlay-put memo-buffer--note-status-float-overlay 'display nil)
    (overlay-put memo-buffer--note-status-float-overlay 'before-string memo-buffer--note-status-content)))

;; 在滚动时更新位置
(defun memo-buffer--update-float-overlay-position ()
  "Update overlay position when scroll window."
  (when (and memo-buffer--note-status-float-overlay
	     (ov-buf memo-buffer--note-status-float-overlay)
	     (eq (current-buffer) (ov-buf memo-buffer--note-status-float-overlay)))
    (move-overlay memo-buffer--note-status-float-overlay (window-start) (window-start))))

(defun memo-buffer--window-scroll-handler (win start)
  "Window scroll handler function by WIN START."
  (if (eq (current-buffer) (window-buffer win))
    (memo-buffer--update-float-overlay-position)))

(add-hook 'window-scroll-functions 'memo-buffer--window-scroll-handler)
;(remove-hook 'window-scroll-functions 'memo-buffer--window-scroll-handler)
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
  "设置当前缓冲区的 header-line face 样式."
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
      (memo-buffer--create-float-overlay note)
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


;;;------------------------------------------------------------------
;;; jump function in memo buffer.
;;;------------------------------------------------------------------
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

(provide 'memo-buffer)
;;; memo-buffer.el ends here
