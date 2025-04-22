;;; memo-annotation-region.el --- Region adjustment for memo annotations -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Dancewhale

;; Author: Dancewhale <542727233@qq.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; This package provides functions to adjust the region of memo annotations
;; interactively using keyboard shortcuts.
;;
;;; Code:

(require 'ov)
(require 'memo-annotation)

;;; Variables for region adjustment mode

(defvar memo-annotation-region--active-overlay nil
  "Currently active overlay being adjusted.")

(defvar memo-annotation-region--original-overlay-props nil
  "Original properties of the active overlay being adjusted.")

(defvar memo-annotation-region--highlight-face '(:background "#ffaa55" :foreground "black")
  "Face to use for highlighting during region adjustment.")

(defvar memo-annotation-region--cursor-type cursor-type
  "Store original cursor type during region adjustment.")

(defvar memo-annotation-region--evil-original-cursor nil
  "Store original evil cursor type during region adjustment.")

(defvar memo-annotation-region-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<right>") #'memo-annotation-region-expand-right)
    (define-key map (kbd "<left>") #'memo-annotation-region-expand-left)
    (define-key map (kbd "S-<right>") #'memo-annotation-region-shrink-right)
    (define-key map (kbd "S-<left>") #'memo-annotation-region-shrink-left)
    (define-key map (kbd "<up>") #'memo-annotation-region-expand-up)
    (define-key map (kbd "<down>") #'memo-annotation-region-expand-down)
    (define-key map (kbd "S-<up>") #'memo-annotation-region-shrink-up)
    (define-key map (kbd "S-<down>") #'memo-annotation-region-shrink-down)
    (define-key map (kbd "RET") #'memo-annotation-region-confirm)
    (define-key map (kbd "<escape>") #'memo-annotation-region-cancel)
    map)
  "Keymap for memo annotation region adjustment mode (Emacs state).")


;;; Helper function to check Evil state

(defun memo-annotation-region--evil-normal-state-p ()
  "Return non-nil if current buffer is in Evil normal state."
  (and (boundp 'evil-local-mode)
       evil-local-mode
       (boundp 'evil-state) ; Ensure evil-state is bound
       (eq evil-state 'normal)))

;;; Core functions for finding and selecting overlays

(defun memo-annotation-region--get-overlay-at-point ()
  "Get memo annotation overlay at point.
Returns the first overlay found with the memo-annotation property."
  (let ((overlays-at-point (overlays-at (point))))
    (cl-find-if (lambda (ov) (overlay-get ov 'memo-annotation))
                overlays-at-point)))

(defun memo-annotation-region--store-original-properties (overlay)
  "Store original properties of OVERLAY for later restoration."
  (list
   :beg (overlay-start overlay)
   :end (overlay-end overlay)
   :face (overlay-get overlay 'face)))

(defun memo-annotation-region--highlight-overlay (overlay)
  "Apply highlight face to OVERLAY for visual feedback during adjustment."
  (overlay-put overlay 'face memo-annotation-region--highlight-face))

;;; Region manipulation functions

(defun memo-annotation-region-expand-right ()
  "Expand the active annotation region one character to the right."
  (interactive)
  (when memo-annotation-region--active-overlay
    (let* ((overlay memo-annotation-region--active-overlay)
           (end (overlay-end overlay))
           (new-end (min (1+ end) (point-max))))
      (move-overlay overlay (overlay-start overlay) new-end))))

(defun memo-annotation-region-expand-left ()
  "Expand the active annotation region one character to the left."
  (interactive)
  (when memo-annotation-region--active-overlay
    (let* ((overlay memo-annotation-region--active-overlay)
           (start (overlay-start overlay))
           (new-start (max (1- start) (point-min))))
      (move-overlay overlay new-start (overlay-end overlay)))))

(defun memo-annotation-region-shrink-left ()
  "Shrink the active annotation region one character from the left."
  (interactive)
  (when memo-annotation-region--active-overlay
    (let* ((overlay memo-annotation-region--active-overlay)
           (start (overlay-start overlay))
           (end (overlay-end overlay))
           (new-end (max (1- end) (1+ start))))  ; Ensure at least 1 char width
      (move-overlay overlay start new-end))))

(defun memo-annotation-region-shrink-right ()
  "Shrink the active annotation region one character from the right."
  (interactive)
  (when memo-annotation-region--active-overlay
    (let* ((overlay memo-annotation-region--active-overlay)
           (start (overlay-start overlay))
           (end (overlay-end overlay))
           (new-start (min (1+ start) (1- end))))  ; Ensure at least 1 char width
      (move-overlay overlay new-start end))))

;;; Line-based region manipulation functions

(defun memo-annotation-region--line-beginning-position (pos)
  "Get the beginning position of the line containing POS."
  (save-excursion
    (goto-char pos)
    (line-beginning-position)))

(defun memo-annotation-region--line-end-position (pos)
  "Get the end position of the line containing POS."
  (save-excursion
    (goto-char pos)
    (line-end-position)))

(defun memo-annotation-region--prev-line-beginning-position (pos)
  "Get the beginning position of the line before the one containing POS."
  (save-excursion
    (goto-char pos)
    (forward-line -1)
    (line-beginning-position)))

(defun memo-annotation-region--prev-line-end-position (pos)
  "Get the end position of the line before the one containing POS."
  (save-excursion
    (goto-char pos)
    (forward-line -1)
    (line-end-position)))

(defun memo-annotation-region--next-line-beginning-position (pos)
  "Get the beginning position of the line after the one containing POS."
  (save-excursion
    (goto-char pos)
    (forward-line 1)
    (line-beginning-position)))

(defun memo-annotation-region--next-line-end-position (pos)
  "Get the end position of the line after the one containing POS."
  (save-excursion
    (goto-char pos)
    (forward-line 1)
    (line-end-position)))

(defun memo-annotation-region-expand-up ()
  "Expand the active annotation region to include the line above."
  (interactive)
  (when memo-annotation-region--active-overlay
    (let* ((overlay memo-annotation-region--active-overlay)
           (start (overlay-start overlay))
           (current-line-start (memo-annotation-region--line-beginning-position start))
           (prev-line-start (memo-annotation-region--prev-line-beginning-position start))
           (new-start (max prev-line-start (point-min))))
      (move-overlay overlay new-start (overlay-end overlay)))))

(defun memo-annotation-region-expand-down ()
  "Expand the active annotation region to include the line below."
  (interactive)
  (when memo-annotation-region--active-overlay
    (let* ((overlay memo-annotation-region--active-overlay)
           (end (overlay-end overlay))
           (current-line-end (memo-annotation-region--line-end-position end))
           (next-line-end (memo-annotation-region--next-line-end-position end))
           (new-end (min next-line-end (point-max))))
      (move-overlay overlay (overlay-start overlay) new-end))))

(defun memo-annotation-region-shrink-up ()
  "Shrink the active annotation region by removing the top line."
  (interactive)
  (when memo-annotation-region--active-overlay
    (let* ((overlay memo-annotation-region--active-overlay)
           (start (overlay-start overlay))
           (end (overlay-end overlay))
           (current-line-start (memo-annotation-region--line-beginning-position start))
           (next-line-start (memo-annotation-region--next-line-beginning-position start)))
      ;; Only shrink if we have at least two lines in the region
      (when (< next-line-start end)
        (move-overlay overlay next-line-start end)))))

(defun memo-annotation-region-shrink-down ()
  "Shrink the active annotation region by removing the bottom line."
  (interactive)
  (when memo-annotation-region--active-overlay
    (let* ((overlay memo-annotation-region--active-overlay)
           (start (overlay-start overlay))
           (end (overlay-end overlay))
           (current-line-end (memo-annotation-region--line-end-position end))
           (prev-line-end (memo-annotation-region--prev-line-end-position end)))
      ;; Only shrink if we have at least two lines in the region
      (when (> prev-line-end start)
        (move-overlay overlay start prev-line-end)))))

(defun memo-annotation-region--is-multiline-p (overlay)
  "Check if OVERLAY spans multiple lines."
  (let ((start (overlay-start overlay))
        (end (overlay-end overlay)))
    (save-excursion
      (goto-char start)
      (< (line-end-position) end))))

(defun memo-annotation-region--get-region-info (overlay)
  "Get formatted information about the region covered by OVERLAY."
  (let ((start (overlay-start overlay))
        (end (overlay-end overlay))
        (text (buffer-substring-no-properties
               (overlay-start overlay)
               (overlay-end overlay))))
    (format "Region: %d-%d (%d chars)%s\nText: %s"
            start end
            (- end start)
            (if (memo-annotation-region--is-multiline-p overlay)
                (format ", %d lines"
                        (count-lines start end))
              "")
            (if (> (length text) 30)
                (concat (substring text 0 27) "...")
              text))))

;; 修改原有函数，在调整时显示更多信息
(defun memo-annotation-adjust-region-at-point ()
  "Start adjusting the region of memo annotation at point."
  (interactive)
  (let ((overlay (memo-annotation-region--get-overlay-at-point)))
    (if overlay
        (progn
          ;; Store the overlay and its original properties
          (setq memo-annotation-region--active-overlay overlay)
          (setq memo-annotation-region--original-overlay-props
                (memo-annotation-region--store-original-properties overlay))

          ;; Highlight the overlay for adjustment
          (memo-annotation-region--highlight-overlay overlay)

          ;; Enable region adjustment mode
          (memo-annotation-region-mode 1)
          (let ((info (memo-annotation-region--get-region-info overlay)))
            (message "Adjusting annotation region. Use arrow keys to resize, Enter to confirm, Esc to cancel\n%s" info)))
      (message "No memo annotation found at point"))))

;; 每次调整区域后更新信息
(defun memo-annotation-region--update-info ()
  "Update region information in minibuffer after adjustment."
  (when memo-annotation-region--active-overlay
    (let ((info (memo-annotation-region--get-region-info
                 memo-annotation-region--active-overlay)))
      (message "Adjusting region: %s" info))))

;; 为所有调整函数添加信息更新
(advice-add 'memo-annotation-region-expand-right :after #'memo-annotation-region--update-info)
(advice-add 'memo-annotation-region-expand-left :after #'memo-annotation-region--update-info)
(advice-add 'memo-annotation-region-shrink-right :after #'memo-annotation-region--update-info)
(advice-add 'memo-annotation-region-shrink-left :after #'memo-annotation-region--update-info)
(advice-add 'memo-annotation-region-expand-up :after #'memo-annotation-region--update-info)
(advice-add 'memo-annotation-region-expand-down :after #'memo-annotation-region--update-info)
(advice-add 'memo-annotation-region-shrink-up :after #'memo-annotation-region--update-info)
(advice-add 'memo-annotation-region-shrink-down :after #'memo-annotation-region--update-info)

;;; Mode control functions

(defun memo-annotation-region--restore-overlay ()
  "Restore the original properties of the active overlay."
  (when (and memo-annotation-region--active-overlay
             memo-annotation-region--original-overlay-props)
    (let ((overlay memo-annotation-region--active-overlay)
          (props memo-annotation-region--original-overlay-props))
      (move-overlay overlay
                    (plist-get props :beg)
                    (plist-get props :end))
      (overlay-put overlay 'face (plist-get props :face)))))

(defun memo-annotation-region--cleanup ()
  "Clean up state after region adjustment is complete."
  ;; Restore cursor based on which one was stored
  (if memo-annotation-region--evil-original-cursor
      (when (boundp 'evil-normal-state-cursor) ; Check if variable exists
        (setq evil-normal-state-cursor memo-annotation-region--evil-original-cursor))
    ;; Restore Emacs cursor type using the buffer-local value stored earlier
    (setq-local cursor-type memo-annotation-region--cursor-type))

  ;; Clear stored values
  (setq memo-annotation-region--active-overlay nil)
  (setq memo-annotation-region--original-overlay-props nil)
  (setq memo-annotation-region--evil-original-cursor nil) ; Reset evil cursor store
  )

(defun memo-annotation-region-confirm ()
  "Confirm the change to the annotation region and update the annotation."
  (interactive)
  (when memo-annotation-region--active-overlay
    (let* ((overlay memo-annotation-region--active-overlay)
           (anno-id (overlay-get overlay 'memo-annotation-id))
           (annotation (memo-annotation--get-by-id anno-id))
           (new-start (overlay-start overlay))
           (new-end (overlay-end overlay))
           (new-text (buffer-substring-no-properties new-start new-end)))
      
      ;; Update annotation with new region
      (when annotation
        (setf (memo-annotation-start annotation) new-start)
        (setf (memo-annotation-end annotation) new-end)
        (setf (memo-annotation-srctext annotation) new-text)
        
        ;; Update face to original (not highlight face)
        (let ((orig-face (plist-get memo-annotation-region--original-overlay-props :face)))
          (overlay-put overlay 'face orig-face))
        
        ;; Update in database
        (memo-annotation--update-db annotation)
        (message "Annotation region updated"))))
  
  (memo-annotation-region--cleanup)
  (memo-annotation-region-mode -1))

(defun memo-annotation-region-cancel ()
  "Cancel region adjustment and restore original overlay."
  (interactive)
  (memo-annotation-region--restore-overlay)
  (memo-annotation-region--cleanup)
  (memo-annotation-region-mode -1)
  (message "Annotation region adjustment cancelled"))

;;;###autoload
(define-minor-mode memo-annotation-region-mode
  "Minor mode for adjusting memo annotation regions."
  :init-value nil
  :lighter " MemoRegion"
  :keymap memo-annotation-region-mode-map ; Keep standard map for now, can be overridden
  (if memo-annotation-region-mode
      (progn
        (if (memo-annotation-region--evil-normal-state-p)
            (progn
              ;; Handle Evil normal state cursor
              (when (boundp 'evil-normal-state-cursor) ; Check if variable exists
                (setq memo-annotation-region--evil-original-cursor evil-normal-state-cursor)
                ;; Set cursor to hollow or another less intrusive style
                (setq evil-normal-state-cursor '(hollow . 1))))
          (progn
            ;; Handle Emacs state cursor
            ;; Store the original buffer-local cursor type
            (setq memo-annotation-region--cursor-type cursor-type)
            ;; Set the buffer-local cursor type to nil to hide it
            (setq-local cursor-type nil))))
    ;; When disabling the mode, call the cleanup function
    (memo-annotation-region--cleanup)))

(provide 'memo-annotation-region)
;;; memo-annotation-region.el ends here
