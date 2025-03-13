;;; memo-treemacs.el --- Use for memo (interactive "P") -*- lexical-binding: t; -*-
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
;; Borrow code from lsp-treemacs.
;;; Code:


(require 'treemacs)
(require 'treemacs-treelib)

(defvar-local memo-treemacs--right-click-actions nil)
(defvar-local memo-treemacs-generic-filter nil)

(defconst memo-review-note-virtual-tree "VirtHeadTree")

(declare-function memo-treemacs--set-mode-line-format "memo-treemacs.el")

(defcustom memo-treemacs-virtual-head-expand-depth nil
  "Automatic expansion depth for `memo-treemacs-virtual-heads'."
  :type 'number
  :group 'memo-treemacs)

(defun memo-treemacs--set-mode-line-format (buffer title)
  "Set the mode line format of BUFFER to TITLE.
This function sets the `mode-name' or `mode-line-format'
depending on if a custom mode line is detected."
  (with-current-buffer buffer
    (cond ((or (fboundp 'spaceline-install)
               (memq 'moody-mode-line-buffer-identification
                     (default-value 'mode-line-format))
               (and (fboundp 'doom-modeline)
                    (fboundp 'doom-modeline-def-modeline)))
           (setq mode-name title))
          (t
           (setq mode-line-format title)))))


(defvar memo-treemacs-virtual-head-position-params
  `((side . ,treemacs-position)
    (slot . 1)
    (window-width . ,treemacs-width))
  "The params which will be used by
  `display-buffer-in-side-window' in
  `memo-treemacs-virtual-heads'.")


(defconst memo-treemacs-buffer-name "*MemoTree*"
  "Memo Buffer name store the tree.")

(defun memo-resolve-value (value)
  "Resolve VALUE's value.
If it is function - call it.
If it is a variable - return it's value
Otherwise returns value itself."
  (cond
   ((functionp value) (funcall value))
   ((and (symbolp value) (boundp value)) (symbol-value value))
   (value)))

(defmacro memo-treemacs-wcb-unless-killed (buffer &rest body)
  "`with-current-buffer' unless buffer killed."
  (declare (indent 1) (debug t))
  `(when (buffer-live-p (get-buffer ,buffer))
     (with-current-buffer ,buffer
       ,@body)))

(defun memo-treemacs-refresh (&optional _cache)
  "Update note under Treemacs."
  (condition-case _err
      (let ((inhibit-read-only t))
        (when-let ((buf (get-buffer memo-treemacs-buffer-name)))
	  (with-current-buffer buf
            (treemacs-update-async-node '("memo-treemacs-root" "VirtHeadTree") buf)
	    (display-buffer-in-side-window buf memo-treemacs-virtual-head-position-params))))
    (error)))

(defun memo-treemacs-generic-right-click (event)
  (interactive "e")
  (let* ((ec (event-start event))
         (p1 (posn-point ec))
         (w1 (posn-window ec)))
    (select-window w1)
    (goto-char p1)
    (hl-line-highlight)
    (run-with-idle-timer
     0.001 nil
     (lambda ()
       (-when-let* ((actions (if-let (node (treemacs-node-at-point))
                                 (memo-resolve-value (plist-get (button-get node :item) :actions))
                               memo-treemacs--right-click-actions))
                    (menu (easy-menu-create-menu nil actions))
                    (choice (x-popup-menu event menu)))
         (when choice (call-interactively (lookup-key menu (apply 'vector choice))))
         (hl-line-highlight))))))

(defvar memo-treemacs-generic-map
  (-doto (make-sparse-keymap)
    (define-key [mouse-1]  #'treemacs-TAB-action)
    (define-key [double-mouse-1] #'treemacs-RET-action)
    (define-key [mouse-3]  #'memo-treemacs-generic-right-click))
  "Keymap for `memo-treemacs-generic-mode'")

(define-minor-mode memo-treemacs-generic-mode "Treemacs generic mode."
  :keymap memo-treemacs-generic-map)

(defun memo-treemacs--generic-icon (item expanded?)
  "Get the symbol for the the kind."
  (concat
   (if (or (plist-get item :children)
           (plist-get item :children-async))
       (if expanded?  "▾ " "▸ ")
     "  ")
   (or (plist-get item :icon-literal)
       (-if-let (icon (plist-get item :icon))
           (treemacs-get-icon-value
            icon
            nil
            memo-treemacs-theme)
         "   "))))

(defun memo-treemacs-perform-ret-action (&rest _)
  (interactive)
  (if-let (item (-> (treemacs-node-at-point)
                      (button-get :item)))
      (memo-open-head-in-view-buffer item)
    (treemacs-pulse-on-failure "No Child head Found.")))

(treemacs-define-expandable-node-type memo-treemacs-virtual-node
  :closed-icon "• "
  :open-icon   "- "
  :label
  (if (equal (memo-note-expandable item) 1)
      (propertize (memo-note-title item) 'face 'font-lock-string-face)
    (propertize (memo-note-title item) 'face 'font-lock-variable-name-face))
  :key (memo-note-id item)
  :ret-action #'memo-treemacs-perform-ret-action
  :children
  (when (equal (memo-note-expandable item) 1)
    (let ((items (memo-api--get-virt-heads-by-parentid (memo-note-id item))))
      (funcall callback items)))
  :child-type
  'memo-treemacs-virtual-node
  :more-properties
  (if (equal (memo-note-expandable item) 0)
      `(:note ,item :leaf t :no-tab? t)
    `(:note ,item))
  :async? t)

(treemacs-define-expandable-node-type memo-treemacs-generic-node
  :closed-icon "--"
  :open-icon   "--"
  :label "--------------------------"
  :key item
  :children
  (when (equal item "VirtHeadTree")
    (let ((items  (list (memo-get-review-note))))
      (funcall callback items)))
  :child-type
  'memo-treemacs-virtual-node
  :async? t)


(treemacs-define-variadic-entry-node-type memo-treemacs-root
  :key  "memo-treemacs-root"
  :children '("VirtHeadTree")
  :child-type 'memo-treemacs-generic-node)

(defun memo-treemacs-render (title expand-depth
                                 &optional buffer-name right-click-actions _clear-cache?)
  (let ((buffer (get-buffer-create (or buffer-name memo-treemacs-buffer-name))))
    (with-current-buffer buffer
      (treemacs-initialize memo-treemacs-root
        :with-expand-depth (or expand-depth 0)
        :and-do (progn
                  (memo-treemacs--set-mode-line-format buffer title)
                  (setq-local face-remapping-alist '((button . default)))
                  (setq-local treemacs-default-visit-action 'treemacs-RET-action)
                  (setq-local memo-treemacs--right-click-actions right-click-actions)
                  (setq-local window-size-fixed nil)
                  (setq-local treemacs--width-is-locked nil)
                  (setq-local treemacs-space-between-root-nodes nil)
                  (when treemacs-text-scale
                    (text-scale-increase treemacs-text-scale))
                  (memo-treemacs-generic-mode t)))
      (current-buffer))))

(defun memo-treemacs-initialize ()
  "Display heads in treemacs."
  (interactive)
  (display-buffer-in-side-window
   (memo-treemacs-render
    "*MemoTree*" memo-treemacs-virtual-head-expand-depth)
   memo-treemacs-virtual-head-position-params))

(defun memo-treemacs-update ()
  "Update treemacs buffer, create buffer and init if buffer not exist.
Update node in buffer if buffer exist."
  (let ((buffer (get-buffer memo-treemacs-buffer-name)))
    (if buffer (memo-treemacs-refresh)
      (memo-treemacs-initialize))))

;(treemacs-define-entry-node-type show-virtual-children-heads-entry
;  :key 'show-virtual-children-heads-entry
;  :label (propertize "Children Note" 'face 'font-lock-keyword-face)
;  :open-icon (treemacs-get-icon-value 'list)
;  :closed-icon (treemacs-get-icon-value 'list)
;  :children (memo-api--get-virt-heads-by-parentid (memo-get-current-note-id))
;  :more-properties nil
;  :child-type 'show-virtual-children-heads)
;
;(treemacs-enable-top-level-extension
; :extension 'show-virtual-children-heads-entry
; :position 'top)


(provide 'memo-treemacs)
;;; memo-treemacs.el ends here
