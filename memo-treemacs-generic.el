;;; memo-treemacs-generic.el --- Use for memo (interactive "P") -*- lexical-binding: t; -*-
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

(defvar-local memo-treemacs-tree nil)
(defvar-local memo-treemacs--right-click-actions nil)
(defvar-local memo-treemacs-generic-filter nil)

(declare-function memo-treemacs--set-mode-line-format "memo-treemacs.el")

(defmacro memo-treemacs-wcb-unless-killed (buffer &rest body)
  "`with-current-buffer' unless buffer killed."
  (declare (indent 1) (debug t))
  `(when (buffer-live-p (get-buffer ,buffer))
     (with-current-buffer ,buffer
       ,@body)))

(defun memo-treemacs-generic-refresh (&optional _cache)
  (condition-case _err
      (let ((inhibit-read-only t))
        (treemacs-update-node '(memo-treemacs-generic-root)))
    (error)))

(defun memo-treemacs-generic-update (tree)
  (setq memo-treemacs-tree tree)
  (memo-treemacs-generic-refresh))

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

(defun memo-treemacs-filter-if-needed (result)
  (if memo-treemacs-generic-filter
      (funcall memo-treemacs-generic-filter result)
    result))

(defun memo-treemacs-perform-ret-action (&rest _)
  (interactive)
  (if-let (action (-> (treemacs-node-at-point)
                      (button-get :item)
                      (plist-get :ret-action)))
      (funcall-interactively action)
    (treemacs-pulse-on-failure "No ret action defined.")))

(treemacs-define-expandable-node-type memo-treemacs-generic-node
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
    (memo-api--get-virt-heads-by-parentid (memo-note-id item)))
  :child-type
  'memo-treemacs-generic-node
  :more-properties
  (if (equal (memo-note-expandable item) 2)
      `(:note ,item :leaf t)
    `(:note ,item)))

(treemacs-define-variadic-entry-node-type memo-treemacs-generic-root
  :key 'memo-treemacs-generic-root
  :children memo-treemacs-tree
  :child-type 'memo-treemacs-generic-node)

(defun memo-treemacs-render (tree title expand-depth
                                 &optional buffer-name right-click-actions _clear-cache?)
  (let ((buffer (get-buffer-create (or buffer-name "*MEMO Lookup*"))))
    (with-current-buffer buffer
      (treemacs-initialize memo-treemacs-generic-root
        :with-expand-depth (or expand-depth 0)
        :and-do (progn
                  (memo-treemacs--set-mode-line-format buffer title)
                  (setq-local face-remapping-alist '((button . default)))
                  (setq-local memo-treemacs-tree tree)
                  (setq-local treemacs-default-visit-action 'treemacs-RET-action)
                  (setq-local memo-treemacs--right-click-actions right-click-actions)
                  (setq-local window-size-fixed nil)
                  (setq-local treemacs--width-is-locked nil)
                  (setq-local treemacs-space-between-root-nodes nil)
                  (when treemacs-text-scale
                    (text-scale-increase treemacs-text-scale))
                  (memo-treemacs-generic-mode t)))
      (current-buffer))))



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


(provide 'memo-treemacs-generic)
;;; memo-treemacs-generic.el ends here
