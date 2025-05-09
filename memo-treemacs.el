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
(require 'f)

(defvar-local memo-treemacs--right-click-actions nil)
(defvar-local memo-treemacs-generic-filter nil)

(defconst memo-treemacs-root-node-key "variadic-entry-node")

(defun memo-treemacs--concat-path (path)
  "Get the path by append the memo-treemacs-root-node-key to the first of PATH."
  (when (listp path)
    (append (list memo-treemacs-root-node-key) path)))

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


(defvar memo-treemacs-file-position-params
  `((side . ,treemacs-position)
    (slot . 0)
    (window-width . ,treemacs-width))
  "The params which will be used by
  `display-buffer-in-side-window' in
  `memo-treemacs-file'.")

(defvar memo-treemacs-note-position-params
  `((side . ,treemacs-position)
    (slot . 1)
    (window-width . ,treemacs-width))
  "The params which will be used by
  `display-buffer-in-side-window' in
  `memo-treemacs-virtual-note")

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

(defun memo-treemacs-goto ()
  "Goto note node under Treemacs."
  (interactive)
  (condition-case _err
      (-if-let* ((inhibit-read-only t)
		 (path   (memo-note-path memo--buffer-local-note))
	    (id  (memo-note-id memo--buffer-local-note))
	    (buffer memo--buffer-local-note-buffer))
	(with-current-buffer buffer
	  (treemacs-goto-extension-node (append path `(,id)))))
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

(defun memo-treemacs-card-perform-ret-action (&rest _)
  "Open note on other buffer when perform ret in note bottom."
  (interactive)
  (-if-let* ((button-node (treemacs-node-at-point))
	     (item (button-get button-node :item)))
      (memo-buffer-open-note item)
    (treemacs-pulse-on-failure "No Child head Found.")))

(defun memo-treemacs-file-perform-ret-action (&rest _)
  "Open file on other buffer when perform ret in file bottom."
  (interactive)
  (if-let (item (-> (treemacs-node-at-point)
                      (button-get :item)))
      (progn
	(if (not (memo-file-filepath item))
	    (treemacs-pulse-on-failure "File Path not exist."))
	(find-file (memo-file-filepath item)))
    (treemacs-pulse-on-failure "No file Found.")))

;;;----------------------------------
;;;  treemacs tree render for note
;;;----------------------------------
(defconst memo-treemacs-note-buffer-name "*MemoTreeNote"
  "Memo Buffer name store the tree.")

(defun memo-treemacs-note-buffer-update ()
  "Update note node."
  (-if-let* ((inhibit-read-only t)
	     (buffer (get-buffer memo-treemacs-note-buffer-name))
	     (current-note memo--buffer-local-note))
      (with-current-buffer buffer
	(if (not (memo-note-headlineid current-note))
	  (progn
	    (setq file-note (memo-api--get-first-file-head (memo-note-id current-note)))
	    (memo-treemacs-note-render file-note 1))
	  (let* ((path (butlast (memo-note-path current-note)))
		 (path (memo-treemacs--concat-path path)))
	    (treemacs-update-async-node path buffer))))))

(treemacs-define-expandable-node-type memo-treemacs-virt-head-node
  :closed-icon treemacs-icon-tag-closed
  :open-icon   treemacs-icon-tag-open
  :label
  (if (> (memo-note-totalvirtcards item) 0)
      (propertize (memo-note-title item) 'face 'font-lock-string-face)
    (propertize (memo-note-title item) 'face 'font-lock-variable-name-face))
  :key (memo-note-id item)
  :ret-action #'memo-treemacs-card-perform-ret-action
  :children
  (when (> (memo-note-totalvirtcards item) 0)
    (let ((items (memo-api--get-children-virt-head (memo-note-id item))))
      (funcall callback items)))
  :child-type
  'memo-treemacs-virt-head-node
  :more-properties
  (if (= (memo-note-totalvirtcards item) 0)
      `(:note ,item :leaf t :no-tab? t)
    `(:note ,item))
  :async? t)

(treemacs-define-variadic-entry-node-type memo-treemacs-virt-node
  :key  memo-treemacs-root-node-key
  :children `(,memo-local-note)
  :child-type 'memo-treemacs-virt-head-node)


(defun memo-treemacs-note-render (note expand-depth
                                 &optional right-click-actions)
  (let ((buffer (get-buffer-create memo-treemacs-note-buffer-name)))
    (with-current-buffer buffer
      (treemacs-initialize memo-treemacs-virt-node
        :with-expand-depth (or expand-depth 0)
        :and-do (progn
                  (memo-treemacs--set-mode-line-format buffer "memo-virt-note")
                  (setq-local face-remapping-alist '((button . default)))
                  (setq-local treemacs-default-visit-action 'treemacs-RET-action)
                  (setq-local memo-treemacs--right-click-actions right-click-actions)
                  (setq-local window-size-fixed nil)
		  (setq-local memo-local-note note)
                  (setq-local treemacs--width-is-locked nil)
                  (setq-local treemacs-space-between-root-nodes nil)
                  (when treemacs-text-scale
                    (text-scale-increase treemacs-text-scale))
                  (memo-treemacs-generic-mode t)))
      (current-buffer))))

;;;----------------------------------
;;;  treemacs tree render for file
;;;----------------------------------
(defconst memo-treemacs-file-buffer-name "*MemoTreeFile"
  "Memo Buffer name store the tree.")


(treemacs-define-expandable-node-type memo-treemacs-read-head-node
  :closed-icon
  (if (= (memo-note-totalcards item) 1)
      treemacs-icon-tag-leaf  treemacs-icon-tag-closed)
  :open-icon   treemacs-icon-tag-open
  :label
  (cond
   ((equal (memo-note-scheduledtype item) "suspend")
    (propertize (f-base (memo-note-title item)) 'face 'font-lock-warning-face))
   ((= (memo-note-state item) 0)
    (propertize (f-base (memo-note-title item)) 'face 'font-lock-string-face))
   ((memo-note-needreview item)
    (propertize (f-base (memo-note-title item)) 'face 'red))
   ((not (memo-note-needreview item))
    (propertize (f-base (memo-note-title item)) 'face 'shadow)))
  :key (memo-note-id item)
  :ret-action #'memo-treemacs-card-perform-ret-action
  :children
  (when (> (memo-note-totalcards item) 1)
    (let ((items (memo-api--get-head-children-heads (memo-note-id item)
						    (memo-note-fileid item))))
      (funcall callback items)))
  :child-type
  'memo-treemacs-read-head-node
  :more-properties
  (if (= (memo-note-totalcards item) 1)
      `(:note ,item :leaf t :no-tab? t)
    `(:note ,item))
  :async? t)

(treemacs-define-expandable-node-type memo-treemacs-read-node
  :closed-icon treemacs-icon-tag-closed
  :open-icon   treemacs-icon-tag-open
  :label
  (if (> (memo-file-waitingcards item) 0)
      (propertize (f-base  (memo-file-filepath item)) 'face 'font-lock-string-face)
      (propertize (f-base  (memo-file-filepath item)) 'face 'font-lock-variable-name-face))
  :key (memo-file-fileid item)
  :ret-action #'memo-treemacs-file-perform-ret-action
  :children
  (when (> (memo-file-totalcards item) 0)
    (let ((items (memo-api--get-file-children-heads (memo-file-fileid item))))
      (funcall callback items)))
  :child-type
  'memo-treemacs-read-head-node
  :more-properties
  (if (= (memo-file-totalcards item) 0)
      `(:file ,item :leaf t :no-tab? t)
    `(:file ,item))
  :async? t)

(treemacs-define-variadic-entry-node-type memo-treemacs-file-node
  :key  memo-treemacs-root-node-key
  :children `(,(memo-api--get-file-by-filid memo-local-file-id))
  :child-type 'memo-treemacs-read-node)


(defun memo-treemacs-file-render (fileid expand-depth
                                 &optional right-click-actions)
  "Render treemacs buffer for file with FILEID EXPAND-DEPTH."
  (let ((buffer (get-buffer-create memo-treemacs-file-buffer-name)))
    (with-current-buffer buffer
      (treemacs-initialize memo-treemacs-file-node
        :with-expand-depth (or expand-depth 0)
        :and-do (progn
                  (memo-treemacs--set-mode-line-format buffer fileid)
                  (setq-local face-remapping-alist '((button . default)))
                  (setq-local treemacs-default-visit-action 'treemacs-RET-action)
                  (setq-local memo-treemacs--right-click-actions right-click-actions)
                  (setq-local window-size-fixed nil)
                  (setq-local treemacs--width-is-locked nil)
                  (setq-local treemacs-space-between-root-nodes nil)
		  (setq-local memo-local-file-id fileid)
                  (when treemacs-text-scale
                    (text-scale-increase treemacs-text-scale))
                  (memo-treemacs-generic-mode t)))
      (current-buffer))))


;;;----------------------------------
;;;  treemacs tree operator
;;;----------------------------------
(defun memo-treemacs-buffer-move-button-to-note (note buffer)
  "Move the bottom to the note in treemacs file buffer by NOTE and BUFFER."
  (-if-let* ((buf (get-buffer buffer)))
      (with-current-buffer buf
	(treemacs-goto-extension-node (append '("variadic-entry-node") (memo-note-path note))))))

(defun memo-treemacs-display-note-context (note-object)
  "Display note context (file or virtual head) in a split side window.

Shows The Treemacs view (either file-based or virtual-head-based)
in the top part and the '*memo-read*' buffer in the bottom part
of a side window.

Argument NOTE-OBJECT is the memo note object."
  (let* ((headid (memo-note-id note-object))
	 (file-note-object (memo-api--get-first-file-head headid))
         (fileid (memo-note-fileid file-note-object))
	 (file-note-path (memo-note-path file-note-object))
	 (window-sides-slots '(2 nil nil nil))
         (treemacs-file-buffer  (get-buffer-create memo-treemacs-file-buffer-name))
         (treemacs-note-buffer  (get-buffer-create memo-treemacs-note-buffer-name)))
    (display-buffer treemacs-file-buffer
		    `(display-buffer-in-side-window . (,@memo-treemacs-file-position-params)))
    (display-buffer treemacs-note-buffer
		    `(display-buffer-in-side-window . (,@memo-treemacs-note-position-params)))
    (memo-treemacs-note-render file-note-object memo-treemacs-virtual-head-expand-depth)
    (memo-treemacs-file-render fileid 1)
    (memo-treemacs-buffer-move-button-to-note file-note-object memo-treemacs-file-buffer-name))
  (memo-buffer-open-note note-object))

(provide 'memo-treemacs)
;;; memo-treemacs.el ends here
