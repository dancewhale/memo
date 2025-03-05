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


(defconst memo-treemacs-deps-buffer-name "*Java Dependency List*")

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

(defun memo-treemacs-virtual-heads ()
  "Display virtual heads."
  (interactive)
  (select-window
   (display-buffer-in-side-window
    (memo-treemacs-render
     (memo-api--get-virt-heads-by-parentid (memo-get-current-note-id))
     "*Memo Relete Node*" memo-treemacs-virtual-head-expand-depth)
    memo-treemacs-virtual-head-position-params)))



(provide 'memo-treemacs)
;;; memo-treemacs.el ends here
