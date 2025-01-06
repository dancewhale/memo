;;; memo-fast.el --- Use for memo (interactive "P") -*- lexical-binding: t; -*-
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
;;  Thanks  Author of anki-editor, org-fc, org-anki, many code borrow from those package.
;;  Use for memo (interactive "P")
;;
;;; Code:

(defun memo-create-head ()
"Create a new head as note in org file."
  (interactive)
  (org-insert-heading-respect-content)
  (org-set-property memo-prop-note-weight "50")
  (org-set-property memo-prop-note-schedule "NORMAL")
  (org-id-get-create))

(defun memo-create-subhead ()
"Create a new head as note in org file."
  (interactive)
  (org-insert-heading-respect-content)
  (org-demote-subtree)
  (org-set-property memo-prop-note-weight "50")
  (org-set-property memo-prop-note-schedule "NORMAL")
  (org-id-get-create))

(provide 'memo-fast)
;;; memo-fast.el ends here
