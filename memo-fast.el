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
  (org-set-property memo-prop-note-type "default")
  (org-id-get-create))

(defun memo-create-subhead ()
"Create a new head as note in org file."
  (interactive)
  (org-insert-heading-respect-content)
  (org-demote-subtree)
  (org-set-property memo-prop-note-type "default")
  (org-id-get-create))



;; setting config for memo.
(general-define-key
    "s-e l RET"    'memo-create-head
    "s-e l n"      'memo-create-subhead
    "s-e l p"      'memo-sync-current-file
    "s-e l P"      'memo-sync-db
    "s-e l b"      'memo-goto-org
    "s-e l r"      'memo-review-note
)


(setq memo-org-directory "/Users/whale/Seafile/Dropbox/roam")
(setq memo-log-level "-1")

(add-hook 'after-save-hook 'memo-sync-file-after-save)

(provide 'memo-fast)
;;; memo-fast.el ends here
