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


(defun memo-goto-org ()
  "Jump to source point from review buffer."
  (interactive)
  (org-id-goto (memo-note-id memo--review-note)))

(defun memo-push-file ()
  "Push current org-file to database."
  (interactive)
  (memo--parse-result (memo-api--upload-file (buffer-file-name)))
  (if (not  (memo-api--return-err memo-api-result) )
      (message "Push file is success complete."))
)

(defun memo-update-dir ()
"Update file under dir."
 (interactive)
 (memo-api--progress "/Users/whale/Seafile/Dropbox/roam"))



(general-define-key
    "s-e l RET"    'memo-create-head
    "s-e l n"      'memo-create-subhead
    "s-e l p"      'memo-push-file
    "s-e l P"      'memo-update-dir
    "s-e l b"      'memo-goto-org
)

(provide 'memo-fast)
;;; memo-fast.el ends here
