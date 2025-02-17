;;; memo.el --- Use for memo (interactive "P") -*- lexical-binding: t; -*-
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

(require 'epc)
(require 'cl-lib)
(require 'org-element)

;; Core env setting.
(defconst memo-prop-note-source  "MEMO_NOTE_SOURCE"
  "Property used to store the cards SOURCE.")

(defvar memo-log-level "0"
  "Setting dynamic module log level, -1 debug, 0 info, 1 warn, 2 error.")

(defvar memo-db-max-idle-conn  "10"
  "Property used to store the cards type;
and used for backend to indentify memo head.")

(defvar memo-db-max-open-conn  "100"
  "Property used to store the cards type;
and used for backend to indentify memo head.")

(defvar memo-db-directory nil
"Setting memo db dir to store database;
if nil will default use user home dir.")

(defvar memo-org-directory nil
"Setting memo dir to scan org file.")


;;; Core primitives

(defvar memo--root (if load-file-name (file-name-directory load-file-name)
		     default-directory)
"The path to the root of memo package.")

(defvar memo--go-root (concat memo--root  "golib")
  "The path to the root of memo go src file.")

(defconst memo-server-path (concat memo--root "server"))

(defvar  memo-epc nil
  "Store memo server session information.")

(defun memo-compile-server ()
  "Compile dynamic module."
  (interactive)
  (let ((default-directory (concat memo--root "golib") ))
    (message "Start to compile memo server")
    (if (zerop (shell-command "make server"))
        (message "Compile module succeed!")
      (error "Compile Memo dynamic server failed"))))

;;;###autoload
(defun memo-activate ()
  "Activate memo."
  (interactive)
  (require 'memo-core)
  (require 'memo-buffer)
  (require 'memo-api)
  (require 'memo-card)
  (require 'memo-cloze)
  (require 'memo-fast)
  (memo-compile-server))

(provide 'memo)
;;; memo.el ends here
