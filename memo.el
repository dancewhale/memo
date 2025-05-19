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

(require 'cl-lib)
(require 'org-element)

;; Core env setting.
(defconst memo-prop-note-source  "MEMO_NOTE_SOURCE"
  "Property used to store the cards SOURCE.")

(defconst memo-prop-note-id  "ID"
  "Property used to store the cards ID.")

(defconst memo-prop-note-weight  "MEMO_NOTE_WEIGHT"
  "Property used to store the cards WEIGHT.")


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

(defconst memo-media-path "media"
"Setting memo media resource dir name under memo-org-directory.")


;;; Core primitives
(defvar memo--root (if load-file-name (file-name-directory load-file-name)
		     default-directory)
"The path to the root of memo package.")

(defvar memo--go-root (concat memo--root  "golib")
  "The path to the root of memo go src file.")

(defconst memo-server-path (concat memo--root "server"))

(defun memo-compile-server ()
  "Compile dynamic module."
  (interactive)
  (let ((default-directory (concat memo--root "golib") ))
    (message "Start to compile memo server")
    (if (zerop (shell-command "make server"))
        (message "Compile module succeed!")
      (error "Compile Memo dynamic server failed"))))

(defun memo-start ()
  "Start memo Server."
  (interactive)
  (let ((server-path (concat memo--root "server")))
    (unless (file-exists-p server-path)
      (memo-compile-server))
    (memo-bridge-start-process)))

;;;###autoload
(defun memo-activate ()
  "Activate memo."
  (interactive)
  (require 'memo-core)
  (require 'memo-buffer)
  (require 'memo-api)
  (require 'memo-bridge)
  (require 'memo-bridge-epc)
  (require 'memo-card)
  (require 'memo-note)
  (require 'memo-cloze)
  (require 'memo-treemacs)
  (require 'memo-annotation)
  (require 'memo-annotation-region)
  (require 'memo-annotation-type)
  (memo-start))

(provide 'memo)
;;; memo.el ends here
