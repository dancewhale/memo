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

;;; Core primitives

(defvar memo--lib-loaded nil
  "If dynamic module is loaded.")

(defvar memo--root (file-name-directory (or load-file-name buffer-file-name))
  "The path to the root of memo package.")

(defvar memo--go-root (concat memo--root  "golib")
  "The path to the root of memo go src file.")

(defvar memo--module-path
  (concat memo--root "libmemo.so")
  "The path to the dynamic module.")

(defvar memo--db-path  (concat  memo--root ".memo.db")
  "Memo server db path, default is under package dir.")

(defvar memo--log-level  "-1"
  "Memo server log level, -1 is debug, 0 is info, 1 is warn, 2 is error.")

  
(defun memo-compile-module ()
  "Compile dynamic module."
  (interactive)
  (let ((default-directory (concat memo--root "golib") ))
    (message "Start to compile memo library, Please wait...")
    (if (zerop (shell-command "make so"))
        (message "Compile module succeed!")
      (error "Compile Memo dynamic module failed"))))

(defun memo--load-dynamic-module ()
  "Load dynamic module."
  (if (not (file-exists-p memo--module-path))
      (error "Dynamic module not exist")
    (load-file memo--module-path)
    (setq memo--lib-loaded t)))

(defun memo--set-server-env ()
  "Set memo server env, For example port,host,dbpath,loglevel."
  (setenv "dbpath"   memo--db-path)
  (setenv "loglevel" memo--log-level))


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
  (unless memo--lib-loaded
    (unless (file-exists-p memo--module-path)
      (memo-compile-module))
    (memo--load-dynamic-module)))

(provide 'memo)
;;; memo.el ends here
