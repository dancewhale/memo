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
(require 'ansi-color)

(require 'memo-core)
(require 'memo-fsrs)

;;; Core primitives

(defvar memo--lib-loaded nil
  "If dynamic module is loaded.")

(defvar memo--server-process nil
  "The object store memo server process.")

(defvar memo--root (file-name-directory (or load-file-name buffer-file-name))
  "The path to the root of memo package.")

(defvar memo--server-buffer "*memo-server*"
  "The buffer name of memo sever.")

(defvar memo--go-root (concat memo--root  "golib")
  "The path to the root of memo go src file.")

(defvar memo--module-path
  (concat memo--root "libmemo" module-file-suffix)
  "The path to the dynamic module.")

(defvar memo--server-port "50051"
  "Memo server port setting, default is 50051.")

(defvar memo--server-host "127.0.0.1"
  "Memo server host setting, default is 127.0.0.1.")


(defvar memo--server-db-path  (concat  memo--root ".memo.db")
  "Memo server db path, default is under package dir.")

(defvar memo--server-log-level  "0"
  "Memo server log level, -1 is debug, 0 is info, 1 is warn, 2 is error.")

  
(defun memo-compile-module ()
  "Compile dynamic module."
  (interactive)
  (let ((default-directory (concat memo--root "golib") ))
    (message "Start to compile memo library, Please wait...")
    (if (zerop (shell-command "make so"))
        (message "Compile module succeed!")
      (error "Compile Memo dynamic module failed"))))

(defun memo-compile-server ()
  "Compile memo server."
  (interactive)
  (message "Start to compile memo server, Please wait...")
  (let ((default-directory  memo--go-root))
    (if (zerop (shell-command "make build"))
        (message "Compile server succeed!")
      (error "Compile Memo server failed"))))

(defun memo--load-dynamic-module ()
  "Load dynamic module."
  (if (not (file-exists-p memo--module-path))
      (error "Dynamic module not exist")
    (load-file memo--module-path)
    (setq memo--lib-loaded t)))

(defun memo--set-server-env ()
  "Set memo server env, For example port,host,dbpath,loglevel."
  (setenv "port"     memo--server-port)
  (setenv "host"     memo--server-host)
  (setenv "dbpath"   memo--server-db-path)
  (setenv "loglevel" memo--server-log-level))

(defun memo--start-server ()
  "Start memo server in daemon."
  (let ((default-directory memo--go-root)
	(memo--server-path (concat memo--go-root "/memo"))
	(memo-server-buffer (get-buffer-create memo--server-buffer)))
    (with-current-buffer memo-server-buffer
      (ansi-color-for-comint-mode-on)
      (comint-mode))
    (unless memo--lib-loaded
      (unless (file-exists-p memo--module-path)
	(memo-compile-module)))
    (unless (file-exists-p memo--server-path)
      (memo-compile-server))
    (memo--set-server-env)
    (setq memo--server-process (start-process "memo-server" memo--server-buffer memo--server-path "daemon"))
    ;; for ansi-color filter
    (set-process-filter memo--server-process 'comint-output-filter)))



;;;###autoload
(defun memo-activate ()
  "Activate memo."
  (interactive)
  (unless memo--lib-loaded
    (unless (file-exists-p memo--module-path)
      (memo-compile-module))
    (memo--load-dynamic-module))

  (if memo--server-process
    (if (not (process-live-p memo--server-process))
      (memo--start-server)))
  (if (not memo--server-process)
      (memo--start-server))
  (message "memo server 启动成功.")

;  TODO: 如何处理版本不一致问题
;  (unless (string-equal memo-version (memo-lib-version))
;  (memo-compile-module)
;  (error "Dynamic module recompiled, please restart Emacs"))
  )
(provide 'memo)
;;; memo.el ends here
