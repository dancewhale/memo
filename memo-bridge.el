;;; memo-bridge-bridge.el --- LSP bridge  -*- lexical-binding: t -*-

;; Filename: memo-bridge-bridge.el
;; Description: LSP bridge
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-06-15 14:10:12
;; Version: 0.5
;; Last-Updated: 2025-02-14 15:23:53 +0800
;;           By: dancewhale
;; URL: https://github.com/manateelazycat/memo-bridge
;; Keywords:
;; Compatibility: emacs-version >= 28
;; Package-Requires: ((emacs "28") (markdown-mode "2.6"))
;;
;; Features that might be required by this library:
;;
;; Please check README
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Memo-Bridge
;;

;;; Installation:
;;
;; Please check README
;;

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET memo-bridge RET
;;

;;; Change log:
;;
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Code:


(require 'memo-bridge-epc)

(defvar memo-bridge-server nil
  "The Memo-Bridge Emacs EPC Server Process.")

(defvar memo-bridge-server-port nil
  "The Memo-Bridge Emacs EPC Server Process Port.")

(defvar memo-bridge-golang-file (concat memo--root "server")
  "The Memo-Bridge golang server binary file path.")

(defvar memo-bridge-golang-dlv-file "dlv"
  "The file path of  golang debug file dlv.")

(defun memo-bridge--start-epc-server ()
  "Function to start the EPC server."
  (unless (process-live-p memo-bridge-server)
    (setq memo-bridge-server
          (memo-bridge-epc-server-start
	   (lambda (mngr)
	     (let ((mngr mngr))
	       (memo-bridge-epc-define-method mngr "eval-in-emacs"  'memo-bridge--eval-in-emacs-func)
	       (memo-bridge-epc-define-method mngr "get-emacs-vars" 'memo-bridge--get-emacs-vars-func)
	    ))  memo-bridge-server-port ))
    (if memo-bridge-server
        (setq memo-bridge-server-port (process-contact memo-bridge-server :service))
      (error "[Memo-Bridge] memo-bridge-server failed to start")))
  memo-bridge-server)

(defun memo-bridge--eval-in-emacs-func (sexp-string)
  (eval (read sexp-string))
  ;; Return nil to avoid epc error `Got too many arguments in the reply'.
  nil)

(defun memo-bridge--get-emacs-var-func (var-name)
  (let* ((var-symbol (intern var-name))
         (var-value (symbol-value var-symbol))
         ;; We need convert result of booleanp to string.
         ;; Otherwise, python-epc will convert all `nil' to [] at Python side.
         (var-is-bool (prin1-to-string (booleanp var-value))))
    (list var-value var-is-bool)))

(defun memo-bridge--get-emacs-vars-func (&rest vars)
  (mapcar #'memo-bridge--get-emacs-var-func vars))

(defvar memo-bridge-golang-epc-server-mgr nil)

(defvar memo-bridge-internal-process nil)
(defvar memo-bridge-internal-process-prog nil)
(defvar memo-bridge-internal-process-args nil)

(defcustom memo-bridge-name "*memo-bridge*"
  "Name of Memo-Bridge buffer."
  :type 'string)

(defun memo-bridge-call-async (method &rest args)
  "Call Golang EPC function METHOD and ARGS asynchronously."
  (if (memo-bridge-process-live-p)
      (memo-bridge-deferred-chain
	(memo-bridge-epc-call-deferred memo-bridge-golang-epc-server-mgr (read method) args))
    (user-error "Memo server not Start")))

(defun memo-bridge-call-wait (response)
"Wait for Call response."
  (let ((it response))
     (while (eq nil (memo-bridge-deferred-object-status it))
       (sleep-for 0.1))
     it))

(defun memo-bridge-call-sync (method &rest args)
  "Call Golang EPC function METHOD and ARGS synchronously."
  (let ((result nil))
    (unless (memo-bridge-process-live-p)
      (user-error "Memo server not Start"))
    (memo-bridge-deferred-chain
      (memo-bridge-epc-call-deferred memo-bridge-golang-epc-server-mgr (read method) args)
      (memo-bridge-call-wait it)
      (memo-bridge-deferred-nextc it
        (lambda (x) (progn  (setq result x))))
      (memo-bridge-deferred-error it
        (lambda (er) (progn  (setq result er)))))
    result))

(defun memo-bridge-process-live-p ()
  (memo-bridge-epc-live-p memo-bridge-golang-epc-server-mgr))

(defvar memo-bridge-log-buffer-window nil)

(defun memo-bridge-restart-process ()
  "Stop and restart Memo-Bridge process."
  (interactive)
  ;; Record log buffer window before restart memo-bridge process.
  (setq memo-bridge-log-buffer-window
        (cl-dolist (buffer (buffer-list))
          (when (string-equal (buffer-name buffer) memo-bridge-name)
            (cl-return (cons (get-buffer-window buffer) (selected-window)))
            )))

  ;; Restart memo-bridge process.
  (memo-bridge-kill-process)
  (memo-bridge-start-process)

  ;; Try restore memo-bridge log buffer after restart.
  (when memo-bridge-log-buffer-window
    (save-excursion
      (when (window-live-p (car memo-bridge-log-buffer-window))
        (select-window (car memo-bridge-log-buffer-window))
        (switch-to-buffer memo-bridge-name))
      (select-window (cdr memo-bridge-log-buffer-window))))

  (message "[Memo-Bridge] Process restarted."))

(defun memo-bridge--first-start (memo-bridge-golang-epc-port)
  "Call `memo-bridge--open-internal' upon receiving `start_finish' signal from server."
  ;; Make EPC process.
  (setq memo-bridge-golang-epc-server-mgr (make-memo-bridge-epc-manager
                                :server-process memo-bridge-internal-process
                                :commands (cons memo-bridge-internal-process-prog memo-bridge-internal-process-args)
                                :title (mapconcat 'identity (cons memo-bridge-internal-process-prog memo-bridge-internal-process-args) " ")
                                :port memo-bridge-golang-epc-port
                                :connection (memo-bridge-epc-connect "127.0.0.1" memo-bridge-golang-epc-port)
                                ))
  (memo-bridge-epc-init-epc-layer memo-bridge-golang-epc-server-mgr))

(defun memo-bridge-start-process-debug ()
  "Start Memo-Bridge process if it isn't started."
  (interactive)
  (if (memo-bridge-process-live-p)
      (message "Memo server is already start.")
    ;; start epc server and set `memo-bridge-server-port'
    (memo-bridge--start-epc-server)
    (let* ((memo-bridge-debug-args (append
				    (list "--check-go-version=false" "--listen=:2345"  "--log-dest=/private/tmp/log" "--headless=true" "--api-version=2" "exec")
                                    (list memo-bridge-golang-file "--" "daemon")
			            (list "--emacs-port" (number-to-string memo-bridge-server-port))
			            (list "--log-level" memo-log-level))))

      ;; Set process arguments.
      (setq memo-bridge-internal-process-prog memo-bridge-golang-dlv-file)
      (setq memo-bridge-internal-process-args memo-bridge-debug-args)

      (with-current-buffer (get-buffer-create  memo-bridge-name)
	(ansi-color-for-comint-mode-on)
	(comint-mode))
      ;; Start server process.
      (let ((process-connection-type (not (memo-bridge--called-from-wsl-on-windows-p))))
        (setq memo-bridge-internal-process
              (apply 'start-process
                     memo-bridge-name memo-bridge-name
                     memo-bridge-internal-process-prog memo-bridge-internal-process-args)))
      (set-process-filter memo-bridge-internal-process 'comint-output-filter)
      (set-process-query-on-exit-flag memo-bridge-internal-process nil))))

(defun memo-bridge-start-process ()
  "Start Memo-Bridge process if it isn't started."
  (interactive)
  (if (memo-bridge-process-live-p)
      (message "Memo server is already start.")
    ;; start epc server and set `memo-bridge-server-port'
    (memo-bridge--start-epc-server)
    (let* ((memo-bridge-args (append
                               (list "daemon")
			       (list "--emacs-port" (number-to-string memo-bridge-server-port))
			       (list "--log-level" memo-log-level))))
      
      ;; Set process arguments.
      (setq memo-bridge-internal-process-prog memo-bridge-golang-file)
      (setq memo-bridge-internal-process-args memo-bridge-args)

      (with-current-buffer (get-buffer-create  memo-bridge-name)
	(ansi-color-for-comint-mode-on)
	(comint-mode))
      ;; Start server process.
      (let ((process-connection-type (not (memo-bridge--called-from-wsl-on-windows-p))))
        (setq memo-bridge-internal-process
              (apply 'start-process
                     memo-bridge-name memo-bridge-name
                     memo-bridge-internal-process-prog memo-bridge-internal-process-args)))
      (set-process-filter memo-bridge-internal-process 'comint-output-filter)
      (set-process-query-on-exit-flag memo-bridge-internal-process nil))
    (message "Memo server start success.")))

(defun memo-bridge--called-from-wsl-on-windows-p ()
  "Check whether memo-bridge is called by Emacs on WSL and is running on Windows."
  (and (eq system-type 'gnu/linux)))

(defvar memo-bridge-stop-process-hook nil)

(defun memo-bridge-kill-process ()
  "Stop Memo-Bridge process and kill all Memo-Bridge buffers."
  (interactive)

  ;; Run stop process hooks.
  (run-hooks 'memo-bridge-stop-process-hook)

  ;; Kill process after kill buffer, make application can save session data.
  (memo-bridge--kill-golang-process))



(add-hook 'kill-emacs-hook #'memo-bridge-kill-process)

(defalias 'memo-bridge-stop-process #'memo-bridge-kill-process)

(defun memo-bridge--kill-golang-process ()
  "Kill Memo-Bridge background golang process."
  (when (memo-bridge-process-live-p)
    ;; Delete Memo-Bridge server process.
    (memo-bridge-epc-stop-epc memo-bridge-golang-epc-server-mgr)
    ;; Kill *memo-bridge* buffer.
    (when (get-buffer memo-bridge-name)
      (kill-buffer memo-bridge-name))
    (setq memo-bridge-golang-epc-server-mgr nil)
    (message "[Memo-Bridge] Process terminated.")))



(provide 'memo-bridge)
;;; memo-bridge.el ends here
