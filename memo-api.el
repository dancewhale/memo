;;; memo-api.el --- Use for memo (interactive "P") -*- lexical-binding: t; -*-
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
;;  Use for memo fsrs library.
;;
;;; Code:

(require 'cl-lib)
(require 'org-element)

;;; Properties and Env setting
(defconst memo-prop-note-type  "MEMO_NOTE_TYPE"
  "Property used to store the cards type;
and used for backend to indentify memo head.")

(defconst memo-prop-note-id  "ID"
  "Property used to store the cards id; 
and used for backend to indentify memo head.")

(defvar  memo-log-level "0"
  "Setting dynamic module log level, -1 debug, 0 info, 1 warn, 2 error.")


;;; hook for memo-api call.
;;; Setting env before call go module api.
(defun memo--before-api-call ()
  "Use to prepare some action like env setting before call backend api."
  (setenv "MEMO_TYPE_PROVERTY" memo-prop-note-type)
  (setenv "MEMO_ID_PROVERTY" memo-prop-note-id)
  (setenv "MEMO_LOG_LEVEL" memo-log-level))



;;; parse api call result; store value and throught err.
(cl-defstruct memo-api--return
  err value)

(defvar  memo-api-result  nil
"Store result return from memo-api call.")


(defun memo--parse-result (result)
"To parse value return from backend memo-api call;
pare value like (err (value value))"
  (setq memo-api-result  (make-memo-api--return  :err (car result) 
                                                 :value (cadr result)))
  (if  (car result)
    (user-error (car result))))


;; get note for review.
(cl-defstruct memo-note
  id type content hash)

(defvar memo--review-note nil
  "The memo-note object which store note info wait for review.")

(defun memo--get-review-note-object ()
  "Return memo-note object which need review from server;
memo-note is (orgid  type  content)."
  (memo--before-api-call)
  (let* ((memo-note-object (memo--parse-result (memo-api--get-next-review-note)))
	 (note-id (car memo-note-object))
	 (note-type (cadr memo-note-object))
	 (note-content (caddr memo-note-object)))
    (setq memo--review-note (make-memo-note :id note-id
					     :type note-type
					     :content note-content
					     :hash nil))))

;; setting memo scan dir and function.
(defvar  memo-org-directory nil
"Setting memo dir to scan org file.")


(defun memo-sync-db ()
"Synchronize the db state with the current Org files on-disk."
  (interactive)
  (memo--parse-result (memo-api--sync-dir memo-org-directory))
  (if (not  (memo-api--return-err memo-api-result) )
      (message "Sync dir is success complete.")))


(defun memo-sync-file ()
"Synchronize current org-file to db."
  (interactive)
  (memo--parse-result (memo-api--sync-file (buffer-file-name)))
  (if (not  (memo-api--return-err memo-api-result) )
      (message "Push file is success complete.")))


;;  TODO: go api需要新增强制清空 headline 和 file 的接口。
;;  TODO: 更新daily 一个文件存在问题，headline 缺失
;;  TODO: log无法根据env 进行调整


(provide 'memo-api)
;;; memo-api.el ends here
