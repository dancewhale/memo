;;; memo.el --- Use for memo (interactive "P") -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 whale
;;
;; Author: whale <whale@MacBook-Pro-4.local>
;; Maintainer: whale <whale@MacBook-Pro-4.local>
;; Created: 6月 17, 2024
;; Modified: 6月 17, 2024
;; Version: 0.0.1
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

(require 'memo-core)

;;; Core primitives

(defvar memo--lib-loaded nil
  "If dynamic module is loaded.")

(defvar memo--root (file-name-directory (or load-file-name buffer-file-name))
  "The path to the root of memo package.")

(defvar memo--go-root (concat memo--root  "golib")
  "The path to the root of memo go src file.")

(defvar memo--module-path
  (concat memo--root "libmemo" module-file-suffix)
  "The path to the dynamic module.")


(defconst memo-prop-note-type "MEMO_NOTE_TYPE")
(defconst memo-prop-note-id   "ID")
(defconst memo--review-buffer-name "*memo-review*"
  "The memo buffer for review note show and flip."
  )

(cl-defstruct memo-note
  id type content finish)

(defvar memo--review-note nil
  "The memo-note object which store note info wait for review.")


(defun memo-push-note-at-point ()
  "Push note at point to memo db.
If heading without an `MEMO_NOTE_TYPE' property push failed.
If heading without an `ID' property create it."
  (interactive)
  (save-excursion
    (let* ((note-type  (org-entry-get nil memo-prop-note-type))
	   (note-content  (memo--note-contents-current-heading))
	   (note-id (org-id-get-create)))
      (if (not note-type)
	  (user-error "Missing note type")
	(if (memo-api--create-or-update-note note-id note-type note-content)
	    (message "Create note to memo successful.")
	  (message "Create note to memo failed."))))))

(defun memo-review-note()
  "Review note."
  (interactive)
  (setq memo--review-note (memo--get-review-note-object-from-server))
  (memo--review-show memo--review-note)
)

(defun memo--review-show (mnote)
  "Show note in review buffer, MNOTE is memo-note object."
  (if (not (memo-note-id mnote))
      (user-error "Review memo-note object is nil"))
  (let* ((buf (get-buffer-create memo--review-buffer-name))
	 answer-start answer-end)
    (with-current-buffer buf
	(memo-remove-overlays)
	(erase-buffer)
	(insert (memo-note-content mnote))
	(goto-char (point-min))
	(if (re-search-forward "^-+$" nil t)
	    (progn
	       (forward-line)
	       (beginning-of-line)
	       (setq answer-start (point))
	       (goto-char (point-max))
	       (setq answer-end (point))
	       (memo-hide-region answer-start answer-end)))
	(switch-to-buffer buf)
	(org-mode)
      )
   )
 )

(defun memo-flip-note()
  "Flip review note."
  (interactive)
  (let* ((buf (get-buffer  memo--review-buffer-name)))
    (if buf
	(with-current-buffer buf
	  (memo-remove-overlays)
	  )))
  )

(defun memo-review-easy()
  "Review note with score: Easy."
  (interactive)
  (memo-api--review-note (memo-note-id memo--review-note) "Easy")
  (memo-review-note)
  )

(defun memo-review-good()
  "Review note with score: Good."
  (interactive)
  (memo-api--review-note (memo-note-id memo--review-note) "Good")
  (memo-review-note)
  )

(defun memo-review-hard()
  "Review note with score: Hard."
  (interactive)
  (memo-api--review-note (memo-note-id memo--review-note) "Hard")
  (memo-review-note)
  )

(defun memo-review-again()
  "Review note with score: Again."
  (interactive)
  (memo-api--review-note (memo-note-id memo--review-note) "Again")
  (memo-review-note)
  )


(defun memo-goto-org ()
  (interactive)
  (org-id-goto (memo-note-id memo--review-note)))

(defun memo--get-review-note-object-from-server ()
  "Return memo-note object which need review from server."
  (let* ((memo-note-object (memo-api--get-next-review-note))
	 (note-id (car memo-note-object))
	 (note-type (cadr memo-note-object))
	 (note-content (caddr memo-note-object)))
    (make-memo-note :id note-id
		    :type note-type
		    :content note-content
		    :finish nil)))

(defun memo--get-note-object-from-point ()
  "Make and return memo-note object from current note."
  (let* ((note-id (org-id-get-create))
	 (note-content  (memo--note-contents-current-heading))
	 (note-type (org-entry-get nil memo-prop-note-type))
    (unless note-type (user-error "Missing note type"))
    (make-memo-note :id note-id
		    :type note-type
		    :content note-content
		    :finish nil)))
  )


(defun memo--note-contents-current-heading ()
   "Get entry content until any subentry."
  ;; We move around with regexes, so restore original position
  (save-excursion
    ;; Jump to beginning of entry
    (goto-char (org-entry-beginning-position)) ;; was: (re-search-backward "^\\*+ .*\n")
    ;; Skip heading
    (re-search-forward ".*\n")
    ;; Possibly skip property block until end of entry
    (re-search-forward ":properties:\\(.*\n\\)*:end:" (org-entry-end-position) t)
    ;; Get entry content
    (let ((from (point))
          (to (progn (outline-next-heading) (point))))
      (buffer-substring-no-properties from to))))
  
(defun memo-compile-module ()
  "Compile dynamic module."
  (interactive)
  (let ((default-directory (concat memo--root "golib") ))
    (if (zerop (shell-command "make so"))
        (message "Compile module succeed!")
      (error "Compile Memo dynamic module failed"))))

(defun memo-compile-server ()
  "Compile memo server."
  (interactive)
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

(defun memo--start-server ()
  "Start memo server in daemon."
  (let ((default-directory memo--go-root)
	(memo--server-path (concat memo--go-root "/memo")))
    (start-process-shell-command "memo" "*memo-server*" memo--server-path)))
;; wait for debug
(memo--start-server)


;;;###autoload
(defun memo-activate ()
  "Activate memo."
  (unless memo--lib-loaded
    (unless (file-exists-p memo--module-path)
      (memo-compile-module))
    (memo--load-dynamic-module))

;  如何处理版本不一致问题
;  (unless (string-equal memo-version (memo-lib-version))
;    (memo-compile-module)
;    (error "Dynamic module recompiled, please restart Emacs"))


; 启动后台进程
  (when memo--lib-loaded
    (dolist (binding memo-translate-keybindings)
          (define-key memo-active-mode-map (kbd binding) 'memo-send-keybinding))
))



(provide 'memo)
;;; memo.el ends here
