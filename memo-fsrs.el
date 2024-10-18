;;; memo-fsrs.el --- Use for memo (interactive "P") -*- lexical-binding: t; -*-
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
(require 'thunk)
(require 'org-element)
(require 'ansi-color)

;; custom var
(defcustom memo-inherit-tags t
  "Inherit tags, set to nil to turn off."
  :type 'boolen
  :group 'memo)

(defcustom memo-default-tags nil
  "Default tags.

This variable will be used if none is set on the org item nor as
a global property."
  :type '(repeat string)
  :group 'memo)

(defcustom memo-default-note-type "default"
  "Default note type.

This variable will be used if none is set on the org item nor as
a global property."
  :type 'string
  :group 'memo)


(defcustom memo-fields-get-alist
  '(("Basic" . memo-fields-get-default)
    ("Cloze" . memo-fields-get-cloze))
  "Alist of (NOTE-TYPE . FUNCTION) pairs.

Used by `memo--entry-get-fields'.

FUNCTION should return a list of string, where each string
corresponds to a field in NOTE-TYPE."
  :type 'alist
  :group 'memo)


;; review releat var and function
(defconst memo--review-buffer-name "*memo-review*"
  "The memo buffer for review note show and flip.")



;;; Org Tags / Properties

(defconst memo-prop-note-type  "MEMO_NOTE_TYPE"
  "Property used to store the cards type.")

(defconst memo-prop-note-hash  "MEMO_NOTE_HASH"
"Used to determine whether the note has been modified.")

(defconst memo-prop-global-tags "MEMO_TAGS"
  "Specify Memo note tags.")



(defun memo--get-global-keyword (keyword)
  "Get global property by KEYWORD."
  (cadar (org-collect-keywords (list keyword))))


(defun memo--find-note-prop (name default)
  "Find property with NAME from:
1. item,
2. inherited from parents
3. in-buffer setting
4. otherwise use DEFAULT"
  (thunk-let
      ((prop-item (org-entry-get nil name t))
       (keyword-global (memo--get-global-keyword name)))
    (cond
     ((stringp prop-item) prop-item)
     ((stringp keyword-global) keyword-global)
     ((stringp default) default)
     (t (error "No property '%s' in item nor file nor set as default!"
               name)))))

(defun memo--get-tags ()
  "Get all tags for the current note include inherited from parents."
  (append
   (delete-dups
    (split-string
     (let ((global-tags (memo--get-global-keyword memo-prop-global-tags)))
       (concat
        (if memo-inherit-tags
            (substring-no-properties (or (org-entry-get nil "ALLTAGS") ""))
          (org-entry-get nil "TAGS"))
        global-tags))
     ":" t))
   memo-default-tags))


;; hash relative code.
(defun memo--compute-note-hash ()
  "Compute hash from note content and tag."
  (let* ((note-content  (memo--note-contents-current-heading))
         (tags (memo--get-tags)))
    (md5 (mapconcat #'identity (push note-content tags) ""))))



;; note push
(cl-defstruct memo-note
  id type content hash)

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
	   (note-id (org-id-get-create))
	   (note-hash (memo--compute-note-hash)))
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
  "Jump to source point from review buffer."
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
		    :hash nil)))

(defun memo--get-note-object-from-point ()
  "Make and return memo-note object from current note."
  (let* ((note-id (org-id-get-create))
	 (note-content  (memo--note-contents-current-heading))
	 (note-type (org-entry-get nil memo-prop-note-type))
    (unless note-type (user-error "Missing note type"))
    (make-memo-note :id note-id
		    :type note-type
		    :content note-content
		    :hash nil)))
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

;;; Card Initialization
(defun org-memo-entry-p ()
  "Check if the current heading is a flashcard."
  (member org-memo-flashcard-tag (org-get-tags nil 'local)))

(defun org-memo--init-card (type)
  "Initialize the current card as a flashcard.
Should only be used by the init functions of card TYPEs."
  (when (org-memo-entry-p)
    (error "Headline is already a flashcard"))
  (let ((algo
         (if (= 1 (length org-memo-algorithms))
             (car org-memo-algorithms)
           (completing-read "Algorithm: " org-memo-algorithms))))
    (when (null algo)
        (error "No algorithm selected"))
    (org-back-to-heading)
    (org-set-property
     org-memo-created-property
     (org-memo-timestamp-in 0))
    (org-set-property org-memo-type-property type)
    (when algo
      (org-set-property org-memo-algo-property algo))
    (org-id-get-create)
    (org-memo--add-tag org-memo-flashcard-tag)
    (run-hooks 'org-memo-after-init-card-hook)))


(provide 'memo-fsrs)
;;; memo-fsrs.el ends here
