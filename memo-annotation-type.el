;;; memo-annotation-type.el --- Annotation type and face management -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2025  Dancewhale

;; Author: Dancewhale <542727233@qq.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This file provides a system for managing annotation types and their
;; associated faces (visual styles). It allows defining different types
;; of annotations (e.g., highlight, comment) and associating multiple
;; selectable faces with each type.

;;; Code:

(defgroup memo-annotation-faces nil
  "Custom faces for memo annotations."
  :group 'memo)

;;; Annotation Type Definitions
;; These are integer constants representing different annotation types.
(defconst memo-annotation-type-highlight 0 "Annotation type for highlighting text.")
(defconst memo-annotation-type-comment 1 "Annotation type for adding comments.")
(defconst memo-annotation-type-important 2 "Annotation type for marking important sections.")
(defconst memo-annotation-type-cloze   3 "Annotation type for cloze sections.")

(defconst memo-annotation-type-list
  `(("hightlight" . memo-annotation-type-highlight)
    ("commemnt" .  memo-annotation-type-comment)
    ("cloze" .  memo-annotation-type-cloze)
    ("important" . memo-annotation-type-important)))

(defun memo-annotation-get-type ()
  "Prompt user to select an annotation type and return its value.
This function displays a completion list of descriptive type names
defined in `memo-annotation-type-list`.
Returns the integer constant associated with the selected type (e.g., `memo-annotation-type-highlight`).
Returns nil if no selection is made or if `memo-annotation-type-list` is empty."
  (let* ((descriptive-names (mapcar #'car memo-annotation-type-list))
         (prompt "Select annotation type: ")
         (selected-desc-name (completing-read prompt descriptive-names nil t)))
    (if selected-desc-name
        (symbol-value (cdr (assoc selected-desc-name memo-annotation-type-list)))
      (progn
        (message "No annotation type selected.")
        nil))))

;; Add more types as needed by defining new defconst here.

;;; Default Face Definitions
;; Define some default faces that can be associated with annotation types.
;; Users can customize these faces using `customize-face`.
(defface memo-annotation-face-default
  '((t :background "#1d3c25"))
  "Default annotation face string to use when no specific face is set."
  :group 'memo-annotation-faces)

(defface memo-annotation-face-highlight-red
  '((t :background "red" :foreground "white"))
  "红色高亮 (白字红底)"
  :group 'memo-annotation-faces)

(defface memo-annotation-face-highlight-green
  '((t :background "green" :foreground "black"))
  "绿色高亮 (黑字绿底)"
  :group 'memo-annotation-faces)

(defface memo-annotation-face-highlight-yellow
  '((t :background "yellow" :foreground "black"))
  "黄色高亮 (黑字黄底)"
  :group 'memo-annotation-faces)

(defface memo-annotation-face-comment-italic
  '((t :slant italic :foreground "dim gray"))
  "灰色斜体评论"
  :group 'memo-annotation-faces)

(defface memo-annotation-face-important-bold-red
  '((t :weight bold :foreground "red"))
  "重要标记 (红色粗体)"
  :group 'memo-annotation-faces)


(defface memo-annotation-face-cloze-yellow
  '((t :background "yellow" :foreground "black"))
  "黄色高亮 (黑字黄底)"
  :group 'memo-annotation-faces)

;;; Annotation Type to Face Mapping
;; This variable stores the association between annotation types and available faces.
;; It's an alist where each key is an annotation type (integer constant)
;; and the value is a list of (Descriptive Name . Face Symbol) pairs.

(defvar memo-annotation-type-face-map
  `((,memo-annotation-type-highlight
     ("红色高亮" . memo-annotation-face-highlight-red)
     ("绿色高亮" . memo-annotation-face-highlight-green)
     ("黄色高亮" . memo-annotation-face-highlight-yellow))
    (,memo-annotation-type-comment
     ("灰色斜体评论" . memo-annotation-face-comment-italic))
    (,memo-annotation-type-cloze
     ("黄色高亮" . memo-annotation-face-cloze-yellow))
    (,memo-annotation-type-important
     ("重要标记 (红)" . memo-annotation-face-important-bold-red)))
  "Alist mapping annotation types to a list of (Descriptive Name . Face Symbol).
Each entry is (TYPE . ((DESC-NAME . FACE-SYMBOL) ...)).
This allows associating multiple named faces with each annotation type.
To add a new face for a type, or a new type, modify this variable
or use `memo-annotation-register-face`.")

;;; Core Functionality

(defun memo-annotation-select-face-for-type (annotation-type &optional facename)
  "Prompt user to select a face for the given ANNOTATION-TYPE.
ANNOTATION-TYPE is an integer constant (e.g., `memo-annotation-type-highlight`).
This function displays a completion list of descriptive face names
associated with the ANNOTATION-TYPE.
Returns a cons cell (FACE . FACENAME), where FACE is the selected face symbol
(e.g., 'memo-annotation-face-highlight-red) and FACENAME is its descriptive name.
Returns (memo-annotation-face-default . \"default\") if facename is \"default\",
no selection is made, the type is not found, or no faces are configured."
  (catch 'value
    (let* ((type-config (assoc annotation-type memo-annotation-type-face-map))
           (face-options (cdr type-config)))
      (if (and facename (string= facename "default"))
          (throw 'value (cons 'memo-annotation-face-default "default"))
	(if face-options
            (if facename
		;; If facename is provided (and not "default"), try to find it directly
		(let ((found-face-pair (assoc facename face-options))) ; facename is Descriptive Name
                  (if found-face-pair ; found-face-pair is (Descriptive Name . Face Symbol)
                      (throw 'value (cons (cdr found-face-pair) (car found-face-pair))) ; Corrected: Return (Face . Facename)
                    (progn
                      (message "Can't find face %s for type '%s'. Using default." facename annotation-type)
                      (throw 'value  (cons memo-annotation-face-default "default")))))
              ;; If facename is not provided, use completing-read
              (let* ((descriptive-names (mapcar #'car face-options))
                     (prompt (format "Find face for type %s: " annotation-type))
                     (selected-desc-name (completing-read prompt descriptive-names nil t)))
		(if selected-desc-name
                    (let ((selected-face-pair (assoc selected-desc-name face-options))) ; selected-face-pair is (Descriptive Name . Face Symbol)
                      (throw 'value  (cons (cdr selected-face-pair) (car selected-face-pair)))) ; Return (Face . Facename)
                  (progn
                    (message "未为类型 %s 选择外观. Using default." annotation-type)
                    (throw 'value  (cons 'memo-annotation-face-default "default"))))))
          (progn
            (message "There is no face get from type %s. Using default." annotation-type)
            (cons 'memo-annotation-face-default "default")))))))

;;; Management Functions (for extensibility)

(defun memo-annotation-register-face (annotation-type descriptive-name face-symbol)
  "Register a FACE-SYMBOL with DESCRIPTIVE-NAME for ANNOTATION-TYPE.
If ANNOTATION-TYPE does not exist in \'memo-annotation-type-face-map\',
it will be added.
If DESCRIPTIVE-NAME already exists for the ANNOTATION-TYPE, its associated
FACE-SYMBOL will be updated.

Example:
(memo-annotation-register-face memo-annotation-type-highlight
                                 \"蓝色高亮\" \'my-custom-blue-highlight-face)"
  (let ((type-entry (assoc annotation-type memo-annotation-type-face-map)))
    (if type-entry
        ;; Type exists, check if descriptive name exists
        (let ((face-entry (assoc descriptive-name (cdr type-entry))))
          (if face-entry
              (setcdr face-entry face-symbol) ; Update existing descriptive name's face
            ;; Add new descriptive name and face to existing type
            (setcdr type-entry (cons (cons descriptive-name face-symbol) (cdr type-entry)))))
      ;; Type does not exist, add new type entry with the face
      (add-to-list 'memo-annotation-type-face-map 
                   (list annotation-type (cons descriptive-name face-symbol))
                   t ; Append to the list
                   ))))

(defun memo-annotation-unregister-face (annotation-type descriptive-name)
  "Unregister the face associated with DESCRIPTIVE-NAME for ANNOTATION-TYPE.
Removes the (DESCRIPTIVE-NAME . FACE-SYMBOL) pair from the list for
the given ANNOTATION-TYPE.
If the ANNOTATION-TYPE has no more faces after unregistering, the type
entry itself can optionally be removed (current implementation does not remove empty types)."
  (let ((type-entry (assoc annotation-type memo-annotation-type-face-map)))
    (when type-entry
      (setcdr type-entry (delq (assoc descriptive-name (cdr type-entry)) (cdr type-entry))))))

(defun memo-get-faces-for-type (annotation-type)
  "Return a list of (Descriptive Name . Face Symbol) pairs for ANNOTATION-TYPE.
Returns nil if the type is not found or has no faces configured."
  (cdr (assoc annotation-type memo-annotation-type-face-map)))

(defun memo-get-all-annotation-types ()
  "Return a list of all registered annotation types (integer constants)."
  (mapcar #'car memo-annotation-type-face-map))


;;;---------------------------------------------------------------------
;;;  annotation type hook function for overlay
;;;---------------------------------------------------------------------
(defun memo-annotation-cloze--make-placeholder (original-text comment)
  "Generate a placeholder string based on COMMENT and ORIGINAL-TEXT.
The returned string will have the same length as ORIGINAL-TEXT.
It will start with '[' and end with ']'.
The content between the brackets is determined as follows:
- If ORIGINAL-TEXT length is less than 2, returns an empty string or adjusts.
  Specifically, if length is 0, returns \"\". If 1, returns \"[\".
- If COMMENT is empty, the inner part is filled with '-' characters.
- If COMMENT is not empty:
  - If COMMENT fits perfectly, it's used as is.
  - If COMMENT is shorter, it's padded with spaces (balanced) to fit.
  - If COMMENT is longer, it's truncated to fit."
  (let* ((original-text-len (length original-text)))
    (cond
     ((< original-text-len 1) "") ; Length 0
     ((< original-text-len 2) "[") ; Length 1, only space for opening bracket
     (t ; Length >= 2
      (let* ((target-inner-len (- original-text-len 2))
             (inner-content
              (if (string-empty-p comment)
                  (make-string target-inner-len ?-)
                (let ((comment-len (length comment)))
                  (cond
                   ((= comment-len target-inner-len)
                    comment)
                   ((< comment-len target-inner-len)
                    (let* ((padding-needed (- target-inner-len comment-len))
                           (left-padding-len (/ padding-needed 2))
                           (right-padding-len (- padding-needed left-padding-len))
                           (left-spaces (make-string left-padding-len ?\s))
                           (right-spaces (make-string right-padding-len ?\s)))
                      (concat left-spaces comment right-spaces)))
                   (t ; comment-len > target-inner-len
                    (substring comment 0 target-inner-len)))))))
        (concat "[" inner-content "]"))))))

;;; Cloze Hide/Unhide Functions
(defun memo-annotation-cloze-hide (overlay)
  "Hide the content of a cloze OVERLAY.
If OVERLAY is of type `memo-annotation-type-cloze` and not already hidden
by this mechanism, its display is replaced by a placeholder.
The original text is stored in the 'memo-cloze-original-text property,
and 'memo-cloze-is-hidden is set to t."
  (when (and (overlayp overlay)
             (eq (overlay-get overlay 'memo-annotation-type) memo-annotation-type-cloze)
             (not (overlay-get overlay 'memo-cloze-is-hidden)))
    (let* ((original-text (memo-annotation-overlay--get-original-text overlay))
	   (comment (memo-annotation-overlay--get-comment overlay))
           (placeholder (memo-annotation-cloze--make-placeholder original-text comment)))
      (overlay-put overlay 'memo-cloze-original-text original-text)
      (overlay-put overlay 'display placeholder)
      (overlay-put overlay 'memo-cloze-is-hidden t))))

(defun memo-annotation-cloze-unhide (overlay)
  "Unhide the content of a cloze OVERLAY.
Restores the original display if it was hidden by `memo-annotation-cloze-hide`.
This removes the 'display override and clears 'memo-cloze-is-hidden
and 'memo-cloze-original-text properties."
  (when (and (overlayp overlay)
             (eq (overlay-get overlay 'memo-annotation-type) memo-annotation-type-cloze)
             (overlay-get overlay 'memo-cloze-is-hidden))
    (overlay-put overlay 'display nil)
    (overlay-put overlay 'memo-cloze-is-hidden nil)
    (overlay-put overlay 'memo-cloze-original-text nil)))

(provide 'memo-annotation-type)
;;; memo-annotation-type.el ends here
