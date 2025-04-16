;;; memo-core.el --- Core functions of memo -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Dancewhale

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
;;
;;; Code:

(require 'org-element)
(require 'cl)

;;; Working with Overlays / Hiding Text
;;;; Showing / Hiding Overlays
(defun memo-set-overlay-props (overlay &rest props)
  "Set an OVERLAY with PROPS.
\(memo-set-overlay-props overlay 'face '(:foreground \"red\" :background \"yellow\"))."
  (let ((o overlay))
    (cl-loop for (prop value) on props by #'cddr do
             (overlay-put o prop value))
    o))

(defun memo-set-overlay-text (text overlay)
  "Set the text of the OVERLAY to TEXT."
  (overlay-put overlay 'display  text))

(cl-defun memo-card-create-overlay (begin end &optional (text  "...") face)
  "Create card overlay from BEGIN to END, optionally replacing it with TEXT.
FACE can be used end set the text face of the overlay, e.g. end
make it bold."
  (cl-check-type text string)
  (let ((o (make-overlay begin end nil 'front-advance)))
    (memo-set-overlay-props o 'category 'memo-card 'evaporate t)
    (memo-set-overlay-props o 'invisible nil 'display text)
    (if face  (memo-set-overlay-props o 'face face))
    o))

(cl-defun memo-cloze-create-overlay (begin end &optional (text "..."))
  "Create a cloze overlay between BEGIN END with TEXT."
  (cl-check-type text string)
  (let ((o (make-overlay begin end)))
    (memo-set-overlay-props o 'category 'memo-cloze)
    (memo-set-overlay-props o 'display text)
    o))

(defun memo-overlay-surround (o before after &optional face)
  "Surround O with strings BEFORE and AFTER with optional FACE."
  (overlay-put o 'before-string (propertize before 'face face))
  (overlay-put o 'after-string (propertize after 'face face))
  o)

(cl-defun memo-card-remove-overlays (&optional (begin (point-min)) (end (point-max)))
  "Remove all card overlays in the region between BEGIN END."
  (remove-overlays begin end 'category 'memo-card))

(cl-defun memo-cloze-remove-overlays (&optional (begin (point-min)) (end (point-max)))
  "Remove all cloze overlays in the region between BEGIN END."
  (remove-overlays begin end 'category 'memo-cloze))

(cl-defun memo-remove-all-overlays (&optional (begin (point-min)) (end (point-max)))
  "Remove all overlays include card and cloze in the region between BEGIN END."
  (remove-overlays begin end 'category 'memo-cloze)
  (remove-overlays begin end 'category 'memo-card))

;;; Working with org-mode buffer Text
(defun memo-narrow-to-org-subtree-content (&optional element)
  "Narrow buffer to the current subtree."
  (interactive)
  (save-excursion
    (save-match-data
      (org-with-limited-levels
        (narrow-to-region
	   (progn (org-back-to-heading t)
	     (let ((elem (org-element-at-point)))
	       (+ 1 (org-element-property :robust-begin elem))))
	   (progn (org-next-visible-heading 1)
		  (when (and (org-at-heading-p) (not (eobp))) (backward-char 1))
		  (point)))))))


;;; Insert file id at begin of file.
(defun memo-generate-file-ids ()
  "Generate id and insert current file."
  (interactive)
  (save-excursion
   (goto-char (point-min)))
   (insert ":PROPERTIES:\n:ID: " (org-id-uuid) "\n:END:\n\n"))



(provide 'memo-core)

;;; memo-core.el ends here

