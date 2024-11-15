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

(defun memo-remove-overlays ()
  "Remove all memo overlays in the current buffer."
  (remove-overlays (point-min) (point-max) 'category 'memo))

;; Based on `outline-flag-region'
(defun memo-hide-region (from to &optional text face)
  "Hide region FROM ... TO, optionally replacing it with TEXT.
FACE can be used to set the text face of the overlay, e.g. to
make it bold."
  ;; (remove-overlays from to 'category 'memo)
  (let ((o (make-overlay from to nil 'front-advance)))
    (overlay-put o 'category 'memo)
    (overlay-put o 'evaporate t)
    (if face (overlay-put o 'face face))
    (if (stringp text)
        (progn
          (overlay-put o 'invisible nil)
          (overlay-put o 'display text))
      (overlay-put o 'invisible t))
    o))

(defun memo-make-overlay (begin end &rest props)
  "Create an overlay from BEGIN to END with PROPS."
  (let ((o (make-overlay begin end)))
    (overlay-put o 'category 'memo)
    (cl-loop for (prop value) on props by #'cddr do
             (overlay-put o prop value))
    o))

(defun memo-overlay-surround (o before after &optional face)
  "Surround O with strings BEFORE and AFTER with optional FACE."
  (overlay-put o 'before-string (propertize before 'face face))
  (overlay-put o 'after-string (propertize after 'face face))
  o)


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




(provide 'memo-core)

;;; memo-core.el ends here

