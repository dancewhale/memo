;;; memo-card.el --- The card deletion item type -*- lexical-binding:t -*-

;; Copyright (C) 2024 Bohong Huang


;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; This package implements card deletion review items in memo,
;; supporting hints, position-independent identifiers, and batch
;; (un)clozing.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'rx)

(require 'org)

(defvar memo-card-regexp
  (rx (seq bol (one-or-more "-") eol)
      (group-n 1  (one-or-more (or any "\n" "\|")))))

(cl-defun  memo-card-collect (&optional (begin (point-min)) (end (point-max)))
"Hidden card content betwwen BEGIN to END."
  (save-excursion
    (goto-char begin)
    (if (re-search-forward memo-card-regexp end t)
	(list (match-beginning 1) (match-end 1))
	nil)))

(cl-defun memo-card-hidden-show (&optional (text "..............."))
  (concat
   (propertize "[" 'face 'bold)
   (propertize text 'face '(:background "red"))
   (propertize "]" 'face 'bold)))

(defun memo-card-hidden ()
  "Hidden card content in current buffer."
  (memo-card-remove-overlays)
  (let* ((card-back-position (memo-card-collect)))
    (if card-back-position
	(progn
	  (let* ((begin (car card-back-position))
		 (end (cadr card-back-position)))
	    (memo-card-create-overlay begin end (memo-card-hidden-show)))))))

(defun memo-card-show ()
  "Remove card overlay and show content."
  (memo-card-remove-overlays)
)


(provide 'memo-card)
;;; memo-card.el ends here
