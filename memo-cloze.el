;;; memo-cloze.el --- The cloze deletion item type -*- lexical-binding:t -*-

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

;; This package implements cloze deletion review items in memo,
;; supporting hints, position-independent identifiers, and batch
;; (un)clozing.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'rx)

(require 'org)
(require 'org-element)

(defun memo-cloze-recenter-horizontally ()
  "Recenter the current line horizontally."
  (let ((offset (- (current-column) (truncate (window-width) 2))))
    (set-window-hscroll (selected-window) (max offset 0))))

(defcustom memo-cloze-centered-in-review-p nil
  "Non-nil means the cloze deletion in review will be centered in selected window."
  :group 'memo
  :type 'boolean)

(defconst memo-cloze-regexp
  (rx "{{" (group-n 1 (*? not-newline))
      (or "}}" (and "}{" (group (*? not-newline)) "}}"))))

(cl-defun memo-cloze-collect (&optional (begin (point-min)) (end (point-max)))
  "Collect all cloze deletions in the region from BEGIN to END."
  (save-excursion
    (goto-char begin)
    (cl-loop while (re-search-forward memo-cloze-regexp end t)
             collect (cl-list*
                      (read (match-string-no-properties 1)) (match-beginning 0)
                      (match-end 0) (match-string 1)
                      (when-let ((hint (match-string 2))) (list hint))))))

(defun memo-cloze-hidden-show (&optional text)
  (concat
   (propertize "[" 'face 'bold)
   (propertize (or text "....") 'face '(:background "red"))
   (propertize "]" 'face 'bold)))

(cl-defmethod memo-cloze-hidden ()
  "Hidden cloze content in current buffer."
  (cl-loop initially (memo-cloze-remove-overlays)
           for cloze in (memo-cloze-collect)
           for (text begin end string hint) = cloze
           do (memo-cloze-create-overlay begin end (memo-cloze-hidden-show))
           finally
           (goto-char (point-min))))


(defun memo-cloze-default (begin end &optional hint)
  "Cloze the region from BEGIN to END with HINT if HINT exist."
  (save-excursion
    (goto-char end)
    (if hint (insert "}{" hint "}}") (insert "}}"))
    (goto-char begin)
    (insert "{{")))

(defvar memo-cloze-hint nil)

;;;###autoload
(defun memo-cloze-dwim ()
  "Cloze the active region at point."
  (interactive)
  (if (use-region-p)
      (progn  (memo-cloze-default (region-beginning) (region-end))
	      (backward-char 2))
    (progn (memo-cloze-default (point) (point))
	   (forward-char 2))))

(cl-defun memo-cloze-bounds (&optional (position (point)))
  (save-excursion
    (cl-loop for function in '(re-search-backward re-search-forward)
             for match-bound in '(match-end match-beginning)
             for line-bound in (list (pos-bol) (pos-eol))
             if (funcall function memo-cloze-regexp line-bound t)
             if (<= (match-beginning 0) position (1- (match-end 0)))
             return (list (match-beginning 0) (match-end 0))
             else do (goto-char (funcall match-bound 0))
             else do (goto-char line-bound))))

(cl-defun memo-uncloze-default (begin end)
  (save-excursion
    (cl-loop initially (goto-char begin)
             while (re-search-forward memo-cloze-regexp end t)
             do (replace-match (match-string 1) t))))

;;;###autoload
(defun memo-uncloze-dwim ()
  "Uncloze the element at point. Also see `memo-cloze-dwim'."
  (interactive)
  (let ((cloze-region (memo-cloze-bounds)))
    (if cloze-region (apply 'memo-uncloze-default cloze-region))))


(defun memo-cloze-at-point ()
  "Get cloze at point."
  (save-excursion
    (goto-char (car (memo-cloze-bounds)))
    (cl-assert (looking-at memo-cloze-regexp))
    (list 'cloze (read (match-string 1)))))

(provide 'memo-cloze)
;;; memo-cloze.el ends here
