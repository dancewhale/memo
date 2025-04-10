;;; memo-annotate.el --- Core functions of memo -*- lexical-binding: t; -*-

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
;;  <memo-head-id:uuid>TEXT</memo-head-id> annotation.
;;; Code:




(require 'font-lock)
(require 'rx)

(defface memo-annotate-face
  '((t :background "blue")) ; Example: Light gray background
  "Face for memo text content rendered by memo-annotate-mode.")

(defconst memo-annotate-regex
  (rx "<memo-head-id:"
      (group (one-or-more (in "0-9" "A-Z" "a-z" "-")))
      ">"
      (group (*? not-newline))
      "</memo-head-id>")
  "Regex to match the memo text block using rx syntax.")

(defconst memo-annotate-font-lock-keywords
  `((,memo-annotate-regex
     (1 (prog1 nil
         (put-text-property (match-beginning 0) (match-end 0) 'rear-nonsticky t)
         (put-text-property (match-beginning 0) (match-beginning 2) 'invisible t)))
     (2 (prog1 'memo-annotate-face
          (put-text-property (match-beginning 2) (match-end 2) 'memo-head-id (match-string-no-properties 1))))
     (0 (prog1 nil
         (put-text-property (match-end 2) (match-end 0) 'invisible t)))))
  "Font lock keywords for memo-annotate-mode.")

;;;###autoload
(define-minor-mode memo-annotate-mode
  "Minor mode to render memo text blocks using text properties."
  :init-value nil
  :lighter " MemoProp"
  :keymap nil
  (if memo-annotate-mode
      (progn
        (font-lock-add-keywords nil memo-annotate-font-lock-keywords t) ;; 添加 t 参数提高优先级
        (font-lock-mode 1)  ;; 确保字体锁定模式开启
        (font-lock-flush)
        (font-lock-ensure)) ;; 确保字体锁定应用到整个缓冲区
    (font-lock-remove-keywords nil memo-annotate-font-lock-keywords)
    (font-lock-flush)))

;; 添加调试函数
(defun memo-debug-annotate ()
  "Test the memo text property regex and highlight matches in current buffer."
  (interactive)
  (save-excursion
    (let ((count 0))
      (goto-char (point-min))
      (while (re-search-forward memo-annotate-regex nil t)
        (setq count (1+ count))
        (message "Match %d - ID: %s, Content: %s"
                 count 
                 (match-string-no-properties 1) 
                 (match-string-no-properties 2)))
      (if (= count 0)
          (message "No matches found for memo text property regex")
        (message "Found %d matches" count)))))

(defun memo-get-head-id-at-point (&optional pos)
  "Return the memo head ID associated with the text at POS (defaults to point).
Returns nil if no memo head ID property is found at POS."
  (get-text-property (or pos (point)) 'memo-head-id))

(provide 'memo-annotate)

;;; memo-annotate.el ends here

