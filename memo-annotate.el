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

;(defface memo-annotate-default-face
;  '((t :background "LightGray")) ; Example: Light gray background for default
;  "Default face for memo annotations when ID is not in the dynamic map.")

;; 1. 定义默认 Face 和动态映射使用的哈希表
;; 这是默认高亮笔的定义，在浅色主题下使用浅绿色背景和下划线，在深色主题下使用深绿色背景。
(defface memo-annotate-default-face
  '((((class color) (min-colors 88) (background light))
     :underline "#aecf90" :background "#ecf7ed")
    (((class color) (min-colors 88) (background dark))
     :underline "#00422a" :background "#1d3c25")
    (t
     :inherit highlight))
  "Face for the default highlighter pen.")

;; 使用哈希表存储 ID (string) -> Color (string) 的映射
(defvar memo-annotate-color-map (make-hash-table :test 'equal)
  "Hash table mapping memo head IDs (string) to background color strings.")

;; 2. 辅助函数：根据 ID 获取颜色字符串
(defun memo-annotate--get-color-for-id (id)
  "Return the background color string associated with ID from `memo-annotate-color-map`.
Returns nil if ID is not found."
  (gethash id memo-annotate-color-map))

(defconst memo-annotate-regex
  (rx "<memo-head-id:"
      (group (one-or-more (in "0-9" "A-Z" "a-z" "-"))) ; Group 1: ID
      ">"
      (group (*? not-newline)) ; Group 2: Content
      "</memo-head-id>")
  "Regex to match the memo text block using rx syntax.")

;; 3. 修改 font-lock 关键字定义
(defconst memo-annotate-font-lock-keywords
  `((,memo-annotate-regex
     ;; Group 1 (ID): 不高亮，仅用于查找。隐藏起始标签部分。
     (1 (prog1 nil
           (put-text-property (match-beginning 0) (match-end 0) 'rear-nonsticky t)
           (put-text-property (match-beginning 0) (match-beginning 2) 'invisible t)))

     ;; Group 2 (Content): 应用动态颜色，并设置 text property.
     (2 (let* ((id (match-string-no-properties 1))
               (color (memo-annotate--get-color-for-id id)))
          ;; prog1 返回它的第一个参数（face spec 或 face symbol），
          ;; 同时执行其他表达式（设置 text property）。
          (prog1 (if color
                     ;; 如果找到颜色，返回一个 face 属性列表
                     `(face (:background ,color))
                   ;; 否则，返回默认的 face symbol
                   'memo-annotate-default-face)
            ;; 无论是否找到颜色，都设置 memo-head-id 属性
            (put-text-property (match-beginning 2) (match-end 2) 'memo-head-id id))))

     ;; Group 0 (整个匹配): 隐藏结束标签
     (0 (prog1 nil
           (put-text-property (match-end 2) (match-end 0) 'invisible t)))))
  "Font lock keywords for memo-annotate-mode.")

;;;###autoload
(define-minor-mode memo-annotate-mode
  "Minor mode to render memo text blocks using text properties with dynamic colors."
  :init-value nil
  :lighter " MemoAn" ; 可选：更改 lighter 字符串
  :keymap nil
  ;; 在 mode 启用时，可以考虑清除或初始化 map（如果需要）
  ;; (clrhash memo-annotate-color-map) ; 如果每次启用都需要重置
  (if memo-annotate-mode
      (progn
        (font-lock-add-keywords nil memo-annotate-font-lock-keywords 'append) ; 使用 'append 或 t
        (font-lock-mode 1)
        (font-lock-flush)  ; 确保启用时立即应用
        (font-lock-ensure))
    ;; 在 mode 禁用时移除关键字
    (font-lock-remove-keywords nil memo-annotate-font-lock-keywords)
    (font-lock-flush))) ; 刷新以移除高亮

;; 4. 提供用于更新映射并刷新高亮的函数
(defun memo-annotate-set-color (id color)
  "Set the background COLOR (string) for memo annotation with ID (string).
Refreshes font-lock highlighting in the current buffer."
  (interactive "sMemo ID: \nsColor: ") ; 示例交互式输入
  (puthash id color memo-annotate-color-map)
  ;; 关键：修改映射后调用 font-lock-flush 使更改生效
  (when (derived-mode-p 'memo-annotate-mode) ; 或检查 major-mode/buffer-local var
    (font-lock-flush)))

(defun memo-annotate-remove-color (id)
  "Remove the custom background color setting for memo annotation with ID.
It will revert to the default face. Refreshes font-lock highlighting."
  (interactive "sMemo ID to remove: ")
  (remhash id memo-annotate-color-map)
  (when (derived-mode-p 'memo-annotate-mode)
    (font-lock-flush)))

(defun memo-annotate-clear-colors ()
  "Remove all custom background color settings.
Refreshes font-lock highlighting."
  (interactive)
  (clrhash memo-annotate-color-map)
  (when (derived-mode-p 'memo-annotate-mode)
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

