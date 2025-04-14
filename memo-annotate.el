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
(require 'memo-api) ; <-- Added require for memo-api

;(defface memo-annotate-default-face
;  '((t :background "LightGray")) ; Example: Light gray background for default
;  "Default face for memo annotations when ID is not in the dynamic map.")

(cl-defstruct memo-annotation  begin end id)

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

;; 定义默认颜色字符串 (例如，使用 face 的背景色)
;; 这里我们用 face symbol, 但也可以用具体颜色字符串 "#ecf7ed"
;; 注意: 默认颜色主要在 memo-annotate-get-color 逻辑中使用
(defvar memo-annotate-default-color "#ecf7ed"
  "Default background color string to use when no specific color is set.")

;; 使用哈希表存储 ID (string) -> Color (string) 的映射
(defvar memo-annotate-color-map (make-hash-table :test 'equal)
  "Hash table mapping memo head IDs (string) to background color strings.")

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
               ;; 使用新的获取颜色函数
               (color (memo-annotate-get-color id)))
          (prog1 (if (and color (not (string-equal color ""))) ; 确保 color 有效
                     `(face (:background ,color))
                   ;; 使用默认 face
                   'memo-annotate-default-face)
            ;; 设置 memo-head-id 属性
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
(defun memo-annotate-update-color-map (parent-headid)
  "Fetch child annotation colors for PARENT-HEADID from the backend
and update the local `memo-annotate-color-map`.
Refreshes font-lock highlighting."
  (interactive "sParent Head ID: ")
  (let ((color-alist (memo-api--get-child-annotation-color-map parent-headid)))
    (when (listp color-alist)
      (dolist (pair color-alist)
        (when (consp pair)
          (let ((id (car pair))
                (color (cdr pair)))
            (when (and (stringp id) (stringp color))
              (puthash id color memo-annotate-color-map))))))
    ;; 刷新高亮
    (when (bound-and-true-p memo-annotate-mode)
      (font-lock-flush))))

(defun memo-annotate-get-color (id)
  "Get the background color string for memo annotation with ID.
Checks local map first, then queries backend via `memo-api--get-annotation-color`.
Returns the specific color, or `memo-annotate-default-color` if none found.
Updates local map if color is fetched from backend."
  (or (gethash id memo-annotate-color-map)
      (let ((api-color (memo-api--get-annotation-color id)))
        (if (and api-color (not (string-equal api-color "")) (not (eq api-color 'null))) ; API 返回有效颜色
            (progn
              (puthash id api-color memo-annotate-color-map) ; 更新本地 map
              api-color) ; 返回 API 颜色
          ;; API 未返回有效颜色，返回默认颜色
          memo-annotate-default-color))))

(defun memo-annotate-set-color (id color)
  "Set the background COLOR (string) for memo annotation with ID (string).
Updates local map, calls backend API `memo-api--set-annotation-color`,
and refreshes font-lock highlighting."
  (interactive "sMemo ID: \nsColor: ")
  (puthash id color memo-annotate-color-map)
  ;; 调用 API 更新后端
  (memo-api--set-annotation-color id color)
  ;; 刷新高亮
  (when (bound-and-true-p memo-annotate-mode)
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



;;;  annotation action.
(defun memo-annotate-insert-tags (start end headid)
  "Insert memo annotation tags around region from START to END with HEADID.
   This will insert <memo-head-id:HEADID> before START and </memo-head-id> after END."
  (save-excursion
    (goto-char end)
    (insert "</memo-head-id>")
    (goto-char start)
    (insert (format "<memo-head-id:%s>" headid))
    ;; 刷新高亮
    (when (bound-and-true-p memo-annotate-mode)
      (font-lock-flush))))

(defun memo-annotate-point-inside-p (&optional pos)
  "Return t if point (or POS if provided) is inside a memo annotation.
   Returns nil otherwise."
  (let ((pos (or pos (point)))
        (inside nil))
    (save-excursion
      (goto-char pos)
      ;; 首先检查当前位置是否有memo-head-id属性
      (if (get-text-property pos 'memo-head-id)
          (setq inside t)
        ;; 如果没有，尝试向前搜索开始标签
        (let ((start-pos nil)
              (end-pos nil))
          (save-excursion
            ;; 向前搜索开始标签
            (when (re-search-backward memo-annotate-regex nil t)
              (setq start-pos (match-beginning 0))
              (setq end-pos (match-end 0))
              ;; 向后搜索结束标签
              (when (re-search-forward "</memo-head-id>" nil t)
                (when (and (> pos start-pos) (< pos (point)))
                  (setq inside t)))))))
      inside))

(defun memo-annotate-get-annotation-at-point (&optional pos)
  "Return a memo-annotation object if point (or POS if provided) is inside a memo annotation.
   Returns nil otherwise."
  (let ((pos (or pos (point))))
    (save-excursion
      (goto-char pos)
      ;; 首先检查当前位置是否有memo-head-id属性
      (if (get-text-property pos 'memo-head-id)
          (let ((id (get-text-property pos 'memo-head-id))
                (begin nil)
                (end nil))
            ;; 向前搜索找到开始位置
            (save-excursion
              (while (and (> (point) (point-min))
                          (get-text-property (1- (point)) 'memo-head-id)
                          (string= (get-text-property (1- (point)) 'memo-head-id) id))
                (backward-char 1))
              (setq begin (point)))
            ;; 向后搜索找到结束位置
            (save-excursion
              (while (and (< (point) (point-max))
                          (get-text-property (point) 'memo-head-id)
                          (string= (get-text-property (point) 'memo-head-id) id))
                (forward-char 1))
              (setq end (point)))
            (make-memo-annotation :begin begin :end end :id id))
        ;; 如果没有text property，尝试通过正则表达式查找
        (let ((start-pos nil)
              (end-pos nil)
              (id nil))
          (save-excursion
            ;; 向前搜索开始标签
            (when (re-search-backward memo-annotate-regex nil t)
              (setq start-pos (match-beginning 0))
              (setq id (match-string-no-properties 1))
              ;; 向后搜索结束标签
              (when (re-search-forward "</memo-head-id>" nil t)
                (setq end-pos (match-end 0))
                (when (and (> pos start-pos) (< pos end-pos))
                  (make-memo-annotation :begin start-pos :end end-pos :id id)))))))))

(defun memo-annotate-get-annotations-in-region (start end)
  "Return a list of memo-annotation objects in the region from START to END.
   Returns nil if no annotations are found.
   This function also includes annotations that partially overlap with the region."
  (let ((annotations nil)
        (start-anno nil)
        (end-anno nil))
    ;; 检查区域起始位置是否在某个注释内部
    (setq start-anno (memo-annotate-get-annotation-at-point start))
    (when start-anno
      (push start-anno annotations))
    ;; 检查区域结束位置是否在某个注释内部
    (setq end-anno (memo-annotate-get-annotation-at-point end))
    (when (and end-anno
               ;; 确保不是与start-anno相同的注释
               (or (not start-anno)
                   (not (string= (memo-annotation-id start-anno)
                                (memo-annotation-id end-anno)))))
      (push end-anno annotations))
    ;; 查找区域内的所有完整注释
    (save-excursion
      (goto-char start)
      (while (re-search-forward memo-annotate-regex end t)
        (let ((anno-start (match-beginning 0))
              (anno-end (match-end 0))
              (id (match-string-no-properties 1))
              (duplicate nil))
          ;; 检查是否与已有注释重复
          (dolist (anno annotations)
            (when (string= (memo-annotation-id anno) id)
              (setq duplicate t)))
          (unless duplicate
            (push (make-memo-annotation :begin anno-start :end anno-end :id id) annotations))))) 
    (nreverse annotations)))


(provide 'memo-annotate)

;;; memo-annotate.el ends here

