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

(cl-defstruct memo-annotation id begin end  headid face text  srctext)

(defun memo-face-to-string (face)
  "Convert a face specification to a string representation.
   The face can be a face name (symbol) or a property list."
  (cond
   ((null face) "nil")
   ((symbolp face) (symbol-name face))
   ((listp face)
    (let ((str ""))
      (while face
        (let ((prop (pop face))
              (val (pop face)))
          (setq str (concat str
                           (if (> (length str) 0) " ")
                           (format ":%s %s"
                                   (symbol-name prop)
                                   (if (stringp val)
                                       (format "\"%s\"" val)
                                     val))))))
      (concat "(" str ")")))
   (t (format "%S" face))))

(defun memo-string-to-face (str)
  "Convert a string representation back to a face specification.
   The string should be in the format produced by `memo-face-to-string'."
  (when (and str (not (string-empty-p str)))
    (cond
     ((string= str "nil") nil)
     ((string-prefix-p "(" str)
      (condition-case nil
          (car (read-from-string str))
        (error nil)))
     (t (intern str))))))

;; 定义默认颜色字符串 (例如，使用 face 的背景色)
;; 这里我们用 face symbol, 但也可以用具体颜色字符串 "#ecf7ed"
;; 注意: 默认颜色主要在 memo-annotate-get-color 逻辑中使用
(defface memo-annotate-default-face  nil
  "Default annotation face string to use when no specific face is set.")

;; 使用哈希表存储 ID (string) -> Color (string) 的映射
(defvar memo-annotate-overlay-map (make-hash-table :test 'equal)
  "Hash table mapping annotation ID (string) to annotation face strings.")


;;;###autoload
(define-minor-mode memo-annotate-mode
  "Minor mode to render memo text blocks using text properties with dynamic colors."
  :init-value nil
  :lighter " MemoAn" ; 可选：更改 lighter 字符串
  :keymap nil
  ;; 在 mode 启用时，可以考虑清除或初始化 map（如果需要）
  ;; (clrhash memo-annotate-color-map) ; 如果每次启用都需要重置
  (if memo-annotate-mode
      (progn)
    ;; 在 mode 禁用时移除关键字
    (font-lock-remove-keywords nil memo-annotate-font-lock-keywords))
  (font-lock-flush)) ; 刷新以移除高亮

;; 4. 提供用于更新映射并刷新高亮的函数
(defun memo-annotate-update-color-map (headid)
  "Fetch child annotation colors for PARENT-HEADID from the backend
and update the local `memo-annotate-color-map`.
Refreshes font-lock highlighting."
  (interactive "sParent Head ID: ")
)

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

(defun memo-get-head-id-at-point (&optional pos)
  "Return the memo head ID associated with the text at POS (defaults to point).
Returns nil if no memo head ID property is found at POS.")



;;;  annotation action.
(defun memo-annotate-point-inside-p (&optional pos)
  "Return t if point (or POS if provided) is inside a memo annotation.
   Returns nil otherwise."
)


(defun memo-annotate-get-annotation-at-point (&optional pos)
  "Return a memo-annotation object if point (or POS if provided) is inside a memo annotation.
Returns nil otherwise.")

(defun memo-annotate-get-annotations-in-region (start end)
  "Return a list of memo-annotation objects in the region from START to END.
Returns nil if no annotations are found.
   This function also includes annotations that partially overlap with the region."
  ( ))


(provide 'memo-annotate)

;;; memo-annotate.el ends here



