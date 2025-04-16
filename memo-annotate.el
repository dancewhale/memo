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
(require 'ov)
(require 'memo-api) ; <-- Added require for memo-api

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
     (t (intern str)))))

;; 定义默认颜色字符串 (例如，使用 face 的背景色)
;; 这里我们用 face symbol, 但也可以用具体颜色字符串 "#ecf7ed"
;; 注意: 默认颜色主要在 memo-annotate-get-color 逻辑中使用
(defface memo-annotate-default-face
  '((((class color) (min-colors 88) (background light))
     :underline "#aecf90" :background "#ecf7ed")
    (t
     :background "#1d3c25"))
  "Default annotation face string to use when no specific face is set.")



;; 使用哈希表存储 headid (string) -> annotations (list of annotation objects) 的映射
(defvar memo-annotate-headid-map (make-hash-table :test 'equal)
  "Hash table mapping headline ID (string) to a list of annotation objects.")

;; 使用哈希表存储 annotation id (string) -> annotation object 的映射
(defvar memo-annotate-id-map (make-hash-table :test 'equal)
  "Hash table mapping annotation ID (string) to annotation object.")

(defun memo-annotate-get-annotations-by-headid (headid)
  "Get all annotations for the headline with HEADID.
Returns a list of annotation objects and initializes the hash tables."
  (let ((annotations (gethash headid memo-annotate-headid-map)))
    (if annotations
        annotations
      ;; 如果缓存中没有，则从服务器获取
      (let ((result (memo-api--get-annotations-by-headid headid)))
        (when result
          ;; 更新 headid -> annotations 映射
          (puthash headid result memo-annotate-headid-map)
          ;; 更新 id -> annotation 映射
          (dolist (anno result)
            (puthash (memo-annotation-id anno) anno memo-annotate-id-map)))
        result))))

(defun memo-annotate-update-annotation (annotation-object)
  "Update the annotation specified by ANNOTATION-OBJECT.
Updates both the cache and calls the API function."
  (when annotation-object
    ;; 更新缓存
    (let ((id (memo-annotation-id annotation-object))
          (headid (memo-annotation-headid annotation-object)))
      ;; 更新 id -> annotation 映射
      (puthash id annotation-object memo-annotate-id-map)
      ;; 更新 headid -> annotations 映射
      (let ((annotations (gethash headid memo-annotate-headid-map)))
        (when annotations
          (setq annotations
                (mapcar (lambda (anno)
                          (if (equal (memo-annotation-id anno) id)
                              annotation-object
                            anno))
                        annotations))
          (puthash headid annotations memo-annotate-headid-map)))
      ;; 调用 API 更新服务器数据
      (memo-api--update-annotation annotation-object)
      
      ;; 如果memo-annotate-mode已启用，更新对应的overlay
      (when memo-annotate-mode
        ;; 先移除旧的overlay
        (let ((old-overlay (cl-find-if (lambda (ov)
                                         (and (overlayp ov)
                                              (equal (overlay-get ov 'memo-annotation-id) id)))
                                       memo-annotate-overlays)))
          (when old-overlay
            (setq memo-annotate-overlays (delq old-overlay memo-annotate-overlays))
            (delete-overlay old-overlay))
        ;; 创建新的overlay
        (memo-annotate-create-overlay-from-annotation annotation-object))))))

(defun memo-annotate-delete-annotation (annotation-object)
  "Delete the annotation specified by ANNOTATION-OBJECT.
Removes from cache and calls the API function."
  (when annotation-object
    (let ((id (memo-annotation-id annotation-object))
          (headid (memo-annotation-headid annotation-object)))
      ;; 从 id -> annotation 映射中删除
      (remhash id memo-annotate-id-map)
      ;; 更新 headid -> annotations 映射
      (let ((annotations (gethash headid memo-annotate-headid-map)))
        (when annotations
          (setq annotations
                (cl-remove-if (lambda (anno)
                                (equal (memo-annotation-id anno) id))
                              annotations))
          (puthash headid annotations memo-annotate-headid-map)))
      ;; 调用 API 从服务器删除
      (memo-api--delete-annotation-by-id id)
      
      ;; 如果memo-annotate-mode已启用，移除对应的overlay
      (when memo-annotate-mode
        (let ((old-overlay (cl-find-if (lambda (ov)
                                         (and (overlayp ov)
                                              (equal (overlay-get ov 'memo-annotation-id) id)))
                                       memo-annotate-overlays)))
          (when old-overlay
            (setq memo-annotate-overlays (delq old-overlay memo-annotate-overlays))
            (delete-overlay old-overlay)))))))

(defun memo-annotate-create-annotation (headid start-pos end-pos anno-text comment-text)
  "Create a new annotation in HEADID from START-POS to END-POS.
With original text ANNO-TEXT and comment text COMMENT-TEXT."
  ;; 调用 API 创建注释并获取返回的注释对象
  (let ((new-annotation (memo-api--create-annotation headid start-pos end-pos anno-text comment-text)))
    (when new-annotation
      ;; 更新缓存
      (let ((id (memo-annotation-id new-annotation)))
        ;; 更新 id -> annotation 映射
        (puthash id new-annotation memo-annotate-id-map)
        ;; 更新 headid -> annotations 映射
        (let ((annotations (gethash headid memo-annotate-headid-map)))
          (if annotations
              (puthash headid (cons new-annotation annotations) memo-annotate-headid-map)
            (puthash headid (list new-annotation) memo-annotate-headid-map))))
      ;; 如果memo-annotate-mode已启用，为新注释创建overlay
      (when memo-annotate-mode
        (memo-annotate-create-overlay-from-annotation new-annotation))
      new-annotation)))

(defun memo-annotate-get-annotation-by-id (id)
  "Get annotation object by ID.
First tries to get from cache, then from server if not found."
  (or (gethash id memo-annotate-id-map)
      (let ((annotation (memo-api--get-annotation-by-id id)))
        (when annotation
          ;; 更新缓存
          (puthash id annotation memo-annotate-id-map)
          ;; 可能还需要更新 headid -> annotations 映射
          (let* ((headid (memo-annotation-headid annotation))
                 (annotations (gethash headid memo-annotate-headid-map)))
            (when annotations
              (unless (cl-find-if (lambda (anno)
                                    (equal (memo-annotation-id anno) id))
                                  annotations)
                (puthash headid (cons annotation annotations) memo-annotate-headid-map)))))
        annotation)))


;; 存储所有创建的overlay，用于后续清理
(defvar memo-annotate-overlays nil
  "List of all annotation overlays created by memo-annotate-mode.")

(defun memo-annotate-create-overlay-from-annotation (annotation)
  "Create an overlay for the given ANNOTATION object.
   Returns the created overlay."
  (when annotation
    (let* ((start (memo-annotation-start annotation))
           (end (memo-annotation-end annotation))
           (face (or (memo-string-to-face (memo-annotation-face annotation))
                     'memo-annotate-default-face))
           (overlay (ov-make start end)))
      ;; 设置overlay属性
      (ov-set overlay
              'face face
              'memo-annotation-id (memo-annotation-id annotation)
              'help-echo (format "Annotation: %s" (memo-annotation-text annotation))
              'memo-annotation t)
      ;; 添加到overlay列表中
      (push overlay memo-annotate-overlays)
      overlay)))

(defun memo-annotate-init-overlays (headid)
  "Initialize overlays for all annotations with HEADID.
   This function retrieves all annotations for the given headid
   and creates overlays for each one."
  (let ((annotations (memo-annotate-get-annotations-by-headid headid)))
    (when annotations
      (dolist (annotation annotations)
        (memo-annotate-create-overlay-from-annotation annotation)))))

(defun memo-annotate-init-current-buffer ()
  "Initialize overlays for the current buffer.
   This is a convenience function that can be called manually
   to initialize or refresh annotations in the current buffer."
  (interactive)
  (let ((headid (memo-annotate-get-current-headid)))
    (if headid
        (progn
          (memo-annotate-clear-overlays)
          (memo-annotate-init-overlays headid)
          (message "Initialized annotations for headid: %s" headid))
      (message "No headid found in current buffer"))))

(defun memo-annotate-clear-overlays ()
  "Clear all annotation overlays created by memo-annotate-mode."
  (when memo-annotate-overlays
    (dolist (overlay memo-annotate-overlays)
      (when (overlayp overlay)
        (delete-overlay overlay)))
    (setq memo-annotate-overlays nil)))

(defun memo-annotate-get-current-headid ()
  "Get the headid of the current buffer or section.
   Returns nil if no headid is found."
  ;; 这个函数需要根据实际应用场景来实现
  ;; 例如，可以从当前buffer的属性中获取，或者从当前光标位置的org元素中获取
  ;; 下面是一个示例实现，假设headid存储在buffer-local变量中
  (when (boundp 'memo-current-headid)
    memo-current-headid))

;;;###autoload
(define-minor-mode memo-annotate-mode
  "Minor mode to render memo text blocks using text properties with dynamic colors."
  :init-value nil
  :lighter " MemoAn" ; 可选：更改 lighter 字符串
  :keymap nil
  ;; 在 mode 启用时，初始化overlay
  (if memo-annotate-mode
      (progn
        ;; 清除之前的overlays，确保干净的状态
        (memo-annotate-clear-overlays)
        ;; 初始化当前buffer中的annotations
        (let ((headid (memo-annotate-get-current-headid)))
          (when headid
            (memo-annotate-init-overlays headid))))
     ;; 在 mode 禁用时清除所有overlay
     (memo-annotate-clear-overlays)))


(provide 'memo-annotate)
;;; memo-annotate.el ends here
