;;; memo-annotation.el --- Core functions of memo -*- lexical-binding: t; -*-

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
(require 'posframe) ; <-- Added require for posframe

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
;; 注意: 默认颜色主要在 memo-annotation-get-color 逻辑中使用
(defface memo-annotation-default-face
  '((((class color) (min-colors 88) (background light))
     :underline "#aecf90" :background "#ecf7ed")
    (t
     :background "#1d3c25"))
  "Default annotation face string to use when no specific face is set.")



;; 使用哈希表存储 headid (string) -> annotations (list of annotation objects) 的映射
(defvar memo-annotation--headid-map (make-hash-table :test 'equal)
  "Hash table mapping headline ID (string) to a list of annotation objects.")

;; 使用哈希表存储 annotation id (string) -> annotation object 的映射
(defvar memo-annotation--id-map (make-hash-table :test 'equal)
  "Hash table mapping annotation ID (string) to annotation object.")

(defun memo-annotation--get-by-headid (headid)
  "Get all annotations for the headline with HEADID.
Returns a list of annotation objects and initializes the hash tables."
  (let ((annotations (gethash headid memo-annotation--headid-map)))
    (if annotations
        annotations
      ;; 如果缓存中没有，则从服务器获取
      (let ((result (memo-api--get-annotations-by-headid headid)))
        (when result
          ;; 更新 headid -> annotations 映射
          (puthash headid result memo-annotation--headid-map)
          ;; 更新 id -> annotation 映射
          (dolist (anno result)
            (puthash (memo-annotation--id anno) anno memo-annotation--id-map)))
        result))))

(defun memo-annotation--update (annotation-object)
  "Update the annotation specified by ANNOTATION-OBJECT.
Updates both the cache and calls the API function."
  (when annotation-object
    ;; 更新缓存
    (let ((id (memo-annotation-id annotation-object))
          (headid (memo-annotation-headid annotation-object)))
      ;; 更新 id -> annotation 映射
      (puthash id annotation-object memo-annotation--id-map)
      ;; 更新 headid -> annotations 映射
      (let ((annotations (gethash headid memo-annotation--headid-map)))
        (when annotations
          (setq annotations
                (mapcar (lambda (anno)
                          (if (equal (memo-annotation-id anno) id)
                              annotation-object
                            anno))
                        annotations))
          (puthash headid annotations memo-annotation--headid-map)))
      ;; 调用 API 更新服务器数据
      (memo-api--update-annotation annotation-object)
      
      ;; 如果memo-annotation-mode已启用，更新对应的overlay
      (when memo-annotation-mode
        ;; 先移除旧的overlay
        (let ((old-overlay (cl-find-if (lambda (ov)
                                         (and (overlayp ov)
                                              (equal (overlay-get ov 'memo-annotation-id) id)))
                                       memo-annotation-overlays)))
          (when old-overlay
            (setq memo-annotation-overlays (delq old-overlay memo-annotation-overlays))
            (delete-overlay old-overlay))
        ;; 创建新的overlay
        (memo-annotation--create-overlay annotation-object))))))

(defun memo-annotation--delete (annotation-object)
  "Delete the annotation specified by ANNOTATION-OBJECT.
Removes from cache and calls the API function."
  (when annotation-object
    (let ((id (memo-annotation-id annotation-object))
          (headid (memo-annotation-headid annotation-object)))
      ;; 从 id -> annotation 映射中删除
      (remhash id memo-annotation--id-map)
      ;; 更新 headid -> annotations 映射
      (let ((annotations (gethash headid memo-annotation--headid-map)))
        (when annotations
          (setq annotations
                (cl-remove-if (lambda (anno)
                                (equal (memo-annotation-id anno) id))
                              annotations))
          (puthash headid annotations memo-annotation--headid-map)))
      ;; 调用 API 从服务器删除
      (memo-api--delete-annotation-by-id id)
      
      ;; 如果memo-annotation-mode已启用，移除对应的overlay
      (when memo-annotation-mode
        (let ((old-overlay (cl-find-if (lambda (ov)
                                         (and (overlayp ov)
                                              (equal (overlay-get ov 'memo-annotation-id) id)))
                                       memo-annotation-overlays)))
          (when old-overlay
            (setq memo-annotation-overlays (delq old-overlay memo-annotation-overlays))
            (delete-overlay old-overlay)))))))

(defun memo-annotation--create (headid start-pos end-pos anno-text comment-text face)
  "Create a new annotation in HEADID from START-POS to END-POS.
With original text ANNO-TEXT and comment text COMMENT-TEXT and FACE."
  ;; 调用 API 创建注释并获取返回的注释对象
  (let ((new-annotation (memo-api--create-annotation headid start-pos end-pos anno-text comment-text face)))
    (when new-annotation
      ;; 更新缓存
      (let ((id (memo-annotation-id new-annotation)))
        ;; 更新 id -> annotation 映射
        (puthash id new-annotation memo-annotation--id-map)
        ;; 更新 headid -> annotations 映射
        (let ((annotations (gethash headid memo-annotation--headid-map)))
          (if annotations
              (puthash headid (cons new-annotation annotations) memo-annotation--headid-map)
            (puthash headid (list new-annotation) memo-annotation--headid-map))))
      ;; 如果memo-annotation-mode已启用，为新注释创建overlay
      (when memo-annotation-mode
        (memo-annotation--create-overlay new-annotation))
      new-annotation)))

(defun memo-annotation--get-by-id (id)
  "Get annotation object by ID.
First tries to get from cache, then from server if not found."
  (or (gethash id memo-annotation--id-map)
      (let ((annotation (memo-api--get-annotation-by-id id)))
        (when annotation
          ;; 更新缓存
          (puthash id annotation memo-annotation--id-map)
          ;; 可能还需要更新 headid -> annotations 映射
          (let* ((headid (memo-annotation-headid annotation))
                 (annotations (gethash headid memo-annotation--headid-map)))
            (when annotations
              (unless (cl-find-if (lambda (anno)
                                    (equal (memo-annotation-id anno) id))
                                  annotations)
                (puthash headid (cons annotation annotations) memo-annotation--headid-map)))))
        annotation)))


;; 存储所有创建的overlay，用于后续清理
(defvar memo-annotation-overlays nil
  "List of all annotation overlays created by memo-annotation-mode.")

(defun memo-annotation--create-overlay (annotation-object)
  "Create an overlay for the given ANNOTATION-OBJECT.
Returns the created overlay."
  (when annotation-object
    (let* ((start (memo-annotation-start annotation-object))
           (end (memo-annotation-end annotation-object))
           (face (or (memo-string-to-face (memo-annotation-face annotation-object))
                     'memo-annotation-default-face))
           (overlay (ov-make start end)))
      ;; 设置overlay属性
      (ov-set overlay
              'face face
              'memo-annotation-id (memo-annotation-id annotation-object)
              'help-echo (format "Annotation: %s" (memo-annotation-text annotation-object))
              'memo-annotation t)
      ;; 添加到overlay列表中
      (push overlay memo-annotation-overlays)
      overlay)))

(defun memo-annotation--create-overlay (overlay)
  "Get the memo-annotation object associated with OVERLAY.
Returns nil if the overlay is not associated with an annotation."
  (when (and (overlayp overlay) (overlay-get overlay 'memo-annotation-id))
    (let ((anno-id (overlay-get overlay 'memo-annotation-id)))
      (memo-annotation--get-by-id anno-id))))

(defun memo-annotation--get-from-buffer ()
  "Get all memo-annotation objects from overlays in the current buffer.
Returns a list of memo-annotation objects."
  (let ((annotations nil)
        (headid (memo-annotation-get-current-headid)))
    (when headid
      ;; 获取当前buffer中所有的overlay
      (dolist (overlay memo-annotation-overlays)
        (let ((annotation (memo-annotation--create-overlay overlay)))
          (when annotation
            (push annotation annotations))))
      ;; 如果没有从overlay中获取到annotations，尝试从缓存或服务器获取
      (when (null annotations)
        (setq annotations (memo-annotation--get-by-headid headid))))
    annotations))

(defun memo-annotation-update-all-annotations-from-buffer ()
  "Update all annotations from overlays in the current buffer.
Reads all overlays, updates the hash tables, and calls the API to update the server."
  (interactive)
  (let ((annotations (memo-annotation--get-from-buffer))
        (headid (memo-annotation-get-current-headid)))
    (if (and headid annotations)
        (progn
          ;; 更新hash表中的annotations
          (puthash headid annotations memo-annotation--headid-map)
          ;; 更新id -> annotation映射
          (dolist (anno annotations)
            (puthash (memo-annotation--id anno) anno memo-annotation--id-map))
          ;; 调用API批量更新
          (memo-api--update-annotations (memo-make-alist-from-annotations annotations))
          (message "Updated %d annotations from buffer successfully" (length annotations)))
      (message "No annotations found in current buffer"))))

(defun memo-annotation-init-overlays (headid)
  "Initialize overlays for all annotations with HEADID.
   This function retrieves all annotations for the given headid
   and creates overlays for each one."
  (let ((annotations (memo-annotation--get-by-headid headid)))
    (when annotations
      (dolist (annotation annotations)
        (memo-annotation--create-overlay annotation)))))

(defun memo-annotation-init-current-buffer ()
  "Initialize overlays for the current buffer.
   This is a convenience function that can be called manually
   to initialize or refresh annotations in the current buffer."
  (interactive)
  (let ((headid (memo-annotation-get-current-headid)))
    (if headid
        (progn
          (memo-annotation-clear-overlays)
          (memo-annotation-init-overlays headid)
          (message "Initialized annotations for headid: %s" headid))
      (message "No headid found in current buffer"))))

(defun memo-annotation-clear-overlays ()
  "Clear all annotation overlays created by memo-annotation-mode."
  (when memo-annotation-overlays
    (dolist (overlay memo-annotation-overlays)
      (when (overlayp overlay)
        (delete-overlay overlay)))
    (setq memo-annotation-overlays nil)))

(defun memo-annotation-get-current-headid ()
  "Get the headid of the current buffer or section.
Returns nil if no headid is found."
  (when memo--buffer-local-note
    (memo-note-id memo--buffer-local-note)))

;;;###autoload
(define-minor-mode memo-annotation-mode
  "Minor mode to render memo text blocks using text properties with dynamic colors."
  :init-value nil
  :lighter " MemoAn" ; 可选：更改 lighter 字符串
  :keymap nil
  ;; 在 mode 启用时，初始化overlay
  (if memo-annotation-mode
      (progn
        ;; 清除之前的overlays，确保干净的状态
        (memo-annotation-clear-overlays)
        ;; 初始化当前buffer中的annotations
        (let ((headid (memo-annotation-get-current-headid)))
          (when headid
            (memo-annotation-init-overlays headid))))
     ;; 在 mode 禁用时清除所有overlay
     (memo-annotation-clear-overlays)))

;;;;;;;annotation operator function
(defun memo-annotation-get-face ()
  "Get face for new annotation.
This function can be customized to return different faces based on various conditions."
  'memo-annotation-default-face)

(defun memo-annotation-create-at-region ()
  "Create a new annotation for the selected region.
Uses the region text as the annotation source text."
  (interactive)
  (when (use-region-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (text (buffer-substring-no-properties start end))
           (headid (memo-annotation-get-current-headid)))
      (if headid
          (progn
            (memo-annotation--create headid start end text "" (memo-face-to-string (memo-annotation-get-face)))
            (deactivate-mark))
        (message "No headid found in current buffer")))))

(defun memo-annotation-delete-at-point ()
  "Delete the annotation at point if one exists."
  (interactive)
  (let ((overlays (overlays-at (point))))
    (cl-loop for overlay in overlays
             when (overlay-get overlay 'memo-annotation-id)
             do (let ((anno-id (overlay-get overlay 'memo-annotation-id)))
                  (let ((annotation (memo-annotation--get-by-id anno-id)))
                    (when annotation
                      (memo-annotation--delete annotation)
                      (message "Annotation deleted")))
                  (cl-return))
             finally (message "No annotation found at point"))))

(defun memo-annotation-edit-comment-at-point ()
  "Edit the comment of annotation at point if one exists."
  (interactive)
  (let ((overlays (overlays-at (point))))
    (cl-loop for overlay in overlays
             when (overlay-get overlay 'memo-annotation-id)
             do (let* ((anno-id (overlay-get overlay 'memo-annotation-id))
                       (annotation (memo-annotation--get-by-id anno-id))
                       (comment (memo-annotation-text annotation)))
		  (when (equal comment "")
		    (setq comment (memo-annotation-srctext annotation)))
                  (when annotation
                    (let ((new-comment (memo-get-content-from-posframe comment)))
                      (when new-comment
                        (setf (memo-annotation-text annotation) new-comment)
                        (memo-annotation--update annotation)
                        (message "Annotation comment updated"))))
                  (cl-return))
             finally (message "No annotation found at point"))))

(defun memo-annotation-update-all-annotations ()
  "Update all annotations in current buffer."
  (interactive)
  (let ((headid (memo-annotation-get-current-headid)))
    (if headid
        (let ((annotations (memo-annotation--get-by-headid headid)))
          (if annotations
              (progn
                (memo-api--update-annotations (memo-make-alist-from-annotations annotations))
                (message "Updated all annotations successfully"))
            (message "No annotations found for current headid")))
      (message "No headid found in current buffer"))))

(defun memo-annotation-show-comment-at-point ()
  "Show the comment of annotation at point using posframe.
Displays the annotation text in a posframe popup near the cursor."
  (interactive)
  (let ((overlays (overlays-at (point))))
    (cl-loop for overlay in overlays
             when (overlay-get overlay 'memo-annotation-id)
             do (let* ((anno-id (overlay-get overlay 'memo-annotation-id))
                       (annotation (memo-annotation--get-by-id anno-id))
                       (comment (memo-annotation-text annotation)))
                  (when (and annotation comment)
                    (posframe-show "*memo-annotation*"
                                  :string comment
                                  :position (point)
                                  :internal-border-width 1
                                  :internal-border-color "gray80"
                                  :background-color "#f0f0f0"
                                  :foreground-color "#303030"
                                  :timeout 5))
                  (cl-return))
             finally (message "No annotation found at point"))))



(provide 'memo-annotation)
;;; memo-annotation.el ends here
