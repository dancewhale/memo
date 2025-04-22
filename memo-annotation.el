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
;;  
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
  "Convert a string STR representation back to a face specification.
The string should be in the format produced by `memo-face-to-string'."
  (when (and str (not (string-empty-p str)))
    (cond
     ((string= str "nil") nil)
     ((string-prefix-p "(" str)
      (condition-case nil
          (car (read-from-string str))
        (error nil)))
     (t (intern str)))))

;;---------------------------------------------------------
;; annotation overlay region change
;;---------------------------------------------------------



;;---------------------------------------------------------
;; annotation operator function
;;---------------------------------------------------------
;; 定义默认颜色字符串 (例如，使用 face 的背景色)
;; 这里我们用 face symbol, 但也可以用具体颜色字符串 "#ecf7ed"
;; 注意: 默认颜色主要在 memo-annotation-get-color 逻辑中使用
(defface memo-annotation-default-face
  '((((class color) (min-colors 88) (background light))
     :underline "#aecf90" :background "#ecf7ed")
    (t
     :background "#1d3c25"))
  "Default annotation face string to use when no specific face is set.")

(defun memo-annotation--get-face ()
  "Get face for new annotation.
This function can be customized to return different faces based on various conditions."
  'memo-annotation-default-face)

;; 使用哈希表存储 headid (string) -> annotations (list of annotation objects) 的映射
(defvar memo-annotation--headid-map (make-hash-table :test 'equal)
  "Hash table mapping headline ID (string) to a list of annotation objects.")

(defun memo-annotation--get-current-headid ()
  "Get the headid of the current buffer or section.
Returns nil if no headid is found."
  (when memo--buffer-local-note
    (memo-note-id memo--buffer-local-note)))

(defun memo-annotation--get-by-headid (headid)
  "Return a list of annotation objects by HEADID and initialize the hash tables."
  (let ((annotations (gethash headid memo-annotation--headid-map)))
    (if (or (not annotations) (hash-table-empty-p annotations))
        ;; 如果缓存中没有，则从服务器获取
        (let ((result (memo-api--get-annotations-by-headid headid))
	      (memo-annotation--map (make-hash-table :test 'equal)))
          (when result
          ;; 更新 id -> annotation 映射
            (dolist (anno result)
              (puthash (memo-annotation-id anno) anno memo-annotation--map))
          ;; 更新 headid -> annotations hash 映射
            (puthash headid memo-annotation--map memo-annotation--headid-map))
          memo-annotation--map)
      annotations)))

(defun memo-annotation--update-db (annotation-object)
  "Update the annotation specified by ANNOTATION-OBJECT.
Updates both the cache and calls the API function."
  (when annotation-object
    ;; 更新缓存
    (-if-let* ((id (memo-annotation-id annotation-object))
               (headid (memo-annotation-headid annotation-object)))
      ;; 更新 id -> annotation 映射
	(let* ((memo-annotation--map (memo-annotation--get-by-headid headid)))
	    (puthash id annotation-object memo-annotation--map)
	    ;; 更新 headid -> annotations hash 映射
	    (puthash headid memo-annotation--map memo-annotation--headid-map)
	    ;; 调用 API 更新服务器数据
	    (memo-api--update-annotation annotation-object)))))

(defun memo-annotation--delete-db (annotation-object)
  "Delete the annotation specified by ANNOTATION-OBJECT.
Removes from cache and calls the API function."
  (when annotation-object
    (-if-let* ((id (memo-annotation-id annotation-object))
               (headid (memo-annotation-headid annotation-object)))
	(let* ((memo-annotation--map (memo-annotation--get-by-headid headid)))
	  ;; 从 id -> annotation 映射中删除
	  (remhash id memo-annotation--map)
	  ;; 更新 headid -> annotations hash 映射
	  (puthash headid memo-annotation--map memo-annotation--headid-map)
	  ;; 调用 API 从服务器删除
	  (memo-api--delete-annotation-by-id id)))))

(defun memo-annotation--create-db (headid start-pos end-pos anno-text comment-text face)
  "Create a new annotation in HEADID from START-POS to END-POS.
With original text ANNO-TEXT and comment text COMMENT-TEXT and FACE."
  ;; 调用 API 创建注释并获取返回的注释对象
  (let ((new-annotation (memo-api--create-annotation headid start-pos end-pos anno-text comment-text face)))
    (when new-annotation
      ;; 更新缓存
      (let ((id (memo-annotation-id new-annotation))
	    (memo-annotation--map (memo-annotation--get-by-headid headid)))
        ;; 更新 id -> annotation 映射
        (puthash id new-annotation memo-annotation--map)
        ;; 更新 headid -> annotations hash 映射
        (puthash headid memo-annotation--map memo-annotation--headid-map)))
    new-annotation))

(defun memo-annotation--clear-cache ()
  "Clear the annotation hash table which store ANNOTATION-OBJECT."
      (clrhash memo-annotation--headid-map))

(defun memo-annotation--get-by-id-in-cache (annotation-id)
  "Get the annotation in cache map: MEMO-ANNOTATION--HEADID-MAP by ANNOTATION-ID."
  (let ((headids (hash-table-keys memo-annotation--headid-map))
	(annotation nil))
    (when headids
      (dolist (headid  headids)
	(let* ((memo-annotation--map (gethash headid memo-annotation--headid-map))
	       (ids (hash-table-keys memo-annotation--map)))
	  (dolist (id ids)
	    (if (equal id annotation-id)
		(setq annotation (gethash annotation-id memo-annotation--map)))))))
   annotation))

(defun memo-annotation--get-by-id (id)
  "Get annotation object by ID.
First tries to get from cache, then from server if not found."
  (or (memo-annotation--get-by-id-in-cache)
      (let ((annotation (memo-api--get-annotation-by-id id)))
        (when annotation
	  (let ((memo-annotation--map
		 (memo-annotation--get-by-headid (memo-annotation-headid annotation))))
            ;; 更新缓存
            (puthash id annotation memo-annotation--map)
            ;; 可能还需要更新 headid -> annotations 映射
            (puthash headid memo-annotation--map memo-annotation--headid-map)))
        annotation)))


;;---------------------------------------------------------
;; overlay operator function
;;---------------------------------------------------------
;; 存储所有创建的overlay，用于后续清理
(defvar memo-annotation--overlays-map (make-hash-table :test 'equal)
  "Hash of all annotation overlays created by memo-annotation-mode.")

(defun memo-annotation--overlay-create (annotation-object)
  "Create an overlay for the given ANNOTATION-OBJECT.
Put overlay in memo-annotation--overlays-map hash-table,
Returns the created overlay."
  (when annotation-object
    (let* ((start (memo-annotation-start annotation-object))
           (end (memo-annotation-end annotation-object))
	   (id  (memo-annotation-id annotation-object))
           (face (or (memo-string-to-face (memo-annotation-face annotation-object))
                     'memo-annotation-default-face))
           (overlay (ov-make start end)))
      ;; 设置overlay属性
      (ov-set overlay
              'face face
              'memo-annotation-id (memo-annotation-id annotation-object)
              'memo-annotation-headid (memo-annotation-headid annotation-object)
	      'memo-annotation-comment (memo-annotation-text annotation-object)
              'memo-annotation t)
      ;; 添加到overlay列表中
      (puthash id overlay memo-annotation--overlays-map)
      overlay)))

(defun memo-annotation--overlay-create-annotation (overlay)
  "Update a annotation for the given OVERLAY."
  (when (and  (ov-p overlay) (ov-buf overlay))
    (let ((anno (make-memo-annotation :id (ov-val overlay 'memo-annotation-id)
				      :start (ov-beg overlay)
				      :end (ov-end overlay)
				      :face (memo-face-to-string (ov-val overlay 'face))
				      :headid (ov-val overlay 'memo-annotation-headid)
				      :text (ov-val overlay 'memo-annotation-comment)
				      :srctext (buffer-substring-no-properties (ov-beg overlay) (ov-end overlay)))))
     anno)))

(defun memo-annotation--overlay-delete (annotation-object)
  "Delete an overlay for the given ANNOTATION-OBJECT.
Remove overlay in memo-annotation--overlays-map hash-table,
Returns t or nil."
  (when annotation-object
    (let* ((headid (memo-annotation-headid annotation-object))
           (annotation-id (memo-annotation-id annotation-object)))
      (delete-overlay (gethash annotation-id memo-annotation--overlays-map))
      ;; 从hash table 中删除overlay
      (remhash annotation-id memo-annotation--overlays-map)
      t)))

(defun memo-annotation--overlay-get-by-id (annotation-id)
  "Get an overlay for the given ANNOTATION-ID from.
memo-annotation--overlays-map hash-table."
  (gethash annotation-id memo-annotation--overlays-map))

(defun memo-annotation--overlay-update (annotation-object)
  "Update an overlay by the given ANNOTATION-OBJECT.
Update overlay in memo-annotation--overlays-map hash-table,
Returns the overlay."
  (when annotation-object
    (let* ((annotation-id (memo-annotation-id annotation-object))
	   (annotation-headid (memo-annotation-headid annotation-object))
	   (annotation-start (memo-annotation-start annotation-object))
	   (annotation-end (memo-annotation-end annotation-object))
           (face (or (memo-string-to-face (memo-annotation-face annotation-object))
                     'memo-annotation-default-face))
           (overlay (gethash annotation-id memo-annotation--overlays-map)))
      ;; 移动overlay 位置
      (unless (and (equal annotation-start (ov-beg overlay))
		   (equal annotation-end (ov-end overlay)))
	(ov-move overlay annotation-start annotation-end))
      ;; 设置overlay属性
      (ov-set overlay
              'face face
              'memo-annotation t)
      ;; 添加到overlay列表中
      (puthash memo-annotation-id overlay memo-annotation--overlays-map)
      overlay)))

;;---------------------------------------------------------
;; overlay and annotation operator function
;;---------------------------------------------------------
(defun memo-annotation-overlays-clear ()
  "Clear all annotation overlays created by memo-annotation-mode."
  (let ((ov-keys (hash-table-keys memo-annotation--overlays-map)))
    (when ov-keys
      (dolist (ov-key ov-keys)
	(let ((overlay (gethash ov-key memo-annotation--overlays-map)))
          (delete-overlay overlay)))))
  (clrhash memo-annotation--overlays-map))


(defun memo-annotation-overlays-init ()
  "Initialize overlays for all annotations.
This function retrieves all annotations for the given headid
   and creates overlays for each one."
  (-if-let* ((headid (memo-annotation--get-current-headid))
             (anno-hash (memo-annotation--get-by-headid headid)))
      (progn
	(memo-annotation-overlays-clear)
	(-if-let* ((anno-keys (hash-table-keys anno-hash)))
	    (dolist (anno-key anno-keys)
              (memo-annotation--overlay-create (gethash anno-key anno-hash)))))))

(defun memo-annotation-overlays-save ()
  "Update all memo-annotation objects which attach overlay in the current buffer."
  (interactive)
  ;; 获取当前buffer中所有的overlay
  (-if-let ((ov-keys (hash-table-keys memo-annotation--overlays-map)))
      (dolist (ov-key ov-keys)
	(let* ((overlay (gethash ov-key memo-annotation--overlays-map))
	       (anno (memo-annotation--overlay-create-annotation overlay)))
	  (memo-annotation--update-db anno)))))

;;---------------------------------------------------------
;; memo annotation operator function in buffer.
;;---------------------------------------------------------

;;;###autoload
(define-minor-mode memo-annotation-mode
  "Minor mode to render memo text blocks using text properties with dynamic colors."
  :init-value nil
  :lighter " MemoAn" ; 可选：更改 lighter 字符串
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-a r") #'memo-annotation-adjust-region-at-point)
            (define-key map (kbd "C-c C-a c") #'memo-annotation-create-at-region)
            (define-key map (kbd "C-c C-a d") #'memo-annotation-delete-at-point)
            (define-key map (kbd "C-c C-a e") #'memo-annotation-edit-comment-at-point)
            (define-key map (kbd "C-c C-a s") #'memo-annotation-show-comment-at-point)
            map)
  ;; 在 mode 启用时，初始化overlay
  (if memo-annotation-mode
      (progn
        ;; 清除之前的overlays，确保干净的状态
        (memo-annotation-overlays-clear)
        ;; 初始化当前buffer中的annotations
        (memo-annotation-overlays-init)
        ;; 加载依赖的区域调整功能
        (require 'memo-annotation-region))
     ;; 在 mode 禁用时清除所有overlay
     (memo-annotation-overlays-clear)))

(defun memo-annotation-create-at-region ()
  "Create a new annotation for the selected region.
Uses the region text as the annotation source text."
  (interactive)
  (when (use-region-p)
    (-if-let* ((start (region-beginning))
           (end (region-end))
           (text (buffer-substring-no-properties start end))
           (headid (memo-annotation--get-current-headid)))
	(progn
          (memo-annotation--overlay-create
            (memo-annotation--create-db headid start end text "" (memo-face-to-string (memo-annotation--get-face))))
          (deactivate-mark))
        (message "No headid found in current buffer"))))

(defun memo-annotation-delete-at-point ()
  "Delete the annotation at point if one exists."
  (interactive)
  (let ((overlays (overlays-at (point))))
    (cl-loop for overlay in overlays
             when (overlay-get overlay 'memo-annotation-id)
             do (let ((anno-id (overlay-get overlay 'memo-annotation-id)))
                  (let ((annotation (memo-annotation--get-by-id anno-id)))
                    (when annotation
		      (memo-annotation--overlay-delete overlay)
                      (memo-annotation--delete-db annotation)
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
                        (memo-annotation--update-db annotation)
                        (message "Annotation comment updated"))))
                  (cl-return))
             finally (message "No annotation found at point"))))

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

(defun memo-annotation-overlays-save-batch ()
  "Save the overlay to db in the current buffer.
Reads all overlays, updates the hash tables, and calls the API to update the server."
  (interactive)
  (-if-let* ((headid (memo-annotation--get-current-headid))
	     (memo-annotation--map (memo-annotation--get-by-headid headid))
	     (ov-table-keys (hash-table-keys memo-annotation--overlays-map)))
      (progn
        ;; 更新id -> annotation映射
        (dolist (ov-key  ov-table-keys)
	  (let* ((overlay (gethash ov-key memo-annotation--overlays-map))
		(anno (memo-annotation--overlay-create-annotation overlay)))
	    (if (equal (memo-annotation-headid anno) headid)
		(puthash (memo-annotation-id anno) anno memo-annotation--map))))
        ;; 调用API批量更新
        (memo-api--update-annotations (memo-make-list-from-annotations-table memo-annotation--map))
        t)
      (message "No annotations found in current buffer")))

(provide 'memo-annotation)
;;; memo-annotation.el ends here
