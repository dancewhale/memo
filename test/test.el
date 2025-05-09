(defun treemacs-showcase--buffer-major-modes ()
  (->> (buffer-list)
       (--reject (string-prefix-p " " (buffer-name it)))
       (--map (buffer-local-value 'major-mode it))
       (-distinct)))

(defun treemacs-showcase--buffers-by-mode (mode)
  (->> (buffer-list)
       (--filter (eq mode (buffer-local-value 'major-mode it)))
       (--reject (string-prefix-p " " (buffer-name it)))))

(treemacs-define-entry-node-type showcase-async-buffers
  :key 'showcase-buffers-async
  :label (propertize"Async Buffers" 'face 'font-lock-keyword-face)
  :open-icon (treemacs-get-icon-value 'list)
  :closed-icon (treemacs-get-icon-value 'list)
  :children
  (let ((items (treemacs-showcase--buffer-major-modes)))
         (funcall callback items))
  :child-type 'showcase-async-buffer-group
  :async? t)

(treemacs-define-expandable-node-type showcase-async-buffer-group
  :closed-icon "+ "
  :open-icon "- "
  :label (propertize (symbol-name item) 'face 'font-lock-variable-name-face)
  :key item
  :on-expand (message "Expanding node with key %s" (treemacs-button-get btn :key))
  :children
  (let ((items (treemacs-showcase--buffers-by-mode (treemacs-button-get btn :major-mode))))
        (funcall callback items))
  :child-type 'showcase-buffer-leaf
  :more-properties `(:major-mode ,item)
  :async? t)

(treemacs-define-leaf-node-type showcase-buffer-leaf
  :icon "• "
  :label (propertize (or (buffer-name item) "#<killed buffer>")
                     'face 'font-lock-string-face)
  :key item
  :more-properties `(:buffer ,item)
  :visit-action #'treemacs-showcase-visit-buffer-action
  :ret-action #'treemacs-showcase-RET-buffer-action)


(treemacs-enable-project-extension
 :extension 'showcase-async-buffers
 :position 'bottom)

(defun treemacs-showcase-RET-buffer-action (&optional _)
  (let ((buffer (-some-> (treemacs-current-button)
                  (treemacs-button-get :buffer))))
    (when (buffer-live-p buffer)
      (pop-to-buffer buffer))))

(defun treemacs-showcase-visit-buffer-action (btn)
  (let ((buffer (treemacs-safe-button-get btn :buffer)))
    (when (buffer-live-p buffer)
      (pop-to-buffer buffer))))

(treemacs-define-entry-node-type showcase-buffers
  :label (propertize "Buffers" 'face 'font-lock-keyword-face)
  :key 'showcase-buffers
  :open-icon (treemacs-get-icon-value 'list)
  :closed-icon (treemacs-get-icon-value 'list)
  :children (treemacs-showcase--buffer-major-modes)
  :child-type 'showcase-buffer-group)




(treemacs-define-expandable-node-type showcase-buffer-group
  :closed-icon "+ "
  :open-icon "- "
  :label (propertize (symbol-name item) 'face 'font-lock-variable-name-face)
  :key item
  :children (treemacs-showcase--buffers-by-mode (treemacs-button-get btn :major-mode))
  :child-type 'showcase-buffer-leaf
  :more-properties `(:major-mode ,item)
  :on-expand (message "Expanding node with key %s" (treemacs-button-get btn :key))
  :on-collapse (message "Collapsing node with key %s" (treemacs-button-get btn :key)))


(treemacs--scope-store)
(treemacs-get-local-buffer)

(memo-api--get-first-file-head "498F1DB0-0FD3-4126-8D54-0059A408FF61")

;(progn (with-current-buffer memo-treemacs-buffer-name
;	 (treemacs-update-node "memo-treemacs-generic-root")
;	 (treemacs-with-current-button "Error not button selected." (treemacs-button-get current-btn :key))))
;
(let* ((buf (get-buffer memo-treemacs-buffer-name)))
  (with-current-buffer buf
    (treemacs-update-async-node '("memo-treemacs-review-mode-node" "VirtHeadTree") buf)))

(let* ((buf (get-buffer-create memo-treemacs-buffer-name)))
  (with-current-buffer buf
    (treemacs-update-async-node '("memo-treemacs-review-mode-node" "CurrentReviewNote")  buf)))

(let* ((buf  (treemacs-get-local-buffer)))
  (with-current-buffer buf
    (with-selected-window (treemacs-get-local-window)
      (treemacs-update-async-node (treemacs-safe-button-get (treemacs-current-button) :path) buf))))

(let* ((buf  (get-buffer-create memo-treemacs-buffer-name)))
  (with-current-buffer buf
    (prin1 treemacs-dom)
    (treemacs-update-async-node '("/Users/whale/Dropbox/memo" showcase-buffers-async) buf)))

(let ((buf (get-buffer memo-treemacs-file-buffer-name)))
  (with-current-buffer buf
    (treemacs-goto-extension-node '("variadic-entry-node" "1ad3025f-c304-4227-9765-ba88905380e2" "9828BCBD-6BE3-418B-A5E9-47CAA371AB98"))))

(-if-let* ((readbuf (get-buffer "*memo-review*"))
	   (treebuf (get-buffer memo-treemacs-file-buffer-name)))
    (with-current-buffer readbuf
      (let* ((note-object memo--buffer-local-note)
	     (fileid (memo-note-fileid note-object))
	     (path (append '("variadic-entry-node") (memo-note-path note-object)))
	     (noteid (memo-note-id note-object)))
	(with-current-buffer treebuf
	  (prin1 (memo-note-path note-object))
	  (prin1 (memo-note-headlineid note-object))))))

ceshi

(memo-treemacs--find-node-by-id "04E683D9-48D7-4F54-9DB3-065C8BCC17A6")

(treemacs-goto-extension-node (append path noteid))

(memo-api--get-string "jjjj")
(memo-api--get-first-file-head "7A355DB0-65BA-4340-8588-18A3F247D2F8")
(memo-api--get-first-file-head "04E683D9-48D7-4F54-9DB3-065C8BCC17A6")

(defun get-cursor-position-in-buffer-window (buffer-name)
  "获取名字为 BUFFER-NAME 的缓冲区所在窗口中的光标位置。"
  (let ((target-buffer (get-buffer buffer-name))) ; 查找目标缓冲区
    (if (not target-buffer)
        (message "缓冲区 '%s' 不存在" buffer-name)
      (let ((target-window (get-buffer-window target-buffer))) ; 获取缓冲区关联的窗口
        (if (not target-window)
            (message "缓冲区 '%s' 没有显示在任何窗口中" buffer-name)
          (let ((cursor-pos (window-point target-window))) ; 获取光标位置
            (message "缓冲区 '%s' 的窗口中光标位置为：%d" buffer-name cursor-pos)
            cursor-pos)))))) ; 返回光标位置

(memo-api--get-read-files)

(memo-api--get-property "7A355DB0-65BA-4340-8588-18A3F247D2F8" "MEMO_NOTE_TYPjE")
(memo-api--update-property "7A355DB0-65BA-4340-8588-18A3F247D2F8" "MEMO_NOTE_COLOR" testmap)

(setq jj (read "#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data (\"tset\" \"jjjj\"))"))

(memo-select-face-for-annotation-type 0)
(assoc 0 memo-annotation-type-face-map)
(puthash "jj" "jjj" jj)
(puthash "jjj2" "jjj" jj)
(puthash "jjj3" "jjj" jj)

(cl-defstruct memo-annotation id start end  headid face text  srctext)
(setq test  (make-memo-annotation :id "ID" :start "start"))
(memo-struct-to-alist test)

(setq test '((pine . "cones")
	     (oak . "acorns")
	     (maple . "seeds")))

(setq testmap (make-hash-table :test 'equal))
(puthash "jj" "jjj" testmap)
(puthash "jjj2" "jjj" testmap)
(puthash "jjj3" "jjj" testmap)
(puthash "tset" "jjjj" testmap)

(memo-select-face-for-annotation-type 3)

(memo-annotation--clear-cache)

(memo-buffer-get-content-from-posframe "jjj")


(require 'treemacs)
(require 'treemacs-treelib)

(memo-annotation--get-by-id 4)

(display-buffer (get-buffer-create memo-treemacs-file-buffer-name)
	    `(display-buffer-in-side-window . (,@memo-treemacs-file-position-params)))

(memo-query-next-note)

(posframe-show (get-buffer-create "test")
	       ;:string "测试"
               :border-color "#ee7b29"
               :border-width 2
               :poshandler 'posframe-poshandler-frame-center
               :height (round(* (frame-height) 0.50))
               :width (round(* (frame-width) 0.50))
               :override-parameters '((cursor-type t))
               :respect-header-line t
               :accept-focus t
	       :hidehandler 'memo-buffer--posframe-hidehandler-when-buffer-switch)

(message (memo-buffer-get-content-from-posframe ))

(memo-api--annotation-get-comment 22)
