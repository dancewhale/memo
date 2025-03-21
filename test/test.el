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


;(let* ((buf  (get-buffer-create memo-treemacs-buffer-name)))
;  (with-current-buffer memo-treemacs-buffer-name
;    (treemacs-update-async-node  "a97e5355-a74d-4b01-baa4-4b4c71cae63"  buf)))

; update function
;(closure (t)  (&optional btn item callback) 
;   (ignore btn item callback) 
;   (if (equal (progn (or (progn (and (memq (type-of item) cl-struct-memo-note-tags) t))
;			 (signal 'wrong-type-argument (list 'memo-note item))) (aref item 16)) 1) 
;       (progn (memo-api--get-virt-heads-by-parentid 
;	       (progn (or (progn (and (memq (type-of item) cl-struct-memo-note-tags) t)) 
;			  (signal 'wrong-type-argument (list 'memo-note item))) (aref item 1))))))
;
;;callback
;(lambda (items)
;    (treemacs--async-update-part-complete   path item-path items buffer))

(require 'treemacs)
(require 'treemacs-treelib)




