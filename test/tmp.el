;; 测试研究使用的代码
(load-file "/media/psf/my/my-memo/memo/memo.so")


(defun memo-update-create-note ()
  (interactive)
  (setq memo--property (memo--get-property-id))
  (message memo--ank)
  )


(defun memo--get-property-id ()
  (->> (org-ml-parse-headline-at (point))
       (org-ml-headline-get-node-property "ID")))


(defun memo--get-content ()
  (interactive)
  (->> (org-ml-parse-section-at (point))
       (org-ml-to-trimmed-string)
       )
  )

(defun memo-get-content ()
  (->> (org-ml-parse-this-headline)
       (org-ml-headline-get-contents (list :log-into-drawer t :clock-into-drawer t :clock-out-notes t))
       (org-ml-to-trimmed-string)))
