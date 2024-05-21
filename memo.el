(setq load-path (append load-path (list (file-truename "~/cao/memo/golib"))))

(load-file "~/cao/memo/golib/memo.so")
(setq testj (memo-create-card "font test" "back test"))


