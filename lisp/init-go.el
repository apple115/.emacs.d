;;;init-go.el

(use-package go-ts-mode
:ensure nil
:mode "\\.go\\'"
:config
(setq go-ts-mode-indent-offset 4)
)

(use-package gotest
  :ensure t
  :general
  (:keymaps 'go-ts-mode-map
  :states '(normal)
  :prefix  "SPC"
  "m" '(nil :wk "Go mode")
  "m t"'(nil :wk "Go mode test")
  "m t t" '(go-test-current-test :wk "test current")
  "m t f" '(go-test-current-file :wk "test file")
  "m t p" '(go-test-current-file :wk "test project")
  "m t c" '(go-test-current-coverage :wk "test coverage")
  "m t b" '(go-test-current-benchmark :wk "test benchmark")
  ))


(provide 'init-go)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-go.el ends here
