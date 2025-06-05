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
  "m t" '(go-test-current-test :wk "test current")))


(provide 'init-go)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-go.el ends here
