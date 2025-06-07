;;; init-rust.el ---  Initialize Rust configurations.  -*- lexical-binding: t -*-
;;; Commentary:
;;; rust settings

;;; Code:
(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t)
  :ensure t
  :general
   (:keymaps 'rust-mode
   :states '(normal)
   :prefix  "SPC"
   "m" '(ignore t :wk "mode")
   "m r" '(rust-run :wk "run")
   "m k" '(rust-check :wk "check")
   "m c" '(rust-run-check :wk "run check")
   "m t" '(rust-test :wk "test current"))
)

(provide 'init-rust)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rust.el ends here
