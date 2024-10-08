;;; init-aider.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: aider

;;; Code:
(use-package aider
  :load-path "./site-lisp/aider.el"
  :config
  (setq aider-args '("--no-auto-commits" "--model" "deepseek/deepseek-coder"))
  (setenv "DEEPSEEK_API_KEY" (with-temp-buffer
                               (insert-file-contents "~/.config/deepseek/key.txt")
                               (string-trim (buffer-string))))

  ;; ;; Optional: Set a key binding for the transient menu
  ;; (global-set-key (kbd "C-c a") 'aider-transient-menu))

(provide 'init-aider)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-aider.el ends here
