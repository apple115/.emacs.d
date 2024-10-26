;;; init-ai.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: ai

;;; Code:
(use-package aider
  :load-path "./site-lisp/aider.el"
  :config
  (setq aider-args '("--no-auto-commits" "--model" "deepseek/deepseek-coder"))
  (setenv "DEEPSEEK_API_KEY" (with-temp-buffer
                               (insert-file-contents "~/.config/deepseek/key.txt")
                               (string-trim (buffer-string)))))

(use-package gptel
  :ensure t
  :config
  (setq gptel-model "moonshot-v1-8k")
  (setq gptel-default-mode 'org-mode)
  (setq gptel-backend
        (gptel-make-openai "Moonshot"
          :key 'gptel-api-key
          :models '("moonshot-v1-8k"
                    "moonshot-v1-32k"
                    "moonshot-v1-128k")
          :host "api.moonshot.cn")))

(provide 'init-ai)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
