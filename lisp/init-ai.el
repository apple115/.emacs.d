;;; init-ai.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: ai

;;; Code:
(use-package aider
  :ensure t
  :bind
  ("C-c a". aider-transient-menu)
  :config
  (setq aider-args '("--no-auto-commits" "--model" "deepseek/deepseek-chat"))
  (setenv "DEEPSEEK_API_KEY" (with-temp-buffer
                               (insert-file-contents "~/.config/deepseek/key.txt")
                               (string-trim (buffer-string)))))

(use-package gptel
  :ensure t
  :bind
  ("C-c i" . gptel-menu)
  :config
  (setq gptel-model "deepspek-chat")
  (setq gptel-default-mode 'org-mode)
  (setq gptel-backend
          (gptel-make-openai "DeepSeek"
        :host "api.deepseek.com"
        :endpoint "/chat/completions"
        :stream t
        :key (with-temp-buffer (with-temp-buffer
                                (insert-file-contents "~/.config/deepseek/key.txt")
                                (string-trim (buffer-string))))
        :models '(deepseek-chat deepseek-coder))))

(provide 'init-ai)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
