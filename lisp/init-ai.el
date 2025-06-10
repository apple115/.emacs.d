;;; init-ai.el --- ai tool settings -*- lexical-binding: t -*-
;;; Commentary:
;;; ai tool settings

;;; Code:
(use-package aider
  :ensure t
  ;; :bind
  ;; ("C-c a". aider-transient-menu)
  :config
  (setq aider-args '("--no-auto-commits" "--model" "deepseek/deepseek-chat"))
  (setenv "DEEPSEEK_API_KEY" (with-temp-buffer
                               (insert-file-contents "~/.config/deepseek/key.txt")
                               (string-trim (buffer-string)))))

(use-package aidermacs
  :ensure t
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setq aidermacs-default-model "deepseek/deepseek-chat")
  (setq aidermacs-backend 'vterm)
  (setq aidermacs-vterm-multiline-newline-key "S-<return>")
  (setenv "DEEPSEEK_API_KEY" (with-temp-buffer
                               (insert-file-contents "~/.config/deepseek/key.txt")
                               (string-trim (buffer-string))))
  (setq aidermacs-show-diff-after-change t)
  )

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
