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
  :bind
  ("C-c a" . gptel-menu)
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
        :models '(deepseek-chat deepseek-coder))
    ))

;;; code-copilot

;; (use-package copilot
;;   :ensure t
;;   :config
;;   (setq copilot-version nil)
;;   (setq copilot-node-executable "/opt/homebrew/bin/node")
;;   (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;;  )

;; (use-package aidermacs
;;   :load-path "./site-lisp/aidermacs"
;;   :config
;;   (setq aidermacs-default-model "deepseek/deepseek-coder")
;;   ;; Use vterm backend (default is comint)
;;   (setq aidermacs-backend 'vterm)
;;   (global-set-key (kbd "C-c a") 'aidermacs-transient-menu)
;;   ; Enable minor mode for Aider files
;;   (aidermacs-setup-minor-mode)
;;   ; See the Configuration section below
;;   (setq aidermacs-auto-commits t)
;;   (setq aidermacs-use-architect-mode t)
;;   ; Ensure emacs can access *_API_KEY through .bashrc or setenv
;;   (setenv "ANTHROPIC_API_KEY" (with-temp-buffer
;;                                 (insert-file-contents "~/.config/deepseek/key.txt")
;;                                 (string-trim (buffer-string)))))

(provide 'init-ai)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
