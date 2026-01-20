;;; init-ai.el --- ai tool settings -*- lexical-binding: t -*-
;;; Commentary:
;;; ai tool settings

;;; Code:
;; (use-package aider
;;   :ensure t
;;   ;; :bind
;;   ;; ("C-c a". aider-transient-menu)
;;   :config
;;   (setq aider-args '("--no-auto-commits" "--model" "deepseek/deepseek-chat"))
;;   (setenv "DEEPSEEK_API_KEY" (with-temp-buffer
;;                                (insert-file-contents "~/.config/deepseek/key.txt")
;;                                (string-trim (buffer-string)))))

;; (use-package aidermacs
;;   :ensure t
;;   :bind (("C-c a" . aidermacs-transient-menu))
;;   :config
;;   (setq aidermacs-default-model "deepseek/deepseek-chat")
;;   (setq aidermacs-backend 'vterm)
;;   (setq aidermacs-vterm-multiline-newline-key "S-<return>")
;;   (setenv "DEEPSEEK_API_KEY" (with-temp-buffer
;;                                (insert-file-contents "~/.config/deepseek/key.txt")
;;                                (string-trim (buffer-string))))
;;   (setq aidermacs-show-diff-after-change t))

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


;; (use-package claude-code-ide
;;   :vc (claude-code-ide :url "https://github.com/manzaltu/claude-code-ide.el")
;;   :bind ("C-c C-'" . claude-code-ide-menu)
;;   :config
;;   (claude-code-ide-emacs-tools-setup)
;;   (setq claude-code-ide-use-ide-diff nil)
;;   (setq claude-code-ide-window-side 'right
;;         claude-code-ide-window-width 100)
;; )

(use-package monet
  :vc (:url "https://github.com/stevemolitor/monet" :rev :newest))

;; NOTE: This will take a while due to massive GIFs in the repo!
(use-package claude-code
  :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)

  :config
    (defun my-claude-display-right (buffer)
    "Display Claude buffer in right side window."
    (display-buffer buffer '((display-buffer-in-side-window)
                            (side . right)
                            (window-width . 90))))
    (setq claude-code-display-window-fn #'my-claude-display-right)
  ;; optional IDE integration with Monet
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)
  (setq claude-code-terminal-backend 'vterm)

  (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map)

  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))

(use-package agent-shell
    :ensure t)

(provide 'init-ai)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
