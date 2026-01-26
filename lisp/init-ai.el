;;; init-ai.el --- ai tool settings -*- lexical-binding: t -*-
;;; Commentary:
;;; ai tool settings

;;; Code:
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

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu)
  :init
  ;; 在 init 阶段就设置，确保在包加载前生效
  (setq claude-code-ide-use-side-window nil) ; 使用常规窗口而不是侧边窗口
  (setq claude-code-ide-show-claude-window-in-ediff nil)
  ;; 强制使用全屏显示（替换当前窗口）
  (add-to-list 'display-buffer-alist
               '("\\*claude-code\\[.*?\\]\\*"
                 (display-buffer-full-frame)
                 (inhibit-same-window . t)))
  :config
  (evil-set-initial-state 'claude-code-ide-mode 'insert)
  (claude-code-ide-emacs-tools-setup)
  ;; 确保设置没有被覆盖
  (setq claude-code-ide-use-side-window nil)
  (setq claude-code-ide-terminal-initialization-delay 0.15)
  (setq claude-code-ide-vterm-anti-flicker t)
  (setq claude-code-ide-vterm-render-delay 0.01)
  )

(provide 'init-ai)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
