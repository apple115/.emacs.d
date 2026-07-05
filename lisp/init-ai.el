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

;; (use-package claude-code-ide
;;   :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
;;   :bind ("C-c C-'" . claude-code-ide-menu)
;;   :init
;;   ;; 在 init 阶段就设置，确保在包加载前生效
;;   (setq claude-code-ide-use-side-window nil) ; 使用常规窗口而不是侧边窗口
;;   (setq claude-code-ide-show-claude-window-in-ediff nil)
;;   ;; 强制使用全屏显示（替换当前窗口）
;;   (add-to-list 'display-buffer-alist
;;                '("\\*claude-code\\[.*?\\]\\*"
;;                  (display-buffer-full-frame)
;;                  (inhibit-same-window . t)))
;;   :config
;;   (evil-set-initial-state 'claude-code-ide-mode 'insert)
;;   (claude-code-ide-emacs-tools-setup)
;;   ;; 确保设置没有被覆盖
;;   (setq claude-code-ide-use-side-window nil)
;;   (setq claude-code-ide-terminal-initialization-delay 0.15)
;;   (setq claude-code-ide-vterm-anti-flicker t)
;;   (setq claude-code-ide-vterm-render-delay 0.01)
;;   )

(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

(use-package claude-code
  :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  (claude-code-mode)
  (setq claude-code-terminal-backend 'ghostel)
  :bind-keymap ("C-c c" . claude-code-command-map)
  ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
  :bind
  (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))



;;; gptel-quick: 选中文字 → minibuffer提问 → popup显示回答
(defvar +gptel-quick-buffer "*gptel-quick*")

(defun +gptel-quick ()
  "选中文字(可选), 在 minibuffer 输入问题, 回答显示在临时 popup."
  (interactive)
  (let* ((selection (when (use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))))
         (question (read-string (if selection "Ask (with selection): " "Ask: ")))
         (prompt (if selection
                     (format "根据以下内容回答问题:\n\n%s\n\n问题: %s" selection question)
                   question)))
    (with-current-buffer (get-buffer-create +gptel-quick-buffer)
      (special-mode)
      (erase-buffer)
      (insert (format "# Question: %s\n\nThinking...\n" question)))
    (display-buffer +gptel-quick-buffer
                    '(display-buffer-pop-up-window (inhibit-same-window . t)))
    (gptel-request prompt
                   :buffer (get-buffer +gptel-quick-buffer)
                   :stream nil
                   :callback
                   (lambda (response info)
                     (when (stringp response)
                       (with-current-buffer (get-buffer +gptel-quick-buffer)
                         (erase-buffer)
                         (insert (format "# Question: %s\n\n%s" question response))
                         (goto-char (point-min))))))))


(provide 'init-ai)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
