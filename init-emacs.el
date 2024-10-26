;; init-emacs.el
;; emacs -Q -l init-emacs.el

(setq package-enable-at-startup nil)
(setq package-archives
      '(("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("gnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))

(package-initialize)
(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode-in-directories)
  :config
  (setq denote-directory (expand-file-name "~/Nutstore Files/Nutstore/org/denote"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  ;; org is default, set others such as text, markdown-yaml, markdown-toml
  (setq denote-file-type nil)
  (setq denote-prompts '(title keywords))

  ;; We allow multi-word keywords by default.  The author's personal
  ;; preference is for single-word keywords for a more rigid workflow.
  (setq denote-allow-multi-word-keywords nil)
  (setq denote-date-format nil)

  ;; If you use Markdown or plain text files (Org renders links as buttons
  ;; right away)
  ;; (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
  (setq denote-dired-rename-expert nil)

  ;; OR if only want it in `denote-dired-directories':
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)
  )
