;;; init.el --- The main init entry for Emacs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(require 'package)
(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
	("gnu"    . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; 将lisp目录放到加载路径的前面以加快启动速度
 (let ((dir (locate-user-emacs-file "lisp")))
   (add-to-list 'load-path (file-name-as-directory dir)))

 ;; 加载各模块化配置
 ;; 不要在`*message*'缓冲区显示加载模块化配置的信息
 (with-temp-message ""
(require 'init-tools)
  (require 'init-ui)                    ; 加载UI交互的模块化配置文件
  (require 'init-completion) ; 加载完成的模块文件
  (require 'init-org)
  (require 'init-langs)
 (require 'init-edit)
   )

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-nerd-commenter yasnippet which-key web-mode vterm super-save sudo-edit sis rust-mode reformatter rainbow-delimiters quickrun python-mode projectile popper plantuml-mode ox-gfm org-modern org-auto-tangle markdown-mode magit js2-mode haskell-mode gruvbox-theme general format-all flycheck-rust exec-path-from-shell evil-surround evil-collection envrc denote counsel calibredb cal-china-x avy amx all-the-icons-ivy-rich)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
