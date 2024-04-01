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
 (require 'init-org-set)
 (require 'init-org-capture)
 (require 'init-org-agenda)

 (require 'init-langs)
 (require 'init-edit)
 (require 'init-flyspell)
 (require 'init-mind-wave)
;;( require 'init-centaur-tabs)
 (require 'init-rime)
 (require 'init-aweshell)
 (require 'init-hexo)
 (require 'init-auto-yas)
 (require 'init-sort-tab)


 (require 'init-vertico)
 (require 'init-consult)
 (require 'init-embark)
 (require 'init-marginalia)
 (require 'init-orderless)
 (require 'init-shackle)
 (require 'init-dashboard)

(require 'init-consult-notes)
;; (require 'init-awesome-tray)
;; (require 'init-magit)
 ;;(require 'init-eaf)
;; (require 'init-js2)
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
   '(consult-notes wucuo which-key web-mode vertico undo-tree super-save sudo-edit sis shrink-path shackle rust-mode rime reformatter rainbow-delimiters quickrun pyvenv python-mode projectile popper plantuml-mode ox-gfm org-modern org-contrib org-auto-tangle org-appear orderless nerd-icons markdown-mode marginalia magit js2-mode highlight-indentation haskell-mode gruvbox-theme general format-all flycheck-rust exec-path-from-shell evil-surround evil-nerd-commenter evil-collection eshell-prompt-extras envrc embark-consult diredfl denote dashboard counsel centaur-tabs calibredb cal-china-x avy auto-yasnippet amx all-the-icons-ivy-rich)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
