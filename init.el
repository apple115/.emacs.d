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
(let
    ((dir
      (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path
               (file-name-as-directory dir)))
;; 加载各模块化配置
;; 不要在`*message*'缓冲区显示加载模块化配置的信息
(with-temp-message ""
  (require 'init-tools)
  (require 'init-ui)
  (require 'init-completion)
  (require 'init-org)
  (require 'init-langs)
  (require 'init-edit)
  (require 'init-projectile)
  (require 'init-org-set)
  (require 'init-org-capture)
  (require 'init-org-agenda)
  (require 'init-flyspell)
  (require 'init-hexo)
  (require 'init-auto-yas)
  (require 'init-vertico)
  (require 'init-consult)
  (require 'init-embark)
  (require 'init-marginalia)
  (require 'init-orderless)
  (require 'init-consult-notes)
  (require 'init-vterm)
  (require 'init-magit)
  (require 'init-colorful-mode)
  (require 'init-devdocs)
  (require 'init-ibuffer)
  (require 'init-dired)
  (require 'init-doom-modeline)
  (require 'init-org-reveal)
  (require 'init-gptel)
  (require 'init-func)
  (require 'init-sort-tab)
  (require 'init-dape)
  (require 'init-citre)
  (require 'init-compile)
  (require 'init-docker)
  (require 'init-windows-manager)
  (require 'init-auto-save)
  (require 'init-virtualenvwrapper)
  (require 'init-web)
  (require 'init-clojure)
  (require 'init-aider)
  (require 'init-link-hint)
  (require 'init-smart-input-source)

  ;; (require 'init-reformatter)
  ;; (require 'init-go-mode)
  ;;  (require 'init-rime)
  ;; (require 'init-bufler)
  ;; (require 'init-realgud)
  ;; (require 'init-tab-bar)
  ;; (require 'init-super-save)
  ;; (require 'init-eaf)
  ;; (require 'init-mind-wave)
  ;; (require 'init-centaur-tabs)
  ;; (require 'init-aweshell)
  ;; (require 'init-tabspaces)
  ;; (require 'init-dashboard)
  ;; (require 'init-awesome-tray)
  ;; (require 'init-js2)
  )
(when
    (memq window-system
          '(mac ns x))
  (exec-path-from-shell-initialize))

(when (daemonp)
  (exec-path-from-shell-initialize))

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dape super-save wucuo which-key vterm-toggle vertico tabspaces sudo-edit shackle rust-mode rime realgud rainbow-delimiters python-mode projectile popper plantuml-mode ox-reveal ox-gfm org-modern org-contrib org-auto-tangle org-appear orderless multi-vterm markdown-mode marginalia haskell-mode gruvbox-theme gptel general format-all flycheck exec-path-from-shell evil-surround evil-nerd-commenter evil-collection eshell-prompt-extras envrc embark-consult doom-modeline dirvish diredfl devdocs denote dashboard consult-notes colorful-mode cal-china-x avy auto-yasnippet all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
