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
  (require 'init-edit)
  (require 'init-tools)
  (require 'init-ui)
  (require 'init-completion)
  (require 'init-langs)
  (require 'init-write)
  (require 'init-editer)
  (require 'init-projectile)
  (require 'init-org-agenda)
  (require 'init-org-capture)
  (require 'init-consult)
  (require 'init-func)
  (require 'init-dape)
  (require 'init-compile)
  (require 'init-docker)
  (require 'init-windows-manager)
  (require 'init-web)
  (require 'init-python)
  (require 'init-english)
  (require 'init-anki)
  (require 'init-sql)
  (require 'init-keyboard)
  (require 'init-chinese)
  (require 'init-reading)
 ;; (require 'init-eaf)
  ;; (require 'init-lsp-bridge)
  )
(when
    (memq window-system
          '(mac ns x))
  (exec-path-from-shell-initialize))


(when (daemonp)
  (exec-path-from-shell-initialize))
   (server-start)

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(nov evil-matchit cider clojure-ts-mode olivetti eldoc-box eglot ob-go dape super-save wucuo which-key vterm-toggle vertico tabspaces sudo-edit shackle rust-mode rime realgud rainbow-delimiters python-mode popper plantuml-mode ox-reveal ox-gfm org-modern org-contrib org-auto-tangle org-appear orderless multi-vterm markdown-mode marginalia haskell-mode gruvbox-theme gptel general format-all flycheck exec-path-from-shell evil-surround evil-nerd-commenter evil-collection eshell-prompt-extras envrc embark-consult doom-modeline dirvish diredfl devdocs denote dashboard consult-notes colorful-mode cal-china-x avy auto-yasnippet all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
