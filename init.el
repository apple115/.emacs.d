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
;; (require 'init-mind-wave)
;;( require 'init-centaur-tabs)

 (require 'init-rime)
 (require 'init-aweshell)
 (require 'init-hexo)
 (require 'init-auto-yas)
;; (require 'init-sort-tab)
 ;;(require 'init-tabspaces)

 (require 'init-vertico)
 (require 'init-consult)
 (require 'init-embark)
 (require 'init-marginalia)
 (require 'init-orderless)
 (require 'init-shackle)
;; (require 'init-dashboard)

(require 'init-consult-notes)
(require 'init-vterm)
;;(require 'init-smart-input-source)
;; (require 'init-awesome-tray)
(require 'init-magit)
(require 'init-realgud)
;;(require 'init-doom-modeline)
 ;;(require 'init-eaf)
;; (require 'init-js2)
  )

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
