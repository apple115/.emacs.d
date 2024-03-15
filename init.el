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
 (require 'init-flyspell)
 (require 'init-mind-wave)
 (require 'init-centaur-tabs)
 (require 'init-rime)
 (require 'init-aweshell)
;; (require 'init-js2)
  )

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
