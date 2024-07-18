;;; init-hexo.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: hexo

;;; Code:
;;; coade
(use-package hexo
 :load-path "./site-lisp/hexo.el"
 :config
 (defun hexo-my-blog ()
    (interactive)
    (hexo "~/blog/"))
)

(provide 'init-hexo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-hexo.el ends here
