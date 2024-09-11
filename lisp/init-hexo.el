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


(evil-collection-define-key 'normal 'hexo-mode-map (kbd "RET") #'hexo-command-open-file)
)



(provide 'init-hexo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-hexo.el ends here
