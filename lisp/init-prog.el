;;; init-prog.el ---  prog settings -*- lexical-binding: t -*-
;;; Commentary:
;;; prog

;;; Code:
(use-package devdocs
  :ensure t
  :bind
  (:map prog-mode-map
        ("<f1>" . +devdocs-search))
  :config
  (defun +devdocs-lookup()
    "devdocs look at symbol at point"
    (interactive)
    (devdocs-lookup nil (thing-at-point 'symbol t)))
  (defun +devdocs-search()
    "devdocs look at symbol at point"
    (interactive)
    (devdocs-search (thing-at-point 'symbol t)))
)

(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
