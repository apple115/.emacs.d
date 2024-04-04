;;; init-sort-tab.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: sort-tab

;;; Code:
;;; coade

(use-package sort-tab
  :load-path "~/.config/emacs/site-lisp/sort-tab"
  :config
  (sort-tab-mode 1)
(define-key evil-normal-state-map (kbd "g t") 'sort-tab-select-next-tab)
(define-key evil-normal-state-map (kbd "g T") 'sort-tab-select-prev-tab)

(define-key evil-normal-state-map (kbd "g 1") 'sort-tab-select-visible-tab)
(define-key evil-normal-state-map (kbd "g 2") 'sort-tab-select-visible-tab)
(define-key evil-normal-state-map (kbd "g 3") 'sort-tab-select-visible-tab)
(define-key evil-normal-state-map (kbd "g 4") 'sort-tab-select-visible-tab)
(define-key evil-normal-state-map (kbd "g 5") 'sort-tab-select-visible-tab)
(define-key evil-normal-state-map (kbd "g 6") 'sort-tab-select-visible-tab)
(define-key evil-normal-state-map (kbd "g 7") 'sort-tab-select-visible-tab)
(define-key evil-normal-state-map (kbd "g 8") 'sort-tab-select-visible-tab)
(define-key evil-normal-state-map (kbd "g 9") 'sort-tab-select-visible-tab)
(define-key evil-normal-state-map (kbd "g 0") 'sort-tab-select-visible-tab)
(setq sort-tab-hide-function '(lambda (buf) (with-current-buffer buf (derived-mode-p 'dired-mode))))

)

(provide 'init-sort-tab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-sort-tab.el ends here
