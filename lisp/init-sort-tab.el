;;; init-sort-tab.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: sort-tab

;;; Code:
;;; coade

(use-package sort-tab
  :load-path "./site-lisp/my-fork-sort-tab"
  :after doom-modeline
  :config
  (setq sort-tab-hide-function '(lambda (buf) (with-current-buffer buf (derived-mode-p 'dired-mode))))
  (sort-tab-mode 1)
  (setq sort-tab-show-index-number t)

  (dt/leader-keys
    "1" 'sort-tab-select-visible-tab
    "2" 'sort-tab-select-visible-tab
    "3" 'sort-tab-select-visible-tab
    "4" 'sort-tab-select-visible-tab
    "5" 'sort-tab-select-visible-tab
    "6" 'sort-tab-select-visible-tab
    "7" 'sort-tab-select-visible-tab
    "8" 'sort-tab-select-visible-tab
    "9" 'sort-tab-select-visible-tab
    "0" 'sort-tab-select-visible-tab
    )
    (global-set-key (kbd "s-Q") 'sort-tab-close-all-tabs)
    (global-set-key (kbd "s-q") 'sort-tab-close-mode-tabs)
    (global-set-key (kbd "C-;") 'sort-tab-close-current-tab)
)

(provide 'init-sort-tab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-sort-tab.el ends here
