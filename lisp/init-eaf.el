;;; init-eaf.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: eaf

;;; Code:

(use-package eaf
  :load-path "./site-lisp/emacs-application-framework"
  :custom
  ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  :config
  (defalias 'browse-web #'eaf-open-browser))
   ;; unbind, see more in the Wiki

(use-package eaf-browser
:ensure nil
:config
)

(require 'eaf-pdf-viewer)
(require 'eaf-evil)
;; (setq eaf-evil-leader-keymap
;;       (let ((leader-keymap (make-sparse-keymap)))
;;         (mapc (lambda (pair)
;;                 (define-key leader-keymap (kbd (car pair)) (cdr pair)))
;;                 '(( "1". find-file)
;;                     ("2". save-buffer)))
;;         leader-keymap))
(require 'eaf-evil)
(defvar my-eaf-mode-map (make-sparse-keymap))
(define-key my-eaf-mode-map (kbd "1") 'sort-tab-select-visible-tab)
(define-key my-eaf-mode-map (kbd "2") 'sort-tab-select-visible-tab)
(define-key my-eaf-mode-map (kbd "3") 'sort-tab-select-visible-tab)
(define-key my-eaf-mode-map (kbd "4") 'sort-tab-select-visible-tab)
(define-key my-eaf-mode-map (kbd "5") 'sort-tab-select-visible-tab)
(define-key my-eaf-mode-map (kbd "6") 'sort-tab-select-visible-tab)
(define-key my-eaf-mode-map (kbd "7") 'sort-tab-select-visible-tab)
(define-key my-eaf-mode-map (kbd "8") 'sort-tab-select-visible-tab)
(define-key my-eaf-mode-map (kbd "9") 'sort-tab-select-visible-tab)
(define-key my-eaf-mode-map (kbd "0") 'sort-tab-select-visible-tab)
(define-key my-eaf-mode-map (kbd "b k") '(kill-buffer :wk "kill buffer"))
(define-key my-eaf-mode-map (kbd "s f") '(consult-fd :wk "find file"))

(setq eaf-evil-leader-keymap my-eaf-mode-map)
(setq eaf-evil-leader-key "C-SPC")


(provide 'init-eaf)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eaf.el ends here
