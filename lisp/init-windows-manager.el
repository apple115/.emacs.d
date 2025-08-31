;;; init-windows-manager.el --- windows manager settings -*- lexical-binding: t -*-
;;; Commentary:
;;; windows-manager

;;; Code:
(use-package shackle
  :ensure t
  :hook (after-elpaca-init . shackle-mode)
  :config
  (setq shackle-select-reused-windows nil) ; default nil
  (setq shackle-default-alignment 'below)  ; default below
  (setq shackle-rules
        ;; CONDITION(:regexp)            :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
        '(
          ;;(compilation-mode              :ignore t)
          ;; ("\\*Async Shell.*\\*" :regexp t :ignore t)
          ("*format-all-errors*"         :select nil                        :size 0.3  :align t     :popup t)
          (compilation-mode              :select nil                        :size 0.3  :align t     :popup t)
          ("*vterm*"                     :select t                          :size 0.3  :align t     :popup t)
;;          ("\\*corfu.*\\*"       :regexp t :ignore t)
          ("*eshell*"                    :select t                          :size 0.3  :align t     :popup t)
          (helpful-mode                  :select t                          :size 0.6  :align right :popup t)
          ("*Messages*"                  :select nil                        :size 0.3  :align t     :popup t)
          ;; ("*Org Agenda(t)*"             :select nil                        :size 0.3  :align right :popup t)
          ("*Async Shell Command*"       :select nil                        :size 0.3  :align t     :popup t)
          ("*vterm compilation*"         :select nil                        :size 0.3  :align t     :popup t)
          ("*lsp-bridge-doc*"            :select nil                        :size 0.3  :align t     :popup t)
          ("*fanyi*"                     :select nil                        :size 0.3  :align t     :popup t)
          ("*Calendar*"                  :select t                          :size 0.3  :align t     :popup t)
          ("*info*"                      :select t                                                  :same t)
          (magit-status-mode             :select t   :inhibit-window-quit t                         :same t)
          (magit-log-mode                :select t   :inhibit-window-quit t                         :same t)
          ))
 )

(use-package popper
  :ensure t
  :bind (:map popper-mode-map
         ("C-h z"       . popper-toggle)
         ("C-<tab>"     . popper-cycle)
         ("C-M-<tab>"   . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*quickrun\\*"
          "Aweshell"
         "\\*compilation\\*"
         "\\*vterm compilation\\*"
         "help-mode"
         "\\*vterm\\*"
         "\\*lsp-bridge-doc\\*"
         "\\*fanyi\\*"
         "\\*eshell*\\*"
         "\\*Warnings*\\*"
         ;; "\\*Org Agenda(t)\\*"
          go-test-mode
          help-mode
          compilation-mode))

  (popper-mode +1)
  (popper-echo-mode +1)
  :config
)                ; For echo area hints


(use-package ace-window
  :ensure t
  :custom-face
  (aw-leading-char-face ((t (:inherit font-lock-keyword-face :foreground unspecified :bold t :height 1.0))))
  (aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
  (aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
  :config
  (global-set-key (kbd "M-o") 'ace-window)
)

(provide 'init-windows-manager)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-windows-manager.el ends here
