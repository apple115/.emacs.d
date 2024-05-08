;;; init-shackle.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: shackle

;;; Code:
(use-package shackle
  :ensure t
  :hook (after-init . shackle-mode)
  :init
  (setq shackle-lighter "")
  (setq shackle-select-reused-windows nil) ; default nil
  (setq shackle-default-alignment 'below)  ; default below
  (setq shackle-rules
        ;; CONDITION(:regexp)            :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
        '((compilation-mode              :ignore t)
          ;; ("\\*Async Shell.*\\*" :regexp t :ignore t)
          ("*Async Shell Command*"         :select t                          :size 0.4  :align t     :popup t)
          ("*vterm*"                 :select t                          :size 0.4  :align t     :popup t)
          ("*lsp-bridge-doc*"            :select nil                         :size 0.4  :align t     :popup t)
;;          ("\\*corfu.*\\*"       :regexp t :ignore t)
          ("*eshell*"                    :select t                          :size 0.4  :align t     :popup t)
          (helpful-mode                  :select t                          :size 0.6  :align right :popup t)
          ("*Messages*"                  :select t                          :size 0.4  :align t     :popup t)
          ("*Org Agenda(t)*"             :select nil                        :size 0.4  :align right :popup t)
          ("*Calendar*"                  :select t                          :size 0.3  :align t     :popup t)
          ("*info*"                      :select t                                                  :same t)
          (magit-status-mode             :select t   :inhibit-window-quit t                         :same t)
          (magit-log-mode                :select t   :inhibit-window-quit t                         :same t)
          ))
 )


(provide 'init-shackle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shackle.el ends here
