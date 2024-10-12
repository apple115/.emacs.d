;;; init-smart-input-source.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: smeart-input-soure

;;; Code:

(use-package sis
  ;;:hook
  ;; enable the /context/ and /inline region/ mode for specific buffers
  ;; (((text-mode prog-mode) . sis-context-mode)
  ;; ((text-mode prog-mode) . sis-inline-mode))
  :ensure t
  :config
  (sis-ism-lazyman-config "1" "2" 'fcitx5)
  ;; enable the /cursor color/ mode
  (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /context/ mode for all buffers
  (sis-global-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  (sis-global-inline-mode t)
  )

(provide 'init-smart-input-source)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-smart-input-source.el ends here
