;;; init-mind-wave.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: mind-wave

;;; Code:
;;; coade
(use-package mind-wave
 :load-path "~/.config/emacs/site-lisp/mind-wave"
 :config
 (setq mind-wave-python-command "~/.config/emacs/site-lisp/myemacs/bin/python3")
 (setq mind-wave-api-key-path "~/.config/emacs/site-lisp/mind-wave/chatgpt_api_key.txt")
)

(provide 'init-mind-wave)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-mind-wave.el ends here
