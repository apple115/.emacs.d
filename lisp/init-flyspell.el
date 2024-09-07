;;; init-flyspell.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: flyspell

;;; Code:
;;; coade
(setq ispell-program-name "aspell")
;; You could add extra option "--camel-case" for camel case code spell checking if Aspell 0.60.8+ is installed
;; @see https://github.com/redguardtoo/emacs.d/issues/796
(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16"))

;; (setq ispell-program-name "hunspell")
;; ;; reset the hunspell so it STOPS querying locale!
;; ;; "en_US" is the key to lookup in `ispell-local-dictionary-alist`
;; (setq ispell-local-dictionary "en_US")
;; ;; two dictionaries "en_US" and "zh_CN" are used. Feel free to remove "zh_CN"
;; ;; If `ispell-local-dictionary-alist' is nil, `ispell-local-dictionary' is passed
;; ;; to hunpsell cli program as dictionary.
;; (setq ispell-local-dictionary-alist
;;       '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US" "zh_CN") nil utf-8)))
;; ;; new variable `ispell-hunspell-dictionary-alist' is defined in Emacs
;; ;; If it's nil, Emacs tries to automatically set up the dictionaries.
;; (when (boundp 'ispell-hunspell-dictionary-alist)
;;       (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist))

(use-package  wucuo
  :ensure t
  :custom
  (wucuo-flyspell-start-mode "fast")
  ;; How many seconds wucuo waits before running spell-check.
  (wucuo-update-interval 2)
  :config
  (add-hook 'prog-mode-hook #'wucuo-start)
(add-hook 'text-mode-hook #'wucuo-start)
(setq wucuo-spell-check-buffer-predicate
      (lambda ()
        (not (memq major-mode
                   '(dired-mode
                     vterm-mode
                     log-edit-mode
                     compilation-mode
                     help-mode
                     profiler-report-mode
                     speedbar-mode
                     gud-mode
                     calc-mode
                     Info-mode
                     )))))
)

(provide 'init-flyspell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flyspell.el ends here
