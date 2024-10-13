;;; init-english.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: english

;;; Code:
(use-package fanyi
  :ensure t
  :custom
  (fanyi-providers '(;; 海词
                     fanyi-haici-provider
                     ;; 有道同义词词典
                     fanyi-youdao-thesaurus-provider
                     fanyi-etymon-provider
                     ;; Longman
                     fanyi-longman-provider)))

(provide 'init-english)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-english.el ends here
