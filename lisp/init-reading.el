;;; init-reading.el --- Merged settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Merged from: init-read.el
;;; Code:

;; ---- merged from init-read.el ----
;;; init-read.el --- Write settings -*- lexical-binding: t -*-

;;; Commentary:

(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
)

(use-package pdf-tools
  :ensure t
)

(use-package elfeed
  :ensure t
  :bind ("C-x w" . elfeed))

(use-package elfeed-protocol
  :ensure t
  :after elfeed
  :custom
  ;; 推荐用 curl，比 Emacs 内置 url-retrieve 快且稳定
  (elfeed-use-curl t)
  ;; 如果你的 FreshRSS 是自签名证书，加这个
  ;; (elfeed-curl-extra-arguments '("--insecure"))
  (elfeed-curl-max-connections 10)
  (elfeed-set-timeout 36000)

  ;; FreshRSS 通过 Fever API 对接
  ;; 格式: fever+https://用户名@你的freshrss域名/
  (elfeed-feeds
   '(("fever+https://你的用户名@freshrss.example.com/"
      :api-url "https://freshrss.example.com/api/fever.php"
      :password "你的API密码")))

  ;; 把 FreshRSS 的分类同步为 Elfeed 的 tag
  (elfeed-protocol-fever-fetch-category-as-tag t)
  ;; 只同步未读文章（FreshRSS 的 entry ID 有些问题，这是 workaround）
  (elfeed-protocol-fever-update-unread-only t)
  ;; 每次最多拉取 10000 条
  (elfeed-protocol-fever-maxsize 10000)
  ;; 懒同步，减少不必要的请求
  (elfeed-protocol-lazy-sync t)
  :config
  (elfeed-protocol-enable))

(provide 'init-reading)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-reading.el ends here
