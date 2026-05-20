;;; init-editer.el --- editor settings -*- lexical-binding: t -*-
;;; Commentary:
;;; editor

;;; Code:
;; (use-package sort-tab
;;   :load-path "./site-lisp/my-fork-sort-tab/"
;;   :custom
;;   (sort-tab-separaor "")
;;   (sort-tab-name-max-length 20)
;;   :config
;;   ;; (add-hook 'tab-bar-tab-post-open-functions (lambda (tab) (sort-tab-turn-on)) t)
;;   (setq sort-tab-hide-function '(lambda (buf) (with-current-buffer buf (derived-mode-p 'dired-mode))))
;;   (setq sort-tab-show-index-number t)
;; )

(use-package dired
  :after evil
  :config
  (when (executable-find "gls")
    (setq insert-directory-program "gls"
          dired-use-ls-dired t))
;; dired 是内置包，不需要 use-package
;; 使用 evil-collection-define-key 来定义键绑定
(with-eval-after-load 'evil-collection
  ;; (setq dired-listing-switches
  ;;       "-l --almost-all --human-readable --group-directories-first --no-group")
  ;; this command is useful when you want to close the window of `dirvish-side'
  ;; automatically when opening a file
  ;; (put 'dired-find-alternate-file 'disabled nil)
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "q") 'quit-window
    (kbd "o") 'dired-quick-access
    (kbd "c") 'dired-do-compress-to
    (kbd "h") 'dired-up-directory
    (kbd "j") 'dired-next-line
    (kbd "k") 'dired-previous-line
    (kbd "l") 'dired-find-file
    (kbd "gr") 'revert-buffer
    (kbd "gy") 'dired-show-file-type
    (kbd "m") 'dired-mark
    (kbd "u") 'dired-unmark
    (kbd "x") 'dired-do-flagged-delete
    (kbd "RET") 'dired-find-file

    (kbd "A") 'dired-do-find-regexp
    (kbd "B") 'dired-do-byte-compile
    (kbd "C") 'dired-do-copy
    (kbd "D") 'dired-do-delete
    (kbd "H") 'dired-do-hardlink
    (kbd "G") 'dired-do-chgrp
    (kbd "M") 'dired-do-chmod
    (kbd "O") 'dired-do-chown
    (kbd "R") 'dired-do-rename
    (kbd "S") 'dired-do-symlink
    (kbd "T") 'dired-do-touch
    (kbd "Y") 'dired-copy-filename-as-kill
    (kbd "Z") 'dired-do-compress
    (kbd "!") 'dired-do-shell-command
    (kbd "&") 'dired-do-async-shell-command
    (kbd "+") 'dired-create-directory)))

(use-package dired-rsync
  :ensure t
  :bind (:map dired-mode-map
              ("C-c C-r" . dired-rsync)))

(use-package nerd-icons-dired
  :ensure t
  :hook(dired-mode . nerd-icons-dired-mode))

(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

(defun xah-open-in-external-app (&optional Fname)
  "Open the current file or dired marked files in external app.
When called in emacs lisp, if Fname is given, open that.

URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Version: 2019-11-04 2023-04-05 2023-06-26"
  (interactive)
  (let (xfileList xdoIt)
    (setq xfileList
          (if Fname
              (list Fname)
            (if (eq major-mode 'dired-mode)
                (dired-get-marked-files)
              (list buffer-file-name))))
    (setq xdoIt (if (<= (length xfileList) 10) t (y-or-n-p "Open more than 10 files? ")))
    (when xdoIt
      (cond
       ((+is-win-p)
        (let ((xoutBuf (get-buffer-create "*xah open in external app*"))
              (xcmdlist (list "PowerShell" "-Command" "Invoke-Item" "-LiteralPath")))
          (mapc
           (lambda (x)
             (message "%s" x)
             (apply 'start-process (append (list "xah open in external app" xoutBuf) xcmdlist (list (format "'%s'" (if (string-match "'" x) (replace-match "`'" t t x) x))) nil)))
           xfileList)
          ;; (switch-to-buffer-other-window xoutBuf)
          )
        ;; old code. calling shell. also have a bug if filename contain apostrophe
        ;; (mapc (lambda (xfpath) (shell-command (concat "PowerShell -Command \"Invoke-Item -LiteralPath\" " "'" (shell-quote-argument (expand-file-name xfpath)) "'"))) xfileList)
        )
       ((+is-mac-p)
        (mapc (lambda (xfpath) (shell-command (concat "open " (shell-quote-argument xfpath)))) xfileList))
       ((+is-linux-p)
        (mapc (lambda (xfpath)
                (call-process shell-file-name nil 0 nil
                              shell-command-switch
                              (format "%s %s"
                                      "xdg-open"
                                      (shell-quote-argument xfpath))))
              xfileList))
       ((eq system-type 'berkeley-unix)
        (mapc (lambda (xfpath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" xfpath))) xfileList))))))


(defun my-paste-to-dired ()
  "使用wl-paste 命令复制在当前文件夹中."
  (interactive)
  (let ((past-file-name (read-file-name "Enter file name:") ))
    (async-shell-command (format "wl-paste > %s" past-file-name) )
    ))

(use-package auto-save
  :load-path "./site-lisp/auto-save"
  :config
  (auto-save-enable)
  (setq auto-save-silent t)
  (setq auto-save-delete-trailing-whitespace t)
  )

(use-package ibuffer
  :ensure nil
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  :config
  (use-package nerd-icons-ibuffer
    :ensure
    :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

  (use-package ibuffer-project
    :ensure t
    :config
    (add-hook
     'ibuffer-hook
     (lambda ()
       (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
       (unless (eq ibuffer-sorting-mode 'project-file-relative)
         (ibuffer-do-sort-by-project-file-relative))))
    )
  )

(use-package nerd-icons
  :ensure t)

;;view large file
(use-package vlf
  :ensure t
  :config
  (require 'vlf-setup))

;; (use-package dired-fileNest
;;   :load-path "./site-lisp/dired-fileNest/"
;;   :hook (dired-mode . dired-fileNest-mode)
;;   :custom
;;   (dired-fileNest-expand nil)
;;   (dired-fileNest-indent-string "  ")
;;   (dired-fileNest-patterns
;;   '(;; C/C++
;;     ("\\(.*\\)\\.c$"   . ("\\1.h" "\\1.o"))
;;     ("\\(.*\\)\\.cpp$" . ("\\1.hpp" "\\1.h" "\\1.hxx" "\\1.o"))
;;     ("\\(.*\\)\\.cc$"  . ("\\1.hpp" "\\1.h" "\\1.hxx"))
;;     ("\\(.*\\)\\.cxx$" . ("\\1.hpp" "\\1.h" "\\1.hxx"))
;;     ("\\(.*\\)\\.h$"   . ("\\1.c" "\\1.cpp" "\\1.cc" "\\1.cxx"))
;;     ;; Elisp
;;     ("\\(.*\\)\\.el$"  . ("\\1.elc"))
;;     ;; Go
;;     ("\\(.*\\)\\.go$"  . ("\\1_test.go"))
;;     ;; Java
;;     ("\\(.*\\)\\.java$" . ("\\1.class"))
;;     ;; Rust
;;     ("\\(.*\\)\\.rs$"  . ("\\1.rs.bk"))
;;     ;; Dart/Flutter
;;     ("\\(.*\\)\\.dart$" . ("\\1.freezed.dart" "\\1.g.dart"))
;;     ("\\(.*\\)\\.bloc\\.dart$" . ("\\1.event.dart" "\\1.state.dart"))
;;     ;; Python
;;     ("\\(.*\\)\\.py$"  . ("\\1.pyc" "\\1.pyo"))
;;     ;; Elixir/Phoenix
;;     ("\\(.*\\)\\.ex$"  . ("\\1.html.eex" "\\1.html.heex" "\\1.html.leex"))
;;     ;; JavaScript
;;     ("\\(.*\\)\\.js$"  . ("\\1.js.map" "\\1.*.js" "\\1_*.js" "\\1.d.ts.map"))
;;     ("\\(.*\\)\\.cjs$" . ("\\1.cjs.map" "\\1.*.cjs" "\\1_*.cjs"))
;;     ("\\(.*\\)\\.mjs$" . ("\\1.mjs.map" "\\1.*.mjs" "\\1_*.mjs"))
;;     ;; TypeScript
;;     ("\\(.*\\)\\.ts$"  . ("\\1.js" "\\1.d.ts.map" "\\1.*.ts" "\\1_*.js" "\\1_*.ts"))
;;     ("\\(.*\\)\\.tsx$" . ("\\1.ts" "\\1.*.tsx" "\\1_*.ts" "\\1_*.tsx"))
;;     ("\\(.*\\)\\.jsx$" . ("\\1.js" "\\1.*.jsx" "\\1_*.js" "\\1_*.jsx"))
;;     ;; CSS
;;     ("\\(.*\\)\\.css$" . ("\\1.css.map" "\\1.*.css"))
;;     ("\\(.*\\)\\.scss$" . ("\\1.css"))
;;     ("\\(.*\\)\\.sass$" . ("\\1.css"))
;;     ("\\(.*\\)\\.less$" . ("\\1.css"))
;;     ;; Vue/Svelte
;;     ("\\(.*\\)\\.vue$" . ("\\1.*.ts" "\\1.*.js" "\\1.story.vue"))
;;     ("\\+layout\\.svelte$" . ("+layout.ts" "+layout.js" "+layout.server.ts" "+layout.server.js" "+layout.gql"))
;;     ("\\+page\\.svelte$"  . ("+page.server.ts" "+page.server.js" "+page.ts" "+page.js" "+page.gql"))
;;     ;; .NET/C#
;;     ("\\(.*\\)\\.cs$"   . ("\\1.*.cs"))
;;     ("\\(.*\\)\\.cshtml$" . ("\\1.cshtml.cs"))
;;     ("\\(.*\\)\\.resx$" . ("\\1.*.resx" "\\1.designer.cs" "\\1.designer.vb"))
;;     ;; XAML
;;     ("\\(.*\\)\\.xaml$" . ("\\1.xaml.cs"))
;;     ;; Angular
;;     ("\\(.*\\)\\.component\\.ts$"  . ("\\1.component.html" "\\1.component.spec.ts"
;;                                        "\\1.component.css" "\\1.component.scss"
;;                                        "\\1.component.sass" "\\1.component.less"))
;;     ("\\(.*\\)\\.module\\.ts$" . ("\\1.resolver.ts" "\\1.controller.ts" "\\1.service.ts"))
;;     ;; Documentation
;;     ("\\(.*\\)\\.org$" . ("\\1.pdf" "\\1.html"))
;;     ("\\(.*\\)\\.tex$" . ("\\1.aux" "\\1.log" "\\1.pdf" "\\1.toc" "\\1.synctex.gz"
;;                            "\\1.acn" "\\1.acr" "\\1.alg" "\\1.bbl" "\\1.blg"
;;                            "\\1.fdb_latexmk" "\\1.fls" "\\1.glg" "\\1.glo"
;;                            "\\1.gls" "\\1.idx" "\\1.ind" "\\1.ist" "\\1.lof"
;;                            "\\1.lot" "\\1.out" "\\1.xdv"))
;;     ("\\(.*\\)\\.md$"  . ("\\1.pdf" "\\1.html" "\\1.docx"))
;;     ;; ============================================================
;;     ;; Exact file parents
;;     ;; ============================================================
;;     ("^package\\.json$"  . (".browserslistrc" ".editorconfig" ".eslintignore" ".eslintrc"
;;                               ".gitignore" ".npmignore" ".npmrc" ".prettierrc"
;;                               "package-lock.json" "pnpm-lock.yaml" "yarn.lock"
;;                               "tsconfig.json" "tsconfig.*.json"
;;                               ".babelrc" "babel.config.js" "babel.config.cjs" "babel.config.mjs"
;;                               "webpack.config.js" "webpack.config.cjs" "webpack.config.mjs"
;;                               "rollup.config.js" "rollup.config.cjs" "rollup.config.mjs"
;;                               "vite.config.ts" "vitest.config.ts"
;;                               ".env" ".env.*" "env.d.ts" ".flowconfig"
;;                               ".jslintrc" ".stylelintrc" ".prettierignore"
;;                               "jest.config.js" "jest.config.ts" "jest.config.mjs"
;;                               ".huskyrc" ".lintstagedrc"))
;;     ("^cargo\\.toml$"  . ("cargo.lock" ".clippy.toml" ".rustfmt.toml" "rust-toolchain.toml"
;;                            "clippy.toml" "cross.toml"))
;;     ("^go\\.mod$"  . ("go.sum" ".air.toml"))
;;     ("^go\\.work$" . ("go.work.sum"))
;;     ("^gemfile$"   . ("gemfile.lock" ".ruby-version"))
;;     ("^composer\\.json$" . ("composer.lock" "phpunit.xml" ".phpcs.cache"))
;;     ("^pyproject\\.toml$" . ("pdm.lock" ".pdm.toml"))
;;     ("^mix\\.exs$"  . ("mix.lock" ".credo.exs" ".dialyzer_ignore.exs" ".formatter.exs"
;;                         ".iex.exs" ".tool-versions"))
;;     ("^pubspec\\.yaml$" . ("pubspec.lock" ".packages" ".metadata"
;;                              "analysis_options.yaml" "build.yaml"))
;;     ("^flake\\.nix$" . ("flake.lock"))
;;     ("^default\\.nix$" . ("shell.nix"))
;;     ("^dockerfile$" . ("dockerfile.*" ".dockerignore" "docker-compose.*"))
;;     ("^dockerfile\\..*$" . (".dockerignore" "docker-compose.*"))
;;     ("^cmakelists\\.txt$" . ("*.cmake" "*.cmake.in" ".cmake-format.yaml" "cmakepresets.json"))
;;     ("^build\\.bazel$" . ("*.bzl" "*.bazel" "workspace"))
;;     ("^readme"     . ("authors" "changelog" "changelog.*" "contributing" "contributing.*"
;;                         "license" "license.*" "readme" "security.md"))
;;     ("^license"    . ("readme"))
;;     ("^\\.env$"    . ("*.env" ".env.*" ".envrc" "env.d.ts"))
;;     ("^\\.gitignore$" . (".gitattributes" ".gitmodules" ".gitmessage" ".mailmap"))
;;     ("^\\.clang-tidy$" . (".clang-format" ".clangd" "compile_commands.json"))
;;     ("^\\.project$" . (".classpath"))
;;     ("^shims\\.d\\.ts$" . ("*.d.ts"))
;;     ("^tsconfig\\.json$" . ("tsconfig.*.json" "jsconfig.json" "jsconfig.*.json"))
;;     ("^next\\.config\\..*$"   . ("next-env.d.ts"))
;;     ("^nuxt\\.config\\..*$"   . (".env" ".env.*" "env.d.ts" "tsconfig.json"))
;;     ("^astro\\.config\\..*$"  . ("tsconfig.json"))
;;     ("^svelte\\.config\\..*$" . (".env" ".env.*" "env.d.ts" "tsconfig.json" "vite.config.*"))
;;     ("^vite\\.config\\..*$"   . (".env" ".env.*" "env.d.ts" "tsconfig.json"))
;;     ("^quasar\\.conf\\.js$"   . ("quasar.extensions.json"))
;;     ("^remix\\.config\\..*$"  . ("remix.*"))
;;     ("^deno\\.json"            . ("deno.lock" "import-map.json" "import_map.json"
;;                                    "env.d.ts" "tsconfig.json"))
;;     ("^rush\\.json$" . ("pnpm-lock.yaml" "package-lock.json"))))

(provide 'init-editer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-editer.el ends here
