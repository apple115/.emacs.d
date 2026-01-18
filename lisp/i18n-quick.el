;;; i18n-quick.el --- Pure Native i18n Minor Mode  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'pulse)

;; ==========================================
;; 1. 配置
;; ==========================================

(defgroup i18n-quick nil "Native i18n quick tool." :group 'tools)

(defcustom i18n-quick-file-path "src/locale/zh-CN/"
  "翻译文件或目录的相对路径。" :type 'string :safe #'stringp)

(defcustom i18n-quick-style 'nested
  "嵌套格式 (nested) 或 扁平格式 (flat)。" :type '(choice (const nested) (const flat)) :safe #'symbolp)

;; ==========================================
;; 2. 核心逻辑 (Native C 引擎)
;; ==========================================

(cl-defstruct i18n-quick--ctx file-or-dir style root)

(defun i18n-quick--get-ctx ()
  (let* ((root (or (and (fboundp 'projectile-project-root) (projectile-project-root))
                   (locate-dominating-file default-directory ".git")
                   default-directory))
         ;; 容错处理：
         ;; 1. 如果是 symbol (如通过 add-dir-local 没加引号加的), 转为 string
         ;; 2. 确保拿到的值不是 nil
         (raw-path i18n-quick-file-path)
         (path (cond ((stringp raw-path) raw-path)
                     ((symbolp raw-path) (symbol-name raw-path))
                     (t (error "i18n-quick-file-path 必须是字符串或符号")))))
    (make-i18n-quick--ctx
     :root root
     :file-or-dir (expand-file-name path root)
     :style i18n-quick-style)))

(defun i18n-quick--resolve-target (ctx key)
  "使用 file-directory-p 判断 Split Mode。"
  (let* ((base (i18n-quick--ctx-file-or-dir ctx)))
    (if (file-directory-p base)
        (let* ((parts (split-string key "\\."))
               (file-part (car parts))
               (inner-part (if (cdr parts) (mapconcat #'identity (cdr parts) ".") file-part))
               ;; 安全清洗文件名并拼接 .json
               (filename (concat (replace-regexp-in-string "[^[:alnum:].-]" "" file-part) ".json")))
          (cons (expand-file-name filename base) inner-part))
      (cons base key))))

(defun i18n-quick--read-json (file)
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (condition-case nil (json-parse-buffer :object-type 'alist) (error nil)))))

(defun i18n-quick--save (file alist)
  "Native C 序列化写回。"
  (with-temp-file file
    (insert (json-serialize alist :pretty t :true t :null nil))))

(defun i18n-quick--update-alist (alist keys value)
  "递归更新 Alist 结构。"
  (if (null keys) value
    (let* ((k (car keys))
           (sub (cdr (assoc k alist #'string=))))
      (setf (alist-get k alist nil nil #'string=)
            (i18n-quick--update-alist (if (listp sub) sub nil) (cdr keys) value))
      alist)))

;; ==========================================
;; 3. 交互功能
;; ==========================================

(defun i18n-quick--get-key-at-point ()
  "精准识别 t('...') 或 i18nKey='...'"
  (let* ((case-fold-search t)
         (re "\\(?:t(\\|i18nKey=\\)['\"]\\([^'\"{}() ]+\\)['\"]")
         (p (point))
         (line-start (line-beginning-position))
         (line-end (line-end-position))
         (found nil))
    (save-excursion
      (goto-char line-start)
      (while (and (not found) (re-search-forward re line-end t))
        (when (and (>= p (match-beginning 0)) (<= p (match-end 0)))
          (setq found (match-string-no-properties 1))))
      found)))

(defun i18n-quick-view ()
  "查看翻译内容。"
  (interactive)
  (let ((key (i18n-quick--get-key-at-point)))
    (if (not key) (message "[i18n] 未识别到 Key")
      (let* ((ctx (i18n-quick--get-ctx))
             (target (i18n-quick--resolve-target ctx key))
             (file (car target))
             (inner-key (cdr target)))
        (if (not (file-exists-p file))
            (message "[i18n-Error] 路径不存在: %s" file)
          (let* ((data (i18n-quick--read-json file))
                 (keys (if (eq (i18n-quick--ctx-style ctx) 'flat) (list inner-key) (split-string inner-key "\\.")))
                 (val data))
            (dolist (k keys) (when (listp val) (setq val (cdr (assoc k val #'string=)))))
            (if (stringp val) (message "[i18n] %s" val)
              (message "[i18n] 未在 %s 中找到: %s" (file-name-nondirectory file) inner-key))))))))

(defun i18n-quick-jump-or-create ()
  "跳转到定义位置，如果不存在则原地创建。"
  (interactive)
  (let ((key (i18n-quick--get-key-at-point)))
    (if (not key) (message "[i18n] 未识别到 Key")
      (let* ((ctx (i18n-quick--get-ctx))
             (target (i18n-quick--resolve-target ctx key))
             (file (car target))
             (inner-key (cdr target))
             (data (i18n-quick--read-json file))
             (keys (if (eq (i18n-quick--ctx-style ctx) 'flat) (list inner-key) (split-string inner-key "\\.")))
             (val data))
        (dolist (k keys) (when (listp val) (setq val (cdr (assoc k val #'string=)))))
        (if (stringp val)
            (progn (find-file file) (goto-char (point-min))
                   (when (search-forward (format "\"%s\"" (car (last keys))) nil t)
                     (pulse-momentary-highlight-one-line (point)) (recenter)))
          (let ((new-val (read-string (format "创建翻译 [%s]: " inner-key))))
            (unless (string-empty-p new-val)
              (i18n-quick--save file (i18n-quick--update-alist data keys new-val))
              (message "[i18n] 已保存至 %s" (file-name-nondirectory file)))))))))

;; ==========================================
;; 4. Mode 定义
;; ==========================================

(defvar i18n-quick-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c i v") #'i18n-quick-view)
    (define-key map (kbd "C-c i j") #'i18n-quick-jump-or-create)
    map))

(define-minor-mode i18n-quick-mode
  "Quick i18n Minor Mode." :lighter " i18n" :keymap i18n-quick-mode-map)

;;TODO
;;1. 如果没有则创建一个
;;2. 支持多个语言 给用户选择
;;3. 添加这个view-mode 将i18显示成 当前类型

(provide 'i18n-quick)
