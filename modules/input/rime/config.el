;;; input/rime/config.el -*- lexical-binding: t; -*-

(use-package! rime
  :after-call after-find-file pre-command-hook
  :init
  (setq rime-user-data-dir (concat doom-cache-dir "rime/"))
  :config
  (setq default-input-method "rime")

  (after! evil-escape
    (defun +rime--input-method-p ()
      current-input-method)
    (add-to-list 'evil-escape-inhibit-functions #'+rime--input-method-p))

  (when (modulep! +childframe)
    (setq rime-show-candidate 'posframe))

  ;; allow vertico/selectrum search with pinyin
  (cond ((modulep! :completion vertico)
         (advice-add #'orderless-regexp
                     :filter-return
                     (if (modulep! :editor evil +everywhere)
                         #'evil-pinyin--build-regexp-string ;;貌似正常情况下不会有 pyim 依赖，下面那个 else 就不改了
                       #'pyim-cregexp-build)))))

(use-package! evil-pinyin
  :when (modulep! :editor evil +everywhere)
  :after evil
  :init
  (setq-default evil-pinyin-scheme 'simplified-weiruan-all) ;;这对我非常重要！！！
  :config
  (setq-default evil-pinyin-with-search-rule 'always)
  (global-evil-pinyin-mode 1))

(use-package! ace-pinyin
  :after avy
  :config (ace-pinyin-global-mode t))

;;
;;; Hacks

(defadvice! +rime--org-html-paragraph-a (args)
  "Join consecutive Chinese lines into a single long line without unwanted space
when exporting org-mode to html."
  :filter-args #'org-html-paragraph
  (cl-destructuring-bind (paragraph contents info) args
    (let* ((fix-regexp "[[:multibyte:]]")
           (fixed-contents
            (replace-regexp-in-string
             (concat "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)")
             "\\1\\2"
             contents)))
      (list paragraph fixed-contents info))))
