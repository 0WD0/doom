;;; input/rime/config.el -*- lexical-binding: t; -*-

(use-package! rime
  :after-call after-find-file pre-command-hook
  :init
  (setq rime-user-data-dir (concat doom-cache-dir "rime/"))
  :config
  (setq default-input-method "rime"
        rime-show-candidate 'posframe)

  (after! evil-escape
    (defun +rime--input-method-p ()
      current-input-method)
    (add-to-list 'evil-escape-inhibit-functions #'+rime--input-method-p))

  (when (modulep! +childframe)
    (setq rime-show-candidate 'posframe)))


(use-package! pangu-spacing
  :hook (text-mode . pangu-spacing-mode)
  :config
  ;; Always insert `real' space in org-mode.
  (setq-hook! 'org-mode-hook pangu-spacing-real-insert-separtor t))

(use-package! evil-pinyin
  :when (modulep! :editor evil +everywhere)
  :after evil
  :init
  (setq-default evil-pinyin-scheme 'simplified-weiruan-all) ;;这对我非常重要！！！
  :config
  (setq-default evil-pinyin-with-search-rule 'always)
  (global-evil-pinyin-mode 1))

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
