;;; tools/majutsu/config.el -*- lexical-binding: t; -*-

;; Declare autoloaded commands via use-package! for lazy loading
(use-package! majutsu
  :defer-incrementally (dash f s with-editor package eieio transient))

;; Keybindings: `SPC j` prefix (evil leader)
(when (modulep! :editor evil)
  (map! :leader
        (:prefix ("j" . "Majutsu")
         :desc "Majutsu log"       "j" #'majutsu
         ;; :desc "Rebase menu"       "r" #'majutsu-rebase-transient
         ;; :desc "Bookmark menu"     "b" #'majutsu-bookmark-transient
         ;; :desc "Git menu"          "g" #'majutsu-git-transient
         :desc "Commit"            "C" #'majutsu-commit
         :desc "Describe"          "c" #'majutsu-describe
         :desc "Diff"              "d" #'majutsu-diff)))
