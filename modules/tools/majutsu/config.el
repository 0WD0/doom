;;; tools/majutsu/config.el -*- lexical-binding: t; -*-

;; Declare autoloaded commands via use-package! for lazy loading
(use-package! majutsu
  :commands (majutsu majutsu-log
                     majutsu-rebase-transient majutsu-bookmark-transient majutsu-git-transient
                     majutsu-commit majutsu-describe majutsu-diff majutsu-diffedit-emacs majutsu-diffedit-smerge
                     majutsu-log-refresh majutsu-mode-transient majutsu-squash-transient
                     majutsu-abandon majutsu-undo majutsu-new majutsu-enter-dwim majutsu-goto-current))

;; Keybindings: `SPC j` prefix (evil leader)
(when (modulep! :editor evil)
  (map! :leader
        (:prefix ("j" . "Majutsu")
         :desc "Majutsu log"       "j" #'majutsu
         ;; :desc "Rebase menu"       "r" #'majutsu-rebase-transient
         ;; :desc "Bookmark menu"     "b" #'majutsu-bookmark-transient
         ;; :desc "Git menu"          "g" #'majutsu-git-transient
         :desc "Commit"            "c" #'majutsu-commit
         :desc "Describe"          "d" #'majutsu-describe
         :desc "Diff"              "D" #'majutsu-diff)))

;; Per-mode evil bindings (override upstream in a safe, lazy way)
(after! majutsu
  (when (modulep! :editor evil)
    (add-hook 'majutsu-mode-hook #'turn-off-evil-snipe-mode)
    (add-hook 'majutsu-mode-hook #'turn-off-evil-snipe-override-mode)
    (map! :map majutsu-mode-map
          :nv "." #'majutsu-goto-current

          :nv "R" #'majutsu-log-refresh
          :nv "gr" #'majutsu-log-refresh
          :nv "c" #'majutsu-commit
          :nv "e" #'majutsu-edit-changeset
          :nv "u" #'majutsu-undo
          :nv "C-r" #'majutsu-redo
          ;; :nv "N" #'majutsu-new
          :nv "s" #'majutsu-squash-transient
          :nv "d" #'majutsu-describe
          :nv "x" #'majutsu-abandon

          :nv "b" #'majutsu-bookmark-transient
          :nv "r" #'majutsu-rebase-transient
          ;; :nv "G" #'majutsu-git-transient

          :nv "D" #'majutsu-diff
          :nv "E" #'majutsu-diffedit-emacs
          :nv "M" #'majutsu-diffedit-smerge
          :nv "?" #'majutsu-mode-transient

	  :nv "]" #'magit-section-forward-sibling
	  :nv "[" #'magit-section-backward-sibling
          ;; Make RET consistent even if upstream changes default
          ;; :n  [remap magit-visit-thing] #'majutsu-enter-dwim
          :n  "RET" #'majutsu-enter-dwim
	  )))
