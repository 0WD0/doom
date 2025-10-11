;; -*- no-byte-compile: t; -*-
;;; tools/majutsu/packages.el

;; Ensure magit (and its dependency transient) are available, since majutsu
;; relies on magit-section and transient UIs.
(package! magit)

;; Use the local development repo. The directory name must match a directory
;; under straight's repos dir (usually ~/.config/emacs/.local/straight/repos/).
;; A symlink named "majutsu" is already present and points there.
(package! majutsu
  :recipe (:local-repo "majutsu"
           :files ("*.el")))

