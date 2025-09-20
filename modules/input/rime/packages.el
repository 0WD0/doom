;; -*- no-byte-compile: t; -*-
;;; input/rime/packages.el

(package! rime)
(package! pangu-spacing :pin "6509df9c90bbdb9321a756f7ea15bb2b60ed2530")
(when (modulep! +childframe)
  (package! posframe :pin "12f540c9ad5da09673b2bca1132b41f94c134e82"))
(package! ace-pinyin
  :recipe (:local-repo "ace-pinyin"))
(when (modulep! :editor evil +everywhere)
  (package! evil-pinyin
    :recipe (:build (:not autoloads))
    :pin "0fae5ad8761417f027b33230382a50f826ad3bfb"))
