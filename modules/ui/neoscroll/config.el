;;; ui/neoscroll/config.el -*- lexical-binding: t; -*-

(use-package! neoscroll
  :hook (doom-first-input . neoscroll-mode)
  :config
  ;; 自定义配置
  (setq neoscroll-easing 'linear
        neoscroll-scroll-duration 0.15
        neoscroll-page-duration 0.25
        neoscroll-line-duration 0.05))
