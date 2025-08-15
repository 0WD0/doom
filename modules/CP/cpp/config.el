;;; CP/cpp/config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; This module configures cp-cpp for competitive programming C++ development
;; in Doom Emacs. It provides enhanced compilation, testing, and debugging
;; capabilities specifically designed for competitive programming workflows.

;;; Configuration:

(use-package! cp-cpp
  :after cc-mode
  :init
  ;; Auto-enable cp-cpp-mode for C++ files
  (add-hook 'c++-mode-hook #'cp-cpp-mode)
  
  :config
  ;; Default configuration optimized for competitive programming
  (setq cp-cpp-io-directory (expand-file-name "~/CP/IO")
        cp-cpp-compile-flags "-std=c++23 -Wshadow -Wconversion -Wall -static-libgcc"
        cp-cpp-optimize-flags "-O2"
        cp-cpp-debug-flags "-g"
        cp-cpp-compiler "g++"
        cp-cpp-enable-compile-cache t
        cp-cpp-layout-type 'mixed
        )
  
  ;; Ensure IO directory exists
  (unless (file-directory-p cp-cpp-io-directory)
    (make-directory cp-cpp-io-directory t))

  ;; Doom Emacs specific keybindings
  (map! :localleader
        :map c++-mode-map
        ;; :prefix ("c" . "competitive programming")
        
        ;; Basic operations
        "r" #'cp-cpp-run
        "d" #'cp-cpp-run-debug
        "g" #'cp-cpp-start-gdb
        
        ;; Utilities
        "b" #'cp-cpp-benchmark
        ;; "l" #'cp-cpp-clean-outputs
        "i" #'cp-cpp-show-status
        
        ;; Multi-window layout
        "o" #'cp-cpp-open-layout
        "w" #'cp-cpp-switch-test-case
        "R" #'cp-cpp-run-and-show
        "f" #'cp-cpp-refresh-output
        "." #'cp-cpp-next-test-case
        "," #'cp-cpp-prev-test-case
        "W" #'cp-cpp-configure-layout
        
        ;; Configuration
        "C" #'cp-cpp-customize-compile-options
        "S" #'cp-cpp-show-compile-command
        "E" #'cp-cpp-configure-terminal
        "D" #'cp-cpp-configure-terminal-display
        "!" #'cp-cpp-test-terminal)
  
  ;; Test management submenu
  (map! :localleader
        :map c++-mode-map
        :prefix ("c t" . "test")
        "t" #'cp-cpp-toggle-input-mode
        "s" #'cp-cpp-select-test-case
        "n" #'cp-cpp-create-test-case
        "a" #'cp-cpp-run-all-tests
        "c" #'cp-cpp-compare-output)
  
  ;; Cache management submenu
  (map! :localleader
        :map c++-mode-map
        :prefix ("c x" . "cache")
        "c" #'cp-cpp-clear-compile-cache
        "i" #'cp-cpp-show-cache-info)

  ;; Optional: Function key bindings for quick access
  (map! :map c++-mode-map
        "M-n" #'cp-cpp-run
  ;;       "<f5>" #'cp-cpp-run
  ;;       "<f6>" #'cp-cpp-run-debug
  ;;       "<f7>" #'cp-cpp-run-all-tests
  ;;       "<f8>" #'cp-cpp-toggle-input-mode
        )

  ;; Integration with Doom's popup system
  (set-popup-rule! "^\\*C\\+\\+ Test Results\\*" 
    :side 'bottom :size 0.3 :select t :quit t)
  (set-popup-rule! "^\\*C\\+\\+ Cache Info\\*" 
    :side 'right :size 0.4 :select t :quit t))

;;; Performance optimizations for competitive programming

;; Disable unnecessary features that might slow down compilation/execution
(when (modulep! :config default)
  (add-hook! 'cp-cpp-mode-hook
    (defun +cp-cpp-optimize-performance-h ()
      "Optimize performance for competitive programming."
      ;; Disable spell checking for better performance
      (when (bound-and-true-p flyspell-mode)
        (flyspell-mode -1))
      ;; Disable unnecessary syntax checkers
      (when (bound-and-true-p flycheck-mode)
        (setq-local flycheck-disabled-checkers 
                    '(c/c++-clang c/c++-gcc c/c++-cppcheck))))))

;;; Template and snippet integration

;; Integration with yasnippet for competitive programming templates
(when (modulep! :editor snippets)
  (defvar +cp-cpp-template-dir 
    (expand-file-name "cp-cpp/" doom-user-dir)
    "Directory containing cp-cpp templates.")
  
  (defun +cp-cpp-load-template (template-name)
    "Load a competitive programming template."
    (interactive 
     (list (completing-read "Template: " 
                            (when (file-directory-p +cp-cpp-template-dir)
                              (directory-files +cp-cpp-template-dir nil "\\.cpp$")))))
    (when (file-exists-p (expand-file-name template-name +cp-cpp-template-dir))
      (insert-file-contents (expand-file-name template-name +cp-cpp-template-dir))))
  
  (map! :localleader
        :map c++-mode-map
        :prefix "c"
        "T" #'+cp-cpp-load-template))

;;; Workspace integration

;; Integration with Doom's workspace system
(when (modulep! :ui workspaces)
  (defun +cp-cpp-new-workspace ()
    "Create a new workspace for competitive programming."
    (interactive)
    (let ((name (read-string "Problem name: ")))
      (+workspace-new name)
      (find-file (expand-file-name (concat name ".cpp") "~/CP/"))
      (when cp-cpp-mode
        (cp-cpp-create-test-case))))
  
  (map! :leader
        :desc "New CP workspace" "TAB N" #'+cp-cpp-new-workspace))

;;; Additional integrations

;; Company completion for competitive programming includes
(when (modulep! :completion company)
  (defvar +cp-cpp-headers
    '("#include <algorithm>"
      "#include <array>"
      "#include <bitset>"
      "#include <cassert>"
      "#include <chrono>"
      "#include <cmath>"
      "#include <deque>"
      "#include <iomanip>"
      "#include <iostream>"
      "#include <map>"
      "#include <numeric>"
      "#include <queue>"
      "#include <random>"
      "#include <set>"
      "#include <stack>"
      "#include <string>"
      "#include <unordered_map>"
      "#include <unordered_set>"
      "#include <vector>")
    "Common headers for competitive programming.")
  
  (defun +cp-cpp-insert-common-includes ()
    "Insert common competitive programming includes."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (insert (mapconcat 'identity +cp-cpp-headers "\n") "\n\n")))
  
  (map! :localleader
        :map c++-mode-map
        :prefix "c"
        "I" #'+cp-cpp-insert-common-includes))
