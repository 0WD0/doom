;;; ui/smooth-scroll/config.el -*- lexical-binding: t; -*-

;;
;;; Configuration

(defgroup neoscroll nil
  "Smooth scrolling for Emacs inspired by neoscroll.nvim"
  :group 'scrolling
  :prefix "neoscroll-")

(defcustom neoscroll-mappings
  '("C-u" "C-d" "C-b" "C-f" "C-y" "C-e")
  "Default key mappings for smooth scrolling."
  :type '(repeat string)
  :group 'neoscroll)

(defcustom neoscroll-hide-cursor nil
  "Hide cursor while scrolling (disabled due to compatibility issues)."
  :type 'boolean
  :group 'neoscroll)

(defcustom neoscroll-stop-eof t
  "Stop at EOF when scrolling downwards."
  :type 'boolean
  :group 'neoscroll)

(defcustom neoscroll-duration-multiplier 1.0
  "Global duration multiplier."
  :type 'float
  :group 'neoscroll)

(defcustom neoscroll-easing 'linear
  "Default easing function."
  :type '(choice (const linear) (const quadratic) (const cubic) (const sine))
  :group 'neoscroll)

(defcustom neoscroll-pre-hook nil
  "Function to run before scrolling animation starts."
  :type '(choice function null)
  :group 'neoscroll)

(defcustom neoscroll-post-hook nil
  "Function to run after scrolling animation ends."
  :type '(choice function null)
  :group 'neoscroll)

;;
;;; Core variables

(defvar neoscroll--timer nil "Current animation timer")
(defvar neoscroll--active nil "Whether animation is active")
(defvar neoscroll--interrupt-flag nil "Interrupt flag")

;;
;;; Easing functions

(defun neoscroll--easing-linear (progress)
  "Linear easing function."
  progress)

(defun neoscroll--easing-quadratic (progress)
  "Quadratic easing function."
  (* progress progress))

(defun neoscroll--easing-cubic (progress)
  "Cubic easing function."
  (- 1 (expt (- 1 progress) 3)))

(defun neoscroll--easing-sine (progress)
  "Sine easing function."
  (- 1 (cos (* progress (/ pi 2)))))

(defun neoscroll--apply-easing (progress easing)
  "Apply EASING function to PROGRESS."
  (pcase easing
    ('linear (neoscroll--easing-linear progress))
    ('quadratic (neoscroll--easing-quadratic progress))
    ('cubic (neoscroll--easing-cubic progress))
    ('sine (neoscroll--easing-sine progress))
    (_ progress)))

;;
;;; Core functions

(defun neoscroll--interrupt (&rest _)
  "Interrupt current animation."
  (when neoscroll--timer
    (cancel-timer neoscroll--timer)
    (setq neoscroll--timer nil
          neoscroll--active nil
          neoscroll--interrupt-flag t)
    ;; Clean up highlighting when interrupted
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (overlay-get overlay 'hl-line)
        (delete-overlay overlay)))
    (when (or (bound-and-true-p hl-line-mode)
              (bound-and-true-p global-hl-line-mode))
      (run-hooks 'post-command-hook))))

(defun neoscroll--check-input ()
  "Check if there's pending input and interrupt if so."
  (when (and neoscroll--active (input-pending-p))
    (neoscroll--interrupt)
    t))

(defun neoscroll--cursor-hide ()
  "Hide cursor during scrolling."
  nil)

(defun neoscroll--cursor-show ()
  "Show cursor after scrolling."
  nil)

(defun neoscroll--line-pixel-height ()
  "Get line height in pixels."
  (if (display-graphic-p)
      (line-pixel-height)
    1))

(defun neoscroll--window-pixel-height ()
  "Get window height in pixels."
  (if (display-graphic-p)
      (window-pixel-height)
    (window-height)))

(defun neoscroll--scroll-pixels (pixels)
  "Scroll by PIXELS amount."
  (let ((pixels (round pixels)))
    (when (not (zerop pixels))
      (if (and (display-graphic-p) 
               (fboundp 'pixel-scroll-precision-scroll-down))
          (if (> pixels 0)
              (pixel-scroll-precision-scroll-down pixels)
            (pixel-scroll-precision-scroll-up (- pixels)))
        ;; Fallback to line-based scrolling
        (let ((lines (ceiling (/ (float (abs pixels)) (neoscroll--line-pixel-height)))))
          (when (> lines 0)
            (if (> pixels 0)
                (scroll-up lines)
              (scroll-down lines))))))))

;;
;;; Main scrolling function

(defun neoscroll-scroll (lines &optional opts)
  "Scroll LINES with options OPTS."
  (let* ((opts (or opts '()))
         (duration (or (plist-get opts :duration) 0.25))
         (easing (or (plist-get opts :easing) neoscroll-easing))
         (move-cursor (if (plist-member opts :move-cursor) 
                          (plist-get opts :move-cursor) 
                        t))
         (info (plist-get opts :info))
         (duration (* duration neoscroll-duration-multiplier))
         (steps 30)
         (delay (/ duration steps))
         (step-count 0)
         (start-line (line-number-at-pos))
         (start-col (current-column))
         (start-goal-col (or goal-column start-col))
         (line-height (neoscroll--line-pixel-height))
         (total-pixels (* lines line-height))
         (pixels-per-step (/ total-pixels steps)))
    
    ;; Interrupt any ongoing animation
    (neoscroll--interrupt)
    
    ;; Early exit if no scrolling needed  
    (unless (zerop lines)
      
      ;; Run pre-hook
      (when neoscroll-pre-hook
        (funcall neoscroll-pre-hook info))
      
      ;; Hide cursor
      (neoscroll--cursor-hide)
      
      ;; Clear any existing highlight overlays to prevent ghosting
      (when (bound-and-true-p hl-line-mode)
        (hl-line-unhighlight)
        ;; Force removal of any stray hl-line overlays
        (dolist (overlay (overlays-in (point-min) (point-max)))
          (when (overlay-get overlay 'hl-line)
            (delete-overlay overlay))))
      (when (bound-and-true-p global-hl-line-mode)
        (global-hl-line-unhighlight))
      
      ;; Set up animation state
      (setq neoscroll--active t
            neoscroll--interrupt-flag nil
            goal-column start-goal-col)
      
      ;; Define animation step function
      (cl-labels ((animate-step ()
                    (cond
                     ;; Check for interruption
                     ((or neoscroll--interrupt-flag 
                          (neoscroll--check-input)
                          (>= step-count steps))
                      (neoscroll--cursor-show)
                      ;; Final cleanup: remove all hl-line overlays and refresh
                      (dolist (overlay (overlays-in (point-min) (point-max)))
                        (when (overlay-get overlay 'hl-line)
                          (delete-overlay overlay)))
                      ;; Trigger proper highlight refresh
                      (when (or (bound-and-true-p hl-line-mode)
                                (bound-and-true-p global-hl-line-mode))
                        (run-hooks 'post-command-hook))
                      (when neoscroll-post-hook
                        (funcall neoscroll-post-hook info))
                      (setq neoscroll--timer nil
                            neoscroll--active nil))
                     
                     ;; Continue animation
                     (t
                      (let* ((progress (/ (float step-count) steps))
                             (eased-progress (neoscroll--apply-easing progress easing))
                             (current-pixels (* total-pixels eased-progress))
                             (prev-progress (if (> step-count 0)
                                               (/ (float (1- step-count)) steps)
                                             0.0))
                             (prev-pixels (* total-pixels 
                                            (neoscroll--apply-easing prev-progress easing)))
                             (step-pixels (- current-pixels prev-pixels)))
                        
                        ;; Move cursor and scroll together for smooth experience
                        (if move-cursor
                            ;; Move cursor first, then adjust window view
                            (let ((target-line (+ start-line 
                                                 (round (* lines eased-progress)))))
                              (goto-char (point-min))
                              (forward-line (1- target-line))
                              (move-to-column start-goal-col)
                              ;; Keep cursor in same relative position as start
                              (let ((current-window-line (- (line-number-at-pos) 
                                                           (line-number-at-pos (window-start)))))
                                ;; Use pixel scrolling only for window adjustment
                                (neoscroll--scroll-pixels step-pixels))
                              ;; Force highlight refresh
                              (when (bound-and-true-p hl-line-mode)
                                (run-hooks 'post-command-hook))
                              (when (bound-and-true-p global-hl-line-mode)
                                (run-hooks 'post-command-hook)))
                          ;; Just scroll without moving cursor
                          (neoscroll--scroll-pixels step-pixels))
                        
                        (setq step-count (1+ step-count))
                        (setq neoscroll--timer 
                              (run-with-timer delay nil #'animate-step)))))))
        
        ;; Start animation
        (animate-step)))))

;;
;;; Predefined scroll commands

(defun neoscroll-ctrl-u (&optional opts)
  "Smooth scroll up half page."
  (interactive)
  (let ((scroll-amount (max 1 (/ (window-height) 2))))
    (neoscroll-scroll (- scroll-amount)
                     (append opts '(:duration 0.25)))))

(defun neoscroll-ctrl-d (&optional opts)
  "Smooth scroll down half page."
  (interactive)
  (let ((scroll-amount (max 1 (/ (window-height) 2))))
    (neoscroll-scroll scroll-amount
                     (append opts '(:duration 0.25)))))

(defun neoscroll-ctrl-b (&optional opts)
  "Smooth scroll up full page."
  (interactive)
  (neoscroll-scroll (- (window-height))
                   (append opts '(:duration 0.35))))

(defun neoscroll-ctrl-f (&optional opts)
  "Smooth scroll down full page."
  (interactive)
  (neoscroll-scroll (window-height)
                   (append opts '(:duration 0.35))))

(defun neoscroll-ctrl-y (&optional opts)
  "Smooth scroll up one line."
  (interactive)
  (neoscroll-scroll -1
                   (append opts '(:duration 0.1 :move-cursor nil))))

(defun neoscroll-ctrl-e (&optional opts)
  "Smooth scroll down one line."
  (interactive)
  (neoscroll-scroll 1
                   (append opts '(:duration 0.1 :move-cursor nil))))

;;
;;; Setup function

(defun neoscroll-setup (&optional config)
  "Setup neoscroll with CONFIG options."
  (when config
    (cl-loop for (key . value) in config do
             (set (intern (concat "neoscroll-" (symbol-name key))) value)))
  
  ;; Enable pixel scrolling if available
  (when (and (display-graphic-p)
             (fboundp 'pixel-scroll-precision-mode))
    (pixel-scroll-precision-mode 1))
  
  ;; Set up key mappings
  (let ((keymap-alist '(("C-u" . neoscroll-ctrl-u)
                        ("C-d" . neoscroll-ctrl-d)
                        ("C-b" . neoscroll-ctrl-b)
                        ("C-f" . neoscroll-ctrl-f)
                        ("C-y" . neoscroll-ctrl-y)
                        ("C-e" . neoscroll-ctrl-e))))
    (dolist (mapping neoscroll-mappings)
      (when-let ((command (cdr (assoc mapping keymap-alist))))
        (global-set-key (kbd mapping) command)))))

;;
;;; Integration with Evil mode

(after! evil
  (defun +neoscroll--evil-wrapper (func)
    "Wrap FUNC to interrupt animations."
    (lambda (&rest args)
      (interactive)
      (neoscroll--interrupt)
      (apply func args)))
  
  ;; Override default evil movement commands
  (advice-add 'evil-next-line :before #'neoscroll--interrupt)
  (advice-add 'evil-previous-line :before #'neoscroll--interrupt)
  (advice-add 'evil-forward-char :before #'neoscroll--interrupt)
  (advice-add 'evil-backward-char :before #'neoscroll--interrupt)
  
  ;; Emergency stop
  (defun +neoscroll-emergency-stop ()
    "Emergency stop all animations."
    (interactive)
    (neoscroll--interrupt)
    (keyboard-quit))
  
  (map! :map evil-normal-state-map
        "C-d" #'neoscroll-ctrl-d
        "C-u" #'neoscroll-ctrl-u
        "C-f" #'neoscroll-ctrl-f
        "C-b" #'neoscroll-ctrl-b
        "C-y" #'neoscroll-ctrl-y  
        "C-e" #'neoscroll-ctrl-e
        "<escape>" #'+neoscroll-emergency-stop
        "C-g" #'+neoscroll-emergency-stop))

;;
;;; Auto-setup

(neoscroll-setup)