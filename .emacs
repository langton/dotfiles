(setq backup-inhibited 1) ;; no x.x~ files !!!
(setq enable-local-variables nil)
(cond ((fboundp 'global-font-lock-mode)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)))
(line-number-mode t)
(column-number-mode t)
(blink-cursor-mode -1) 
(tool-bar-mode -1) 
(setq next-line-add-newlines nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(mouse-wheel-mode t)
(xterm-mouse-mode 1)
;; No beeps OR visible bell!
(setq ring-bell-function 'ignore)
(setq frame-title-format (list (getenv "HOST") ":%f"))
(setq inhibit-startup-screen t)
(setq focus-follows-mouse nil)
(setq mouse-autoselect-window nil)


;; Colors and formatting
(setq fgcolor "black")
(set-cursor-color (symbol-value 'fgcolor))
(set-mouse-color (symbol-value 'fgcolor))
(add-to-list 'default-frame-alist '(background-color . "white"))
(add-to-list 'default-frame-alist '(foreground-color . "black"))
(set-face-foreground 'font-lock-string-face "gray33")
(set-face-foreground 'font-lock-comment-face "forest green")
(set-face-foreground 'font-lock-keyword-face "MidnightBlue")
(set-face-foreground 'font-lock-function-name-face "blue")
(set-face-foreground 'font-lock-preprocessor-face "red")
;; override all other syntax highlighting:
(set-face-foreground 'font-lock-type-face (symbol-value 'fgcolor))
(set-face-foreground 'font-lock-variable-name-face (symbol-value 'fgcolor))
(set-face-foreground 'font-lock-builtin-face (symbol-value 'fgcolor))
(set-face-foreground 'font-lock-constant-face (symbol-value 'fgcolor))
(set-face-foreground 'font-lock-warning-face (symbol-value 'fgcolor))
(make-face-bold 'font-lock-function-name-face)
;; highlight marked text
(transient-mark-mode t)
(delete-selection-mode t)


;; Shortcuts
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-cd" 'normal-erase-is-backspace-mode)
(global-set-key [home] 'beginning-of-buffer) 
(global-set-key [end] 'end-of-buffer)
(global-set-key "\C-cp" 'ps-print-buffer-with-faces)
;; gfortran uses tabs, so it helps to be able to toggle
(defun toggle-tabs-mode ()
  "Toggle indent-tabs-mode between t and nil."
  (interactive)
  (set-variable 'indent-tabs-mode (not indent-tabs-mode))
  (message "Tabs mode set to %s" indent-tabs-mode))
(global-set-key "\C-ct" 'toggle-tabs-mode)


;; Modes
(setq auto-mode-alist 
      (append '(("\\.f95\\'" . fortran-mode)
      ("\\.m$" . objc-mode)
      ("\\.mm$" . objc-mode)
      ) auto-mode-alist))
;; C indentation
(add-hook 'c-mode-hook '(lambda () (setq c-indent-level 2)))
(add-hook 'c++-mode-hook '(lambda () (setq c-indent-level 2)))
(add-hook 'java-mode-hook '(lambda () (setq c-indent-level 2)))
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; auto fill for text, TeX, and LaTeX modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'TeX-mode-hook 'turn-on-auto-fill)

;; Auto-indent in C, Python, etc.
(defun newline-indents ()
  (local-set-key "\C-m" 'newline-and-indent))
(add-hook 'c-mode-hook (function newline-indents))
(add-hook 'c++-mode-hook (function newline-indents))
(add-hook 'java-mode-hook (function newline-indents))
(add-hook 'python-mode-hook (function newline-indents))
(add-hook 'shell-mode-hook (function newline-indents))
(add-hook 'fortran-mode-hook (function newline-indents))
(add-hook 'objc-mode-hook (function newline-indents))