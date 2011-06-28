;; My emacs customizations

;; Keep all backups and autosave files hidden away
(setq
   backup-by-copying t
   backup-directory-alist '((".*" . "~/.emacs.d/backups/"))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t
   auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
(make-directory "~/.emacs.d/autosaves/" t)

;; Basic customizations
(setq enable-local-variables nil)
(cond ((fboundp 'global-font-lock-mode)
  (global-font-lock-mode t)
  (setq font-lock-maximum-decoration t)))
(line-number-mode t)
(column-number-mode t)
(which-func-mode t)
(size-indication-mode t)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(blink-cursor-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq next-line-add-newlines nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(mouse-wheel-mode t)
(xterm-mouse-mode 1)
(setq ring-bell-function 'ignore) ;; No beeps OR visible bell!
(setq frame-title-format (list (getenv "HOST") ":%f"))
(setq inhibit-startup-screen t)
(setq focus-follows-mouse nil)
(setq mouse-autoselect-window nil)
;; Don't let Custom modify this .emacs file
(setq custom-file "~/.emacs-custom.el")
(load custom-file 'noerror)
(auto-compression-mode 1)
;; Use <shift>+<arrow> to move between windows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
;; follow compilation output
(setq compilation-scroll-output t)
(setq echo-keystrokes 0.1)
;; jump to last location when reopening a file
(require 'saveplace)
(setq-default save-place t)

;; Colors & syntax highlighting
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
(make-face-italic 'font-lock-comment-face)
(make-face-italic 'font-lock-string-face)

;; highlight marked text (use C-space C-g to set a mark without highlighting)
(transient-mark-mode t)
(delete-selection-mode t)


;; Shortcuts
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\M-g" 'goto-line)
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)
(global-set-key "\C-cp" 'ps-print-buffer-with-faces)
(global-set-key "\C-cs" 'desktop-save)
(global-set-key "\C-cr" 'desktop-read)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key "\C-cy" 'python-shell)
(defun diff-buffer-against-disk ()
  "Compare current buffer to saved version on disk"
  (interactive)
  (diff-buffer-with-file (current-buffer)))
(global-set-key "\C-cd" 'diff-buffer-against-disk)

;; Launch a terminal without being prompted for the shell type
(defun launch-ansi-term ()
   (interactive)
   (ansi-term "/bin/tcsh" "terminal"))
(global-set-key "\C-ca" 'launch-ansi-term)

;; gfortran uses tabs, so it helps to be able to quickly switch to tab mode
(defun toggle-tabs-mode ()
  "Toggle indent-tabs-mode between t and nil."
  (interactive)
  (set-variable 'indent-tabs-mode (not indent-tabs-mode))
  (message "Tabs mode set to %s" indent-tabs-mode))
(global-set-key "\C-ct" 'toggle-tabs-mode)

;; Pin buffers to specific windows
(defun toggle-current-window-dedication ()
 (interactive)
 (let* ((window (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
            (if dedicated "no longer " "")
            (buffer-name))))
(global-set-key "\C-cw" 'toggle-current-window-dedication)

;; Quickly byte-compile and load .emacs.
(defun dotemacs ()
  (interactive)
  (byte-compile-file "~/.emacs")
  (load-file "~/.emacs.elc"))
(global-set-key "\C-cl" 'dotemacs)

;; Alternate ways to get M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm" 'execute-extended-command)

;; Single-key shortcuts
(global-set-key [f1] 'save-buffer)
(global-set-key [f2] 'find-file)
(global-set-key [f3] 'other-window)
(global-set-key [f4] 'buffer-menu)
(global-set-key [f10] 'kill-buffer)
(defun switch-to-todo ()
  (interactive)
  (find-file "~/Notes/work.org"))
(defun switch-to-notes ()
  (interactive)
  (find-file "~/Notes/notes.org"))
(global-set-key [f11] 'switch-to-todo)
(global-set-key [f12] 'switch-to-notes)

;; Modes
(setq auto-mode-alist
      (append '(("\\.f95\\'" . fortran-mode)
      ("\\.m$" . objc-mode)
      ("\\.mm$" . objc-mode)
      ("\\.org$" . org-mode)
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
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; Auto-indent in C, Python, etc.
(defun newline-indents ()
  (local-set-key "\C-m" 'newline-and-indent))
(add-hook 'c-mode-hook (function newline-indents))
(add-hook 'c++-mode-hook (function newline-indents))
(add-hook 'java-mode-hook (function newline-indents))
(add-hook 'python-mode-hook (function newline-indents))
(add-hook 'shell-script-mode-hook (function newline-indents))
(add-hook 'fortran-mode-hook (function newline-indents))
(add-hook 'objc-mode-hook (function newline-indents))
(require 'org-install)
(setq org-log-done t)
(define-key global-map "\C-co" 'org-agenda)
(setq org-agenda-files (list "~/Notes/work.org"
                             "~/Notes/home.org"))
(setq org-export-with-sub-superscripts nil)
(require 'org-faces)
;; org-level-4 takes its value from font-lock-comment-face
(make-face-unitalic 'org-level-4)
(make-face-unitalic 'org-level-8)
(set-face-foreground 'org-level-2 "ForestGreen")
(set-face-foreground 'org-level-3 "MidnightBlue")
(set-face-foreground 'org-level-4 "MediumBlue")
