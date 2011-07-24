;; Asher Langton <langton@gmail.com>

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

(make-directory "~/.emacs.d/site-lisp/" t)
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(setq enable-local-variables :safe
      enable-local-eval nil
      initial-scratch-message nil
      next-line-add-newlines nil
      sentence-end-double-space nil
      ring-bell-function 'ignore ;; No beeps OR visible bell!
      inhibit-startup-screen t
      focus-follows-mouse nil
      mouse-autoselect-window nil)

(setq-default tab-width 8
              indent-tabs-mode nil)

(when (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1))

(setq font-lock-maximum-decoration 
      '((c-mode . 2) (c++-mode . 2) (python-mode . 2)
        (java-mode . 2) (t . 1)))

(line-number-mode t)
(column-number-mode t)
(which-func-mode t)
(size-indication-mode t)
(show-paren-mode 1)

(eval-after-load 'time
  '(progn
     (setq display-time-24hr-format t
           display-time-default-load-average nil
           display-time-day-and-date t)))
(display-time-mode 1)

(if (fboundp 'blink-cursor-mode)
    (blink-cursor-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(unless window-system
  (menu-bar-mode -1))
(mouse-wheel-mode t)
(xterm-mouse-mode 1)

(server-start)

;; For files, show the full path in the frame title. For all other buffers,
;; show the buffer name and default directory. 
(setq frame-title-format 
      (list '(buffer-file-name "%f" ("%b -- " default-directory))))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; Don't let Custom modify this .emacs file
(setq custom-file "~/.emacs-custom.el")
(load custom-file 'noerror)

(auto-compression-mode 1)

;; Use <shift>+<arrow> to move between windows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; follow compilation output
(eval-after-load 'compile
  '(setq compilation-scroll-output t))
(setq echo-keystrokes 0.1)

;; jump to last location when reopening a file
(require 'saveplace)
(setq-default save-place t)

;; Colors & syntax highlighting
(set-foreground-color "black")
(set-background-color "white")
(set-cursor-color "black")
(set-mouse-color "black")
(add-to-list 'default-frame-alist '(background-color . "white"))
(add-to-list 'default-frame-alist '(foreground-color . "black"))
(set-face-foreground 'font-lock-string-face "DarkRed")
(set-face-foreground 'font-lock-comment-face "forest green")
(set-face-foreground 'font-lock-keyword-face "MidnightBlue")
(set-face-foreground 'font-lock-function-name-face "blue")
(set-face-foreground 'font-lock-preprocessor-face "red")
(set-face-foreground 'font-lock-warning-face "red")
;; override all other syntax highlighting:
(set-face-foreground 'font-lock-type-face "black")
(set-face-foreground 'font-lock-variable-name-face "black")
(set-face-foreground 'font-lock-builtin-face "black")
(set-face-foreground 'font-lock-constant-face "black")
(make-face-bold 'font-lock-function-name-face)

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
(global-set-key "\C-cb" 'bury-buffer)
(defun diff-buffer-against-disk ()
  "Compare current buffer to saved version on disk"
  (interactive)
  (diff-buffer-with-file (current-buffer)))
(global-set-key "\C-cd" 'diff-buffer-against-disk)

;; Launch a terminal without being prompted for the shell type
(defun launch-ansi-term ()
  "Launch an ansi-term running /bin/tcsh with no prompts"
  (interactive)
  (ansi-term "/bin/tcsh"))
(global-set-key "\C-ca" 'launch-ansi-term)

;; Launch 3 ansi-terms, titled source, build, and test
(defun launch-sbt-terms ()
  "Launch an ansi-term running /bin/tcsh with no prompts"
  (interactive)
  (ansi-term "/bin/tcsh" "source")
  (ansi-term "/bin/tcsh" "build")
  (ansi-term "/bin/tcsh" "test"))
(global-set-key "\C-cm" 'launch-sbt-terms)

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

(defun dotemacs ()
  "Byte-compile and load ~/.emacs"
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
(global-set-key [f4] 'other-frame)
(global-set-key [f5] 'buffer-menu)
(global-set-key [f9] 'kill-buffer)
(defun switch-to-personal ()
  "Switch to personal todo list."
  (interactive)
  (find-file "~/personal/personal.org"))
(defun switch-to-todo ()
  "Switch to todo list."
  (interactive)
  (find-file "~/Notes/work.org"))
(defun switch-to-notes ()
  "Switch to note file."
  (interactive)
  (find-file "~/Notes/notes.org"))
(global-set-key [f10] 'switch-to-personal)
(global-set-key [f11] 'switch-to-todo)
(global-set-key [f12] 'switch-to-notes)

(defun now ()
  "Insert string for the current date time to be used as a filename, tag, etc."
  (interactive)
  (insert (format-time-string "%Y_%m_%d_%H_%M_%S")))
(global-set-key "\C-cn" 'now)

;; Stolen from Steve Yegge's .emacs, slightly modified
(defun swap-windows ()
 "If you have 2 windows, it swaps them."
 (interactive)
 (cond ((not (= (count-windows) 2)) 
        (message "You need exactly 2 windows to do this."))
       (t
        (let* ((w1 (elt (window-list) 0))
               (w2 (elt (window-list) 1))
               (b1 (window-buffer w1))
               (b2 (window-buffer w2))
               (s1 (window-start w1))
               (s2 (window-start w2)))
          (set-window-buffer w1 b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)))))
(global-set-key "\C-cu" 'swap-windows)

;; Modes
(require 'xscheme)
(setq auto-mode-alist
      (append '(("\\.f95\\'" . fortran-mode)
                ("\\.m$" . objc-mode)
                ("\\.mm$" . objc-mode)
                ("\\.org$" . org-mode)
                ("\\.build\\'" . compilation-minor-mode)
                ) auto-mode-alist))

;; Use d-mode for D if it exists; otherwise fall back to Java mode
(if (locate-library "d-mode")
    (progn 
      (autoload 'd-mode "d-mode" "Major mode for editing D code." t)
      (add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode)))
  (add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . java-mode)))

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(eval-after-load 'term
  '(setq term-buffer-maximum-size 100000))

;; auto fill for text, TeX, and LaTeX modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'TeX-mode-hook 'turn-on-auto-fill)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Auto-indent in C, Python, etc.
(defun newline-indents ()
  (local-set-key "\C-m" 'newline-and-indent))
(add-hook 'c-mode-common-hook (function newline-indents))
(add-hook 'python-mode-hook (function newline-indents))
(add-hook 'shell-script-mode-hook (function newline-indents))
(add-hook 'fortran-mode-hook (function newline-indents))

(defun hl-todo-fixme ()
  (font-lock-add-keywords 
   nil
   '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))))
(add-hook 'c-mode-common-hook 'hl-todo-fixme)
(add-hook 'python-mode-hook 'hl-todo-fixme)

(when (require 'org-install nil t)
  ;; Make windmove work in org-mode:
  (setq org-disputed-keys '(([(shift up)] . [(meta p)])
                            ([(shift down)] . [(meta n)])
                            ([(shift left)] . [(meta -)])
                            ([(shift right)] . [(meta +)])
                            ([(meta return)] . [(control meta return)])
                            ([(control shift right)] . [(meta shift +)])
                            ([(control shift left)] . [(meta shift -)])))
  (setq org-replace-disputed-keys t)
  (setq org-log-done t)
  (setq org-hide-leading-stars t)
  (setq org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" 
                                            "DONE(d)")
                                  (sequence "WAITING(w)" "SOMEDAY(s)" "|" 
                                            "CANCELLED(c)"))))
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("SOMEDAY" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold))))
  (setq org-agenda-files (list "~/Notes/work.org"
                               "~/personal/personal.org"))
  (setq org-export-with-sub-superscripts nil)
  (setq org-export-copy-to-kill-ring nil)
  
  (define-key global-map "\C-co" 'org-agenda)

  (defun wrap-org-ex (b e)
    "Wraps selection in an org-mode example box"
    (interactive "r")
    (save-excursion
      (goto-char e)
      (insert "\n#+END_EXAMPLE")
      (goto-char b)
      (insert "#+BEGIN_EXAMPLE\n")))

  (defun org-yank-code ()
    (interactive)
    (insert "#+BEGIN_EXAMPLE\n")
    (yank)
    (insert "\n#+END_EXAMPLE\n"))
  
  ;; Dropbox support for MobileOrg
  (when (file-accessible-directory-p "~/Dropbox")
    (setq org-directory "~/personal")
    (setq org-mobile-inbox-for-pull "~/personal/flagged.org")
    (setq org-mobile-directory "~/Dropbox/MobileOrg"))
  
  (add-hook 'org-mode-hook (lambda ()
                             (local-set-key "\C-ce" 'wrap-org-ex)
                             (local-set-key "\C-c0" 'org-export-as-html)
                             (local-set-key "\C-cy" 'org-yank-code))))

;; need perforce for some work projects, so load p4.el if available
(require 'p4 nil t)
