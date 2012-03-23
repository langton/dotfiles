;; Asher Langton <langton@gmail.com>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic settings and preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local lisp files
(require 'cl) ; framemove.el needs common lisp

(make-directory "~/.emacs.d/site-lisp/" t)
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; Keep all backups and autosave files hidden away
(setq backup-by-copying t
      backup-directory-alist '((".*" . "~/.emacs.d/backups/"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t))
      desktop-path '("~/.emacs.d/")
      desktop-dirname "~/.emacs.d/"
      desktop-base-file-name "emacs-desktop")
(make-directory "~/.emacs.d/autosaves/" t)

;; Some mode-line settings and other preferences
(line-number-mode t)
(column-number-mode t)
(which-func-mode t)
(show-paren-mode 1)
(transient-mark-mode t)
(delete-selection-mode t)
(menu-bar-mode -1)
(if (fboundp 'savehist-mode)
    (savehist-mode 1))
(winner-mode 1)
(auto-compression-mode 1)
(if (fboundp 'mouse-wheel-mode)
    (mouse-wheel-mode t))
(if (fboundp 'blink-cursor-mode)
    (blink-cursor-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1))

(iswitchb-mode 1)
(setq iswitchb-default-method 'samewindow)
(setq iswitchb-buffer-ignore '("^ " "*Buffer" "*Messages" "*Help" "*Calendar"
                               "*Compile" "*Completions" "*vc" "*tramp"))
(require 'edmacro)
(defun iswitchb-local-keys ()
  (mapc (lambda (K)
          (let* ((key (car K)) (fun (cdr K)))
            (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
        '(("<right>" . iswitchb-next-match)
          ("<left>"  . iswitchb-prev-match)
          ("<up>"    . ignore             )
          ("<down>"  . ignore             ))))

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)
(defadvice iswitchb-kill-buffer (after rescan-after-kill activate)
  "*Regenerate the list of matching buffer names after a kill.
    Necessary if using `uniquify' with `uniquify-after-kill-buffer-p'
    set to non-nil."
  (setq iswitchb-buflist iswitchb-matches)
  (iswitchb-rescan))
(defun iswitchb-rescan ()
  "*Regenerate the list of matching buffer names."
  (interactive)
  (iswitchb-make-buflist iswitchb-default)
  (setq iswitchb-rescan t))

(require 'saveplace)
(require 'uniquify)
(require 'tramp)
(require 'paren)

(setq enable-local-variables :safe
      enable-local-eval nil
      initial-scratch-message nil
      next-line-add-newlines nil
      sentence-end-double-space nil
      ring-bell-function 'ignore ;; No beeps OR visible bell!
      inhibit-startup-screen t
      focus-follows-mouse nil
      mouse-autoselect-window nil
      vc-follow-symlinks t
      word-wrap t
      font-lock-maximum-decoration 2
      echo-keystrokes 0.1
      uniquify-buffer-name-style 'post-forward
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      custom-file "~/.emacs-custom.el"
      tramp-default-method "ssh"
      scroll-preserve-screen-position t
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(3 ((shift) . 1))
      set-mark-command-repeat-pop t
      bookmark-save-flag 1
      xterm-mouse-mode 1)

(load custom-file 'noerror)

(setq-default tab-width 8
              indent-tabs-mode nil
              save-place t)

; Monaco font is nice on OS X; try to use it on Linux too. (This
; doesn't get read in when running in daemon mode, so we need to set
; this in .Xdefaults as well.)
(when (and (eq window-system 'x)
           (x-list-fonts "Monaco"))
  (set-face-attribute 'default nil :family "Monaco" :height 110))
;; Fonts show up smaller on OS X. Make them bigger.
(when (eq window-system 'ns)
  (set-face-attribute 'default nil :family "Monaco" :height 130))

;; use server/emacsclient when possible
(require 'server)
(if (fboundp 'server-running-p)
    (unless (server-running-p)
      (server-start)))

;; For files, show the full path in the frame title. For all other buffers,
;; show the buffer name and default directory.
(setq frame-title-format
      `(,system-name ": " (buffer-file-name 
                           "%f" ("%b (" default-directory ")"))))

;; Use <shift>+<arrow> to move between windows
(when (require 'windmove nil t)
  (windmove-default-keybindings))

(when (require 'framemove nil t)
  (setq framemove-hook-into-windmove t))

;; follow compilation output
(eval-after-load 'compile
  '(setq compilation-scroll-output t))

(defadvice show-paren-function
  (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (interactive)
  (if (not (minibuffer-prompt))
      (let ((matching-text nil))
        ;; Only call `blink-matching-open' if the character before point
        ;; is a close parentheses type character. Otherwise, there's not
        ;; really any point, and `blink-matching-open' would just echo
        ;; "Mismatched parentheses", which gets really annoying.
        (if (char-equal (char-syntax (char-before (point))) ?\))
            (setq matching-text (blink-matching-open)))
        (if (not (null matching-text))
            (message matching-text)))))

; A couple of customizations for when I'm running Aquamacs
(when (boundp 'aquamacs-version)
  (tabbar-mode -1)
  (one-buffer-one-frame-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colors & syntax highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Cocoa Emacs 23 doesn't invert the cursor glyph properly, so I'll make t
;; a red bar instead of a black box.
(defvar curscolor "black")
(when (and (eq emacs-major-version 23) (eq window-system 'ns))
  (setq-default cursor-type 'bar)
  (setq curscolor "red"))

(set-foreground-color "black")
(set-background-color "white")
(set-cursor-color curscolor)
(set-mouse-color "black")
(add-to-list 'default-frame-alist '(background-color . "white"))
(add-to-list 'default-frame-alist '(foreground-color . "black"))
(add-to-list 'default-frame-alist `(cursor-color . ,curscolor))
(set-face-attribute 'mode-line nil
                    :box '(:line-width 2 :color "red" :style nil))
(set-face-background 'fringe "gray85")
(set-face-foreground 'font-lock-string-face "chocolate4")
(set-face-foreground 'font-lock-comment-face "forest green")
(set-face-foreground 'font-lock-keyword-face "MidnightBlue")
(set-face-foreground 'font-lock-function-name-face "blue")
(set-face-foreground 'font-lock-preprocessor-face "red")
(set-face-foreground 'font-lock-warning-face "red")
(set-face-background 'show-paren-match "light gray")
;; override all other syntax highlighting:
(set-face-foreground 'font-lock-type-face "black")
(set-face-foreground 'font-lock-variable-name-face "black")
(set-face-foreground 'font-lock-builtin-face "black")
(set-face-foreground 'font-lock-constant-face "black")
(make-face-bold 'font-lock-function-name-face)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions and key-bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun diff-buffer-against-disk ()
  "Compare current buffer to saved version on disk"
  (interactive)
  (diff-buffer-with-file (current-buffer)))

;; Launch a terminal without being prompted for the shell type
(defun launch-ansi-term ()
  "Launch an ansi-term running /bin/tcsh with no prompts"
  (interactive)
  (ansi-term "/bin/tcsh"))

;; Launch 3 ansi-terms, titled source, build, and test
(defun launch-sbt-terms ()
  "Launch an ansi-term running /bin/tcsh with no prompts"
  (interactive)
  (ansi-term "/bin/tcsh" "term-source")
  (ansi-term "/bin/tcsh" "term-build")
  (ansi-term "/bin/tcsh" "term-test"))

;; gfortran uses tabs, so it helps to be able to quickly switch to tab mode
(defun toggle-tabs-mode ()
  "Toggle indent-tabs-mode between t and nil."
  (interactive)
  (set-variable 'indent-tabs-mode (not indent-tabs-mode))
  (message "Tabs mode set to %s" indent-tabs-mode))

;; Pin buffers to specific windows
(defun toggle-current-window-dedication ()
  (interactive)
  (let* ((window (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

;; Stolen from Steve Yegge's .emacs
(defun swap-windows ()
 "If you have 2 windows, it swaps them."
 (interactive)
 (cond ((not (= (count-windows) 2))
        (message "You need exactly 2 windows to do this."))
       (t
        (let* ((w1 (first (window-list)))
               (w2 (second (window-list)))
               (b1 (window-buffer w1))
               (b2 (window-buffer w2))
               (s1 (window-start w1))
               (s2 (window-start w2)))
          (set-window-buffer w1 b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)))))

;; Work-around to maximize frame on OS X
(defun maximize-frame ()
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 1000 1000))

(defun toggle-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

;; Global shortcuts
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\M-g" 'goto-line)
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)
(global-set-key "\C-cp" 'ps-print-buffer-with-faces)
(global-set-key "\C-cs" 'desktop-save-in-desktop-dir)
(global-set-key "\C-cr" 'desktop-read)
(global-set-key "\C-x\C-b" 'buffer-menu)
(global-set-key "\C-cb" 'bury-buffer)
(global-set-key "\M- " 'hippie-expand)
(global-set-key "\C-cd" 'diff-buffer-against-disk)
(global-set-key "\C-ca" 'launch-ansi-term)
(global-set-key "\C-c1" 'launch-sbt-terms)
(global-set-key "\C-cm" 'maximize-frame)
(global-set-key "\C-ct" 'toggle-tabs-mode)
(global-set-key "\C-cw" 'toggle-current-window-dedication)
(global-set-key "\C-c " 'toggle-show-trailing-whitespace)
;; Alternate ways to get M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm" 'execute-extended-command)
(global-set-key "\C-cu" 'swap-windows)
(global-set-key "\C-ch" 'python-shell)
(global-set-key "\C-cf" 'auto-revert-tail-mode)
(global-set-key [C-return] 'newline) ; handy when return auto-indents
(when (require 'rainbow-delimiters nil 'noerror)
  (global-set-key "\C-cq" 'rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes and language-specific settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'xscheme)

(setq auto-mode-alist
      (append '(("\\.f95\\'" . fortran-mode)
                ("\\.m\\'" . objc-mode)
                ("\\.mm\\'" . objc-mode)
                ("\\.org\\'" . org-mode)
                ("\\.build\\'" . compilation-minor-mode)
                ("\\.h\\'" . c++-mode)
                ) auto-mode-alist))

;; I want Makefile.foobar to open in makefile-gmake-mode, but not
;; Makefile.py, so we'll put these at the end of the alist
(add-to-list 'auto-mode-alist '("[Mm]akefile.*" . makefile-gmake-mode) t)
(add-to-list 'auto-mode-alist '("[Mm]ake\\..*" . makefile-gmake-mode) t)

;; Use d-mode for D if it exists; otherwise fall back to Java mode
(if (locate-library "d-mode")
    (progn
      (autoload 'd-mode "d-mode" "Major mode for editing D code." t)
      (add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode)))
  (add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . java-mode)))

(when (require 'gas-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.[sS]\\'" . gas-mode)))

;; ansi-term mode
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(eval-after-load 'term
  '(setq term-buffer-maximum-size 100000))

;; auto fill for text, TeX, and LaTeX modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'TeX-mode-hook 'turn-on-auto-fill)

;;Turn on documentation in elisp mode
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
	     (turn-on-eldoc-mode)))

;; Auto-indent in C, Python, etc.
(defun newline-indents ()
  (local-set-key "\C-m" 'newline-and-indent))
(add-hook 'c-mode-common-hook (function newline-indents))
(add-hook 'python-mode-hook (function newline-indents))
(add-hook 'shell-script-mode-hook (function newline-indents))
(add-hook 'fortran-mode-hook (function newline-indents))

;; Make TODO, FIXME, BUG easy to spot
(defun hl-todo-fixme ()
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))))
(add-hook 'c-mode-common-hook 'hl-todo-fixme)
(add-hook 'python-mode-hook 'hl-todo-fixme)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    "Yanks text from kill-ring, wrapped in an org-mode example box"
    (interactive)
    (insert "#+BEGIN_EXAMPLE\n")
    (yank)
    (insert "\n#+END_EXAMPLE\n"))

  (add-hook 'org-mode-hook (lambda ()
                             (local-set-key "\C-ce" 'wrap-org-ex)
                             (local-set-key "\C-c0" 'org-export-as-html)
                             (local-set-key "\C-cy" 'org-yank-code)
                             (make-local-variable 'comment-start)
                             (setq comment-start nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version control and development tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; need perforce for some work projects, so load p4.el if available
(require 'magit nil t)

(when (locate-library "p4")
  (load-library "p4"))

;; ;; CEDET setup
;; ;; load recent version of CEDET if possible
;; (if (file-exists-p "~/.emacs.d/site-lisp/cedet/common/cedet.el")
;;     (load-file "~/.emacs.d/site-lisp/cedet/common/cedet.el")
;;   (require 'cedet nil t))
;; ;; if we managed to load cedet, set things up
;; (when (featurep 'cedet)
;;   (if (require 'cedet-load nil t)
;;       (semantic-load-enable-code-helpers))
;;   (when (fboundp 'global-semantic-idle-summary-mode)
;;     (global-semantic-idle-summary-mode 1))
;;   (global-set-key [C-tab] 'semantic-ia-complete-symbol-menu)
;;   (global-set-key "\C-cj" 'semantic-ia-fast-jump)
;;   (when (file-exists-p "~/.emacs.d/projects.el")
;;     (load-file "~/.emacs.d/projects.el")))

(setq eshell-save-history-on-exit t)
(setq eshell-history-size 512)
(setq eshell-hist-ignoredups t)
(setq eshell-cmpl-cycle-completions nil)