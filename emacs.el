;; Asher Langton <langton@gmail.com>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic settings and preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local lisp files
(require 'cl) ; framemove.el needs common lisp


;; emacs version dependent stuff
(when (>= emacs-major-version 23)
  ;; load recent version of CEDET if possible. With the next CEDET release,
  ;; maybe I can remove some of this and rely on a version installed by the
  ;; emacs package manager.
;  (when (file-exists-p "~/.emacs.d/site-lisp/cedet/common/cedet.el")
;    (load-file "~/.emacs.d/site-lisp/cedet/common/cedet.el")
;    (semantic-load-enable-code-helpers))

  ;; (when (require 'auto-complete-config nil t)
  ;;   (add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/ac-dict")
  ;;   (ac-config-default))

  ;; Monaco font is nice on OS X; try to use it on Linux too.
  (when (or (eq window-system 'x) (eq window-system 'ns))
    (add-to-list
     'default-frame-alist
     '(font . "-*-Monaco-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1"))))

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
(require 'tramp nil t)
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
      xterm-mouse-mode 1
      gdb-create-source-file-list nil
      gdb-many-windows t
      display-time-day-and-date t
      display-time-default-load-average nil)

(load custom-file 'noerror)

(setq-default tab-width 8
              indent-tabs-mode nil
              save-place t)

;; Some mode-line settings and other preferences
(line-number-mode t)
(column-number-mode t)
(which-func-mode t)
(show-paren-mode 1)
(transient-mark-mode t)
(delete-selection-mode t)
;; Keep menu on OSX, since the bar is there anyway
(unless (eq window-system 'ns)
  (menu-bar-mode -1))
(display-time-mode t)
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

;; use server/emacsclient when possible
(require 'server)
(if (fboundp 'server-running-p)
    (unless (server-running-p)
      (server-start)))

(setq hostname (car (split-string system-name "\\\.")))
(setq frame-title-format
      '((:eval hostname) ": " (:eval (if (buffer-file-name)
                                  (abbreviate-file-name (buffer-file-name))
                                "%b"))))

;; Use <shift>+<arrow> to move between windows
(when (require 'windmove nil t)
  (windmove-default-keybindings))

;; TODO: manage this via ELPA, when possible
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
  (let* ((cb (char-before (point)))
         (matching-text (and cb
                             (char-equal (char-syntax cb) ?\) )
                             (blink-matching-open))))
    (when matching-text (message matching-text))))

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
(set-background-color "gray93")
(set-cursor-color curscolor)
(set-mouse-color "black")
(add-to-list 'default-frame-alist '(background-color . "gray93"))
(add-to-list 'default-frame-alist '(foreground-color . "black"))
(add-to-list 'default-frame-alist `(cursor-color . ,curscolor))
(set-face-attribute 'mode-line nil
                    :box '(:line-width 2 :color "red" :style nil))
(set-face-background 'fringe "gray85")
(set-face-foreground 'font-lock-string-face "chocolate4")
(set-face-foreground 'font-lock-comment-face "forest green")
(set-face-foreground 'font-lock-keyword-face "MidnightBlue")
(set-face-foreground 'font-lock-function-name-face "blue")
(set-face-foreground 'font-lock-warning-face "red")
(when (boundp 'font-lock-preprocessor-face) ;; for older emacs versions
  (set-face-foreground 'font-lock-preprocessor-face "red"))
(when (boundp 'show-paren-match)
  (set-face-background 'show-paren-match "light gray"))
;; override all other syntax highlighting:
(set-face-foreground 'font-lock-type-face "black")
(set-face-foreground 'font-lock-variable-name-face "black")
(set-face-foreground 'font-lock-builtin-face "black")
(set-face-foreground 'font-lock-constant-face "black")
(make-face-bold 'font-lock-function-name-face)

(when (require 'whitespace nil t)
  (setq whitespace-style '(face tabs lines-tail trailing))
  (global-whitespace-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions and key-bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun diff-buffer-against-disk ()
  "Compare current buffer to saved version on disk"
  (interactive)
  (diff-buffer-with-file (current-buffer)))

(defun update-term-name ()
  (rename-buffer (concat "terminal:[" (abbreviate-file-name
                                  default-directory) "]") t))

;; Launch a terminal without being prompted for the shell type
(defun launch-ansi-term ()
  "Launch an ansi-term running /bin/tcsh with no prompts"
  (interactive)
  (ansi-term "/bin/tcsh")
  (update-term-name))

(defadvice term-handle-ansi-terminal-messages (after fix-term-buffer-name)
  (update-term-name))

(ad-activate 'term-handle-ansi-terminal-messages)

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

;; uptime (From David N. Welton's uptime.el)
(unless (boundp 'uptime-time-init)
  (setq uptime-time-init (current-time)))

(defun uptime ()
  "Emacs uptime."
  (interactive)
  (when (boundp 'uptime-time-init)
    (let* ((tm (current-time))
           (diff (list (- (car tm) (car uptime-time-init))
                       (- (cadr tm) (cadr uptime-time-init))))
           (seconds (+ (* (float (car diff)) 65536) (float (cadr diff))))
           (days  (floor (/ seconds 86400)))
           (hours (progn (decf seconds (* days  86400))
                         (floor (/ seconds 3600))))
           (mins  (progn (decf seconds (* hours 3600))
                         (floor (/ seconds 60)))))
      (message (format "up %d days,  %02d:%02d" days hours mins)))))

;; MacOSX - open current file in finder
;; from http://stackoverflow.com/questions/20510333
(defun open-finder ()
(interactive)
  (let ((path (buffer-file-name))
          dir file)
    (when (and (eq window-system 'ns) path)
      (setq dir (file-name-directory path))
      (setq file (file-name-nondirectory path))
      (open-finder-1 dir file))))

(defun open-finder-1 (dir file)
  (let ((script
         (concat
          "tell application \"Finder\"\n"
          " set frontmost to true\n"
          " make new Finder window to (POSIX file \"" dir "\")\n"
          " select file \"" file "\"\n"
          "end tell\n")))
    (start-process "osascript-getinfo" nil "osascript" "-e" script)))

(defun linkify ()
  "Make selected text into a HTML link."
  (interactive)
  (let (url)
    (progn
      (setq url
            (buffer-substring-no-properties (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (insert (concat "<a href=\"" url "\">" url "</a>"))))

;; put the name of the current buffer on the kill ring; useful
;; for putting error log info and test failures into my daily
;; notes file
(defun buffername-to-killring ()
  (interactive)
  (if (buffer-file-name)
      (kill-new (abbreviate-file-name
                 (buffer-file-name)))))

;; same as previous function, except trim:
;; /long/pathname/src/foo to src/foo
;; and
;; ~/long/pathname/src/foo to src/foo
(defun trim-bname-to-killring ()
  (interactive)
  (if (buffer-file-name)
      (let ((bfn (abbreviate-file-name (buffer-file-name))))
        (kill-new (replace-regexp-in-string "^~?/.*/src/" "src/" bfn)))))
;; Work-around to maximize frame on OS X
(defun maximize-frame ()
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 1000 1000))

(defun toggle-show-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

(defun insert-date ()
  "Insert current date as 'dd Month yyyy (day)'."
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end)))
  (insert (format-time-string "%d %B %Y (%A)")))

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
(global-set-key "\C-cm" 'maximize-frame)
(global-set-key "\C-ct" 'toggle-tabs-mode)
(global-set-key "\C-cw" 'toggle-current-window-dedication)
(global-set-key "\C-c " 'toggle-show-trailing-whitespace)
(global-set-key "\C-x9" 'other-frame)
(global-set-key "\C-c0" 'insert-date)
(global-set-key "\C-c1" 'buffername-to-killring)
(global-set-key "\C-c2" 'trim-bname-to-killring)
(global-set-key "\C-c9" 'open-finder)
;; Alternate ways to get M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm" 'execute-extended-command)
(global-set-key "\C-cu" 'swap-windows)
(global-set-key "\C-ch" 'python-shell)
(global-set-key "\C-cf" 'auto-revert-tail-mode)
(global-set-key [C-return] 'newline) ; handy when return auto-indents

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes and language-specific settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'xscheme)

(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (other . "linux")))

(setq auto-mode-alist
      (append '(("\\.f95\\'" . fortran-mode)
                ("\\.m\\'" . objc-mode)
                ("\\.mm\\'" . objc-mode)
                ("\\.org\\'" . org-mode)
                ("\\.build\\'" . compilation-minor-mode)
                ("\\.h\\'" . c++-mode)
                ) auto-mode-alist))

(require 'python)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "" ; add colors here?
      python-shell-prompt-regexp "In \\[[0-9]+\\]: "
      python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
      python-shell-completion-setup-code
      "from IPython.core.completerlib import module_completion"
      python-shell-completion-module-string-code
      "';'.join(module_completion('''%s'''))\n"
      python-shell-completion-string-code
      "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

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

(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 4
                                  tab-width 4
                                  indent-tabs-mode t)))

; from https://github.com/skybert/.../tkj-java.el
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-eclim")
;; (when (require 'eclim nil t)
;;   (global-eclim-mode)

;;   ;; Variables
;;   (setq eclim-auto-save t
;;         eclim-executable "/opt/eclipse/eclim"
;;         eclimd-executable "/opt/eclipse/eclimd"
;;         eclimd-wait-for-process nil
;;         eclimd-default-workspace "~/Code"
;;         eclim-use-yasnippet nil
;;         help-at-pt-display-when-idle t
;;         help-at-pt-timer-delay 0.1
;;         )

;;   ;; Call the help framework with the settings above & activate
;;   ;; eclim-mode
;;   (help-at-pt-set-timer)

;;   ;; Hook eclim up with auto complete mode
;;   (when (require 'auto-complete-config nil t)
;;     (ac-config-default)
;;     (require 'ac-emacs-eclim-source)
;;     (ac-emacs-eclim-config)))

(when (locate-library "markdown-mode")
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(when (require 'gas-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.[sS]\\'" . gas-mode)))

(when (require 'nasm-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.asm\\'" . nasm-mode)))

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
   '(("\\<\\(FIXME\\|TODO\\|BUG\\|XXX\\):" 1 font-lock-warning-face t))))
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
(require 'magit nil t)

;; need perforce for some work projects, so load p4.el if available
;; if p4.el is available, but there's no p4 executable, p4.el interferes
;; with opening files, so avoid this.
(when (and (locate-library "p4")
           (eq 0 (call-process "which" nil nil nil "p4")))
  (load-library "p4"))

(setq eshell-save-history-on-exit t)
(setq eshell-history-size 512)
(setq eshell-hist-ignoredups t)
(setq eshell-cmpl-cycle-completions nil)
