;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set the load path  
;; add everything under ~/.emacs.d to it
(let* ((my-lisp-dir "~/.emacs.d/")
        (default-directory my-lisp-dir))
  (setq load-path (cons my-lisp-dir load-path))
  (normal-top-level-add-subdirs-to-load-path))

(when (file-exists-p "~/.emacs.d/elpa/package.el")
  (when (load (expand-file-name "~/.emacs.d/elpa/package.el"))
    (package-initialize)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("tromey" . "http://tromey.com/elpa/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar elpa-packages '(
			paredit
			elscreen
			popup 
			pos-tip
			markdown-mode
			fill-column-indicator
			rainbow-mode 
			switch-window 
			auto-complete
			flymake 
			flymake-jslint 
			magit 
			magithub
			highlight-parentheses
			highlight-indentation 
			smart-tab 
			color-theme 
			elein
			popup 
			ac-slime
			clojure-mode 
			clojure-test-mode
			clojurescript-mode 
			undo-tree 
			rainbow-delimiters
			volatile-highlights
			cljdoc))

(dolist (p elpa-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'eldoc nil 'noerror)
(require 'auto-complete nil 'noerror)
(require 'cljdoc nil 'noerror)
(require 'undo-tree nil 'noerror)

(setq url-http-attempt-keepalives nil)
;; (setq inferior-lisp-program "/Users/renatocaliari/.lein/lein repl")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general settings
;;
(menu-bar-mode  t)                       ;; show the menu...
(mouse-avoidance-mode 'jump)             ;; mouse ptr when cursor is too close
(tool-bar-mode -1)                       ;; turn-off toolbar 

(setq cua-enable-cua-keys nil)           ;; only for rectangles
(cua-mode t)

(setq ;; scrolling
  scroll-margin 0                        ;; do smooth scrolling, ...
  scroll-conservatively 100000           ;; ... the defaults ...
  scroll-up-aggressively 0               ;; ... are very ...
  scroll-down-aggressively 0             ;; ... annoying
  scroll-preserve-screen-position t)     ;; preserve screen pos with C-v/M-v 

(setq fringe-mode '(1 . 0))              ;; emacs 22+
(delete-selection-mode 1)                ;; delete the sel with a keyp

;; (setq x-select-enable-clipboard t   ;; copy-paste should work ...
;;       interprogram-paste-function   ;; ...with...
;;       'x-cut-buffer-or-selection-value)	;; ...other X clients

(setq search-highlight t                 ;; highlight when searching... 
  query-replace-highlight t)             ;; ...and replacing
(fset 'yes-or-no-p 'y-or-n-p)            ;; enable y/n answers to yes/no 

(setq completion-ignore-case t           ;; ignore case when completing...
  read-file-name-completion-ignore-case t) ;; ...filenames too

(setq swank-clojure-classpath
        (list "/Users/renatocaliari/Development/clojure/search"))

;; No scratch message
(setq initial-scratch-message nil) 

(setq fci-rule-width 1)
(setq fci-rule-color "yellow")

(setq initial-scratch-message
  ";; scratch buffer created")

(setq-default
 frame-title-format
 '(:eval
   (format "%s@%s:%s"
           (or (file-remote-p default-directory 'user) user-login-name)
           (or (file-remote-p default-directory 'host) system-name)
           (file-name-nondirectory (or (buffer-file-name) default-directory)))))

(put 'narrow-to-region 'disabled nil)    ;; enable...
(put 'erase-buffer 'disabled nil)        ;; ... useful things
(file-name-shadow-mode t)                ;; be smart about filenames in mbuf

(setq inhibit-startup-message t          ;; don't show ...    
  inhibit-startup-echo-area-message t)   ;; ... startup messages
(setq require-final-newline t)           ;; end files with a newline


;; slick-copy: make copy-past a bit more intelligent
;; from: http://www.emacswiki.org/emacs/SlickCopy
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (message "Copied line")
      (list (line-beginning-position)
               (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))


;; key board / input method settings
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")       ; prefer utf-8 for language settings
(set-input-method nil)                   ; no funky input for normal editing;
(setq read-quoted-char-radix 10)         ; use decimal, not octal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the modeline
;; 
(line-number-mode t)                     ;; show line numbers
(column-number-mode t)                   ;; show column numbers
(size-indication-mode t)                 ;; show file size (emacs 22+)

(if (require 'sml-modeline nil 'noerror)    ;; use sml-modeline if available
  (progn 
    (sml-modeline-mode 1)                   ;; show buffer pos in the mode line
    (scroll-bar-mode -1))                   ;; turn off the scrollbar
  (scroll-bar-mode 1)                       ;; otherwise, show a scrollbar...
  (set-scroll-bar-mode 'right))             ;; ... on the right
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the minibuffer
;;
(setq
  enable-recursive-minibuffers nil         ;;  allow mb cmds in the mb
  max-mini-window-height .25             ;;  max 2 lines
  minibuffer-scroll-window nil
  resize-mini-windows nil)

(icomplete-mode t)                       ;; completion in minibuffer
(setq 
  icomplete-prospects-height 1           ;; don't spam my minibuffer
  icomplete-compute-delay 0)             ;; don't wait
(require 'icomplete+ nil 'noerror)       ;; drew adams' extras
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; some handy packages
;;
;; uniquify: unique buffer names
(require 'uniquify) ;; make buffer names more unique
(setq 
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":"
  uniquify-after-kill-buffer-p t
  uniquify-ignore-buffers-re "^\\*")

;; hl-line: highlight the current line
(when (fboundp 'global-hl-line-mode)
  (global-hl-line-mode t)) ;; turn it on for all modes by default

;; http://www.emacswiki.org/cgi-bin/wiki/ShowParenMode
(when (fboundp 'show-paren-mode)
  (show-paren-mode t)
  (setq show-paren-style 'parenthesis))

;; overrride the default function....
(defun emacs-session-filename (SESSION-ID)
  (concat "~/.emacs.d/cache/session." SESSION-ID))

;; bookmarks
(setq bookmark-default-file "~/.emacs.d/data/bookmarks" ;; bookmarks
  bookmark-save-flag 1)                            ;; autosave each change

;; saveplace: save location in file when saving files
(setq save-place-file "~/.emacs.d/cache/saveplace")
(setq-default save-place t)            ;; activate it for all buffers
(require 'saveplace)                   ;; get the package
;;
;; savehist: save some history
(setq savehist-additional-variables    ;; also save...
  '(search ring regexp-search-ring)    ;; ... my search entries
  savehist-autosave-interval 60        ;; save every minute (default: 5 min)
  savehist-file "~/.emacs.d/cache/savehist")   ;; keep my home clean
(savehist-mode t)                      ;; do customization before activation

;; recentf
(require 'recentf)    ;; save recently used files
(setq
  recentf-save-file "~/.emacs.d/cache/recentf"
  recentf-max-saved-items 100     ;; max save 100
  recentf-max-menu-items 15)      ;; max 15 in menu
(recentf-mode t)                  ;; turn it on

;; filecache: http://www.emacswiki.org/cgi-bin/wiki/FileNameCache
(eval-after-load "filecache" 
  '(progn (message "Loading file cache...")
     (file-cache-add-directory "~/")
     (file-cache-add-directory-list '("~/Desktop" "~/Documents"))))

;; backups
(setq make-backup-files t ;; do make backups
  backup-by-copying t     ;; and copy them here
  backup-directory-alist '(("." . "~/.emacs.d/cache/backups")) 
  version-control t
  kept-new-versions 2
  kept-old-versions 5
  delete-old-versions t)

;; time-stamps (better not use those in version-controlled files)
(setq ;; when there's "Time-stamp: <>" in the first 10 lines of the file
  time-stamp-active t        ; do enable time-stamps
  time-stamp-line-limit 10   ; check first 10 buffer lines for Time-stamp: <>
  time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ; date format
(add-hook 'write-file-hooks 'time-stamp) ; update when saving

(setq auto-save-list-file-prefix
  "~/.emacs.d/cache/auto-save-list/.saves-")

(setq ispell-program-name "aspell"
  ispell-extra-args '("--sug-mode=ultra"))

;; tramp, for remote access
(require 'tramp)
;; we need a bit more funky pattern, as tramp will start $SHELL
;; (sudo -s), ie., zsh for root user
(setq shell-prompt-pattern "^[^a-zA-Z].*[#$%>] *")
(setq
  tramp-default-method "ssh"
  tramp-persistency-file-name "~/.emacs.d/cache/tramp")

;; disable cua and transient mark modes in term-char-mode
;; http://www.emacswiki.org/emacs/AnsiTermHints
;; remember: Term-mode remaps C-x to C-c
(defadvice term-char-mode (after term-char-mode-fixes ())
  (set (make-local-variable 'cua-mode) nil)
  (set (make-local-variable 'transient-mark-mode) nil)
  (set (make-local-variable 'global-hl-line-mode) nil)
  (ad-activate 'term-char-mode)
  (term-set-escape-char ?\C-x))

(add-hook 'term-mode-hook 
  (lambda()
    (local-unset-key (kbd "<tab>"))))

;; multi-term
(when (require 'multi-term nil 'noerror)
  (setq multi-term-program "/bin/zsh"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido-mode
;; http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings
(require 'ido) 
(ido-mode 'both) ;; for buffers and files
(setq 
  ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
  ido-ignore-buffers ;; ignore these guys
  '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
     "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
  ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
  ido-case-fold  t                 ; be case-insensitive
  ido-enable-last-directory-history t ; remember last used dirs
  ido-max-work-directory-list 30   ; should be enough
  ido-max-work-file-list      50   ; remember many
  ido-use-filename-at-point nil    ; don't use filename at point (annoying)
  ido-use-url-at-point nil         ; don't use url at point (annoying)
  ido-enable-flex-matching nil     ; don't try to be too smart
  ido-max-prospects 8              ; don't spam my minibuffer
  ido-confirm-unique-completion t) ; wait for RET, even with unique completion

;; when using ido, the confirmation is rather annoying...
 (setq confirm-nonexistent-file-or-buffer nil)

;; increase minibuffer size when ido completion is active
(add-hook 'ido-minibuffer-setup-hook 
  (function
    (lambda ()
      (make-local-variable 'resize-minibuffer-window-max-height)
      (setq resize-minibuffer-window-max-height 1))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet mode
(when (require 'yasnippet nil 'noerror) ;; not: yasnippet-bundle
  (setq yas/root-directory
    '("~/.emacs.d/yas/yasnippet/snippets"
       "~/.emacs.d/yas/custom")) ;; my own snippets
  (mapc 'yas/load-directory yas/root-directory)
  (setq yas/wrap-around-region t)
  (setq yas/prompt-functions
    '(yas/x-prompt yas/ido-prompt))
  (yas/global-mode 1) ;;  make it global
  (add-to-list 'auto-mode-alist '("yas/.*" . snippet-mode)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global keybindings
;;
(global-set-key (kbd "RET")         'newline-and-indent)
(global-set-key (kbd "C-<f4>")      'kill-buffer-and-window)
(global-set-key (kbd "<delete>")    'delete-char)  ; delete == delete    
(global-set-key (kbd "M-g")         'goto-line)    ; M-g  'goto-line

(when (fboundp 'ibuffer)
  (global-set-key (kbd "C-x C-b") 'ibuffer))   ;; ibuffer

;; C-pgup goes to the start, C-pgdw goes to the end
(global-set-key (kbd "<C-prior>")
  (lambda()(interactive)(goto-char(point-min))))
(global-set-key (kbd "<C-next>")
  (lambda()(interactive)(goto-char(point-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; programming
(autoload 'linum-mode "linum" "mode for line numbers" t)
(global-linum-mode t)
(global-set-key (kbd "C-<f5>") 'linum-mode)                 ;; line numbers
(global-set-key (kbd "C-<f6>") 'magit-status)               ;; ...git mode
(global-set-key (kbd "C-<f7>") 'compile)                     ;; compile
(global-set-key (kbd "C-<f8>") 'comment-or-uncomment-region) ;; (un)comment
(global-set-key (kbd "C-<f9>") 'fci-mode)                    ;; column bar indicator

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elscreen
(setq elscreen-prefix-key (kbd "C-c q")
  elscreen-display-tab nil
  elscreen-display-screen-number nil)
(when (require 'elscreen nil 'noerror)
    (global-set-key (kbd "<f12>"    )  'elscreen-create)
    (global-set-key (kbd "<s-f12>"   )  'elscreen-kill)  
    (global-set-key (kbd "<C-M-tab>")  'elscreen-previous) 
    (global-set-key (kbd "<C-tab>"  )  'elscreen-next))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; productivity stuff; f9-f12 
(global-set-key (kbd "<f8>")  'remember)        ;; remember

;; program shortcuts
(global-set-key (kbd "C-c E") ;; .emacs
  (lambda()(interactive)(find-file "~/.emacs.d/init.el")))

;; use super + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings 'super) ;; will be overridden
(global-set-key (kbd "<C-s-left>")  'windmove-left)
(global-set-key (kbd "<C-s-right>") 'windmove-right)
(global-set-key (kbd "<C-s-up>")    'windmove-up)
(global-set-key (kbd "<C-s-down>")  'windmove-down)

;; restore window configuration
(require 'winner)
(setq winner-dont-bind-my-keys t) ;; winner conflicts with org
(global-set-key (kbd "<s-left>")      'winner-undo)
(global-set-key (kbd "<XF86Forward>") 'winner-redo)
(global-set-key (kbd "<s-right>") 'winner-redo)
(global-set-key (kbd "<XF86Back>") 'winner-undo)
(winner-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; safe locals
;; we mark these as 'safe', so emacs22+ won't give us annoying
;; warnings
(setq safe-local-variable-values
      (quote ((auto-recompile . t)
              (folding-mode . t)
              (outline-minor-mode . t)
              auto-recompile outline-minor-mode)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure and Lisp

;; paredit
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook          'turn-on-paredit)
(add-hook 'emacs-lisp-mode-hook       'turn-on-paredit)
(add-hook 'lisp-mode-hook             'turn-on-paredit)
(add-hook 'lisp-interaction-mode-hook 'turn-on-paredit)
(add-hook 'scheme-mode-hook           'turn-on-paredit)

(eval-after-load "paredit"
  '(define-key paredit-mode-map (kbd "C-j") 'eval-last-sexp))

;; show column indicator
(defun turn-on-fci () (fci-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-fci)
(add-hook 'javascript-mode-hook 'turn-on-fci)

;; auto-complete
(global-auto-complete-mode t)
(setq ac-auto-start t) ;; automatically start

;; cljdoc - 'eldoc' for clojure

;; auto indent
(defun turn-on-indent () (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'lisp-mode-hook 'turn-on-indent)
(add-hook 'clojure-mode-hook 'turn-on-indent)

;; eldoc
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round) 

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
            (define-key slime-repl-mode-map
                (read-kbd-macro paredit-backward-delete-key) nil))
          (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;; SLIME
(defun turn-on-slime () (slime-mode t))
(add-hook 'lisp-mode-hook 'turn-on-slime)
(add-hook 'clojure-mode-hook 'turn-on-slime)
(add-hook 'inferior-lisp-mode-hook 'turn-on-slime)

;; take the short answer, y/n is yes/no
(defalias 'yes-or-no-p 'y-or-n-p)	

;; font
(condition-case nil
    (set-default-font "Cousine")
  (error (condition-case nil
	     (set-default-font "Monaco")
	   (error nil))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (deeper-blue))))
