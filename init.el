(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives 
	     '("marmalade" .
	       "http://marmalade-repo.org/packages/"))

(setq url-http-attempt-keepalives nil)
(setq inferior-lisp-program "/Users/renatocaliari/.lein/lein repl")

(setq package-list
      '(paredit popup pos-tip
	       rainbow-mode escreen switch-window auto-complete fuzzy-match
	       icicles magit magithub highlight-parentheses
	       highlight-indentation smart-tab color-theme elein
	       popup ac-slime clojure-mode clojure-test-mode
	       clojurescript-mode undo-tree rainbow-delimiters
	       volatile-highlights cljdoc))

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available 
(when (not package-archive-contents)
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))
	
;; Remove unused UI elements
(tool-bar-mode 0)
(menu-bar-mode 1)
(scroll-bar-mode 0)
(setq inhibit-startup-message t)

;; shhht, give me some time to think, don't blink
(blink-cursor-mode 0)

;; No scratch message
(setq initial-scratch-message nil) 

;; show matching parens
(show-paren-mode 1)

;; always show column numbers
(column-number-mode 1)

;; improved navigation in 'switch-to-buffer' and 'find-file'. Potential conflict with icicle (icy-mode)
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)

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
	
;; auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)
(setq ac-auto-start t)                  ;automatically start

;; cljdoc - 'eldoc' for clojure
(require 'cljdoc)

;; auto indent
(defun turn-on-indent () (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'lisp-mode-hook 'turn-on-indent)
(add-hook 'clojure-mode-hook 'turn-on-indent)

(global-undo-tree-mode)

;; improved auto-complete for emacs commands
(icicle-mode 1)

;; eldoc
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(require 'eldoc) ; if not already loaded
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

(setq backup-directory-alist (list (cons ".*" (expand-file-name "~/.emacsbackup/"))))
(setq x-select-enable-clipboard t)
(global-font-lock-mode 1) ;; Enable syntax highlighting when editing code.
(setq current-language-environment "UTF-8")

(define-key emacs-lisp-mode-map
  (kbd "M-.") 'find-function-at-point)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (deeper-blue)))
 '(icicle-TAB-completion-methods (quote (fuzzy basic vanilla))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
