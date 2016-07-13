(getenv "PATH")
 (setenv "PATH"
(concat
 "/usr/texbin" ":"
 (getenv "PATH")))

;; Set my data
(setq user-full-name "Gonzalo Rivero"
      user-mail-address "griverorz(at)gmail.com")

;; Marmalade
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/")))
(package-initialize)

;; My elisp files
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(require 'use-package)
(setq load-prefer-newer t)

;; Enable clipboard
(setq select-enable-clipboard t)

;; Smex
(load "smex")
(smex-initialize)

;; Server
(load "server")

;; Load init files
(load "~/.emacs.d/init_look.el")
(load "~/.emacs.d/init_keys.el")
(load "~/.emacs.d/init_tools.el")
(load "~/.emacs.d/init_org.el")
(load "~/.emacs.d/init_latex.el")
(load "~/.emacs.d/init_ess.el")
;; (load "~/.emacs.d/init_haskell.el")
;; (load "~/.emacs.d/init_mail.el")
(load "~/.emacs.d/init_python.el")


;; Yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
;; (yas-load-directory yas-root-directory)
(yas-global-mode 1)
;; Bound trigger to C-TAB
(define-key yas-minor-mode-map (kbd "C-c C-x y") 'yas-insert-snippet) 
(define-key yas-minor-mode-map (kbd "TAB") nil)

;; Personal elisp lib dir
;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

;; Open files in correct mode and default to text
(add-to-list 'auto-mode-alist '("\\.el\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.jags\\'" . jags-mode))
(add-to-list 'auto-mode-alist '("\\.R\\'" . R-mode))
(add-to-list 'auto-mode-alist '("\\.stan\\'" . jags-mode))
(add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode))
(add-to-list 'auto-mode-alist '("\\.bugs\\'" . jags-mode))
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(setq major-mode 'text-mode)

;; Abbreviation mode
(setq save-abbrevs t)
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(setq save-abbrevs 'silently)

;; Journal
(defun launch-journal ()
  (interactive)
  (org-capture nil "j"))
(define-key global-map "\C-cd" 'launch-journal)


;; Enable backup files.
(setq make-backup-files t)
(setq delete-old-versions t)
(setq version-control t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))

;; Winner mode
(use-package winner
	     :ensure t)

;; End sentence with single space
(setq sentence-end-double-space nil)

;; Projectile
(projectile-global-mode)

;; Helm
(use-package helm
	     :ensure t
	     :init
	     (progn
	       (require 'helm-config)
	       (setq helm-candidate-number-limit 100)
	       ;; From https://gist.github.com/antifuchs/9238468
	       (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
		     helm-input-idle-delay 0.01  ; this actually updates things
					; reeeelatively quickly.
		     helm-yas-display-key-on-candidate t
		     helm-quick-update t
		     helm-mode-fuzzy-match t
		     helm-mode-fuzzy-find t
		     helm-M-x-fuzzy-match t
		     helm-ff-skip-boring-files t)
	       (helm-mode))
	     :bind (("C-c h" . helm-mini)
		    ("C-h a" . helm-apropos)
		    ("C-x C-b" . helm-buffers-list)
		    ("C-x b" . helm-buffers-list)
		    ("M-y" . helm-show-kill-ring)
		    ("M-x" . helm-M-x)
		    ("C-x c o" . helm-occur)
		    ("C-x c s" . helm-swoop)
		    ("C-x c y" . helm-yas-complete)
		    ("C-x c Y" . helm-yas-create-snippet-on-region)
		    ("C-x c b" . my/helm-do-grep-book-notes)
		    ("C-x c SPC" . helm-all-mark-rings)))
(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

;; Binds
(use-package helm-descbinds
	     :bind (("C-h b" . helm-descbinds)
		    ("C-h w" . helm-descbinds)))

;; Autocomplete
(byte-recompile-directory "~/.emacs.d/src/auto-complete")
(byte-recompile-directory "~/.emacs.d/src/auto-complete/dict/ess")
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/src/auto-complete/dict")
(ac-config-default)
(ac-set-trigger-key "TAB")
(setq ac-auto-start 5)
(ac-flyspell-workaround)
(setq ac-auto-show-menu 5)

;; Multi-term replacement for ansi-term
(require 'multi-term)
(autoload 'multi-term "multi-term" nil t)
(autoload 'multi-term-next "multi-term" nil t)

(setq multi-term-program "/bin/bash")   ;; use bash

;; multiterm
(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term) ;; create a new one

(put 'narrow-to-region 'disabled nil)
(setq-default indent-tabs-mode nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -synctex=1")
 '(package-selected-packages
   (quote
    (flymake-json magit color-theme-tangotango color-theme-zenburn color-theme-wombat zenburn-theme virtualenvwrapper use-package twittering-mode switch-window smex smart-mode-line-powerline-theme simpleclip python-mode pymacs pycomplete projectile project-root pretty-lambdada pandoc-mode multiple-cursors multi-term material-theme markdown-mode jedi ipython idomenu ido-ubiquitous helm haskell-mode guide-key flymake-python-pyflakes flycheck expand-region ess erc-hipchatify elpy el-get color-theme-solarized autopair auto-virtualenv auto-compile auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "#99968b" :slant italic)))))
