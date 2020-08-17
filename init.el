;; Tech preferences
(setq load-prefer-newer t)
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

(getenv "PATH")
(setenv "PATH"
	(concat
	 "/usr/texbin" ":"
	 (getenv "PATH")))

;; Set my data
(when (string-equal user-login-name "gonzalorivero")
  (setq user-full-name "Gonzalo Rivero"
        user-mail-address "griverorz(at)gmail.com")
  (setq default-directory "/Users/gonzalorivero/"))

(when (string-equal user-login-name "rivero_g")
  (setq user-full-name "Gonzalo Rivero"
        user-mail-address "gonzalorivero(at)westat.com")
  (setq default-directory "/Users/rivero_g/"))


;; Marmalade
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("melpa-estable" . "https://stable.melpa.org/packages/")))
(package-initialize)


;; Package shell initialize
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-variables '("PATH"))
  (exec-path-from-shell-initialize))

(defun er-byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

;; My elisp files
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(use-package use-package)
(setq load-prefer-newer t)

;; Flycheck diagnostic at point
(use-package flycheck
  :ensure t
  :config
  (flymake-mode nil)
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)  
  (add-hook 'after-init-hook #'global-flycheck-mode))


;; Load init files
(load "~/.emacs.d/init-server.el")
(load "~/.emacs.d/init-look.el")
(load "~/.emacs.d/init-keys.el")
(load "~/.emacs.d/init-tools.el")
(load "~/.emacs.d/init-markdown.el")
(load "~/.emacs.d/init-org.el")
(load "~/.emacs.d/init-latex.el")
(load "~/.emacs.d/init-ess.el")
(load "~/.emacs.d/init-js.el")
(load "~/.emacs.d/init-html.el")
(load "~/.emacs.d/init-haskell.el")
(load "~/.emacs.d/init-mail.el")
(load "~/.emacs.d/init-python.el")

;; Bound trigger to C-TAB
(define-key yas-minor-mode-map (kbd "C-c C-x y") 'yas-insert-snippet) 
(define-key yas-minor-mode-map (kbd "TAB") nil)

;; Open files in correct mode and default to text
(add-to-list 'auto-mode-alist '("\\.el\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.jags\\'" . jags-mode))
(add-to-list 'auto-mode-alist '("\\.R\\'" . R-mode))
(add-to-list 'auto-mode-alist '("\\.stan\\'" . stan-mode))
(add-to-list 'auto-mode-alist '("\\.jags\\'" . jags-mode))
(add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode))
(add-to-list 'auto-mode-alist '("\\.bugs\\'" . jags-mode))
(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . poly-markdown+r-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(setq major-mode 'text-mode)

;; Abbreviation mode
(setq save-abbrevs t)
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(setq save-abbrevs 'silently)

;; Backups
(setq delete-old-versions nil)
;disable backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;disable auto save
(setq auto-save-default nil)
;; locks
(setq create-lockfiles nil)

;; Binds
(use-package ivy-xref)
;; XRef initialization is different in Emacs 27
(if (< emacs-major-version 27)
    ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
    ;; commands other than xref-find-definitions
    ;; (e.g. project-find-regexp):
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
  ;; Emacs 27 only:
  (setq xref-show-definitions-function #'ivy-xref-show-defs))

;; Autocomplete with company
(global-company-mode)

(add-hook
 'eshell-mode-hook
 'remove-company-eshell-hook) ;; Remove company from eshell
(defun remove-company-eshell-hook () (company-mode -1))

(global-set-key (kbd "C-c (") 'company-complete-common-or-cycle)
(company-tng-configure-default)
(setq company-selection-wrap-around t
      company-tooltip-align-annotations t
      company-minimum-prefix-length 2
      company-dabbrev-downcase 0
      company-idle-delay .1
      company-tooltip-limit 7)
(company-quickhelp-mode)
(eval-after-load 'company
  '(define-key company-active-map
     (kbd "C-c h")
     #'company-quickhelp-manual-begin))
(define-key company-active-map
  (kbd "M-h")
  'company-show-doc-buffer)
(setq company-quickhelp-delay nil)

;; Do not use company in text modes
(add-hook 'markdown-mode-hook (lambda () (company-mode -1)) 'append)
(add-hook 'org-mode-hook (lambda () (company-mode -1)) 'append)
(add-hook 'LaTeX-mode-hook (lambda () (company-mode -1)) 'append)

;; Narrow region
(put 'narrow-to-region 'disabled nil)

;; Add custom variables somewhere else
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(put 'set-goal-column 'disabled nil)

;; Default to Spanish
(setq default-input-method "spanish-prefix")

