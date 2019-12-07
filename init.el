(getenv "PATH")
(setenv "PATH"
	(concat
	 "/usr/texbin" ":"
	 (getenv "PATH")))

;; Set my data
(setq user-full-name "Gonzalo Rivero"
      user-mail-address "griverorz(at)gmail.com")

(setq default-directory "/Users/gonzalorivero/")

;; Marmalade
(require 'package)
(setq package-archives '(("gnu"           . "https://elpa.gnu.org/packages/")
                         ("marmalade"     . "https://marmalade-repo.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
			             ("melpa-estable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; Package shell initialize
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-variables '("PATH"))
  (exec-path-from-shell-initialize))

;; My elisp files
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(use-package use-package)
(setq load-prefer-newer t)
 
;; Load init files
(load "~/.emacs.d/init-server.el")
(load "~/.emacs.d/init-look.el")
(load "~/.emacs.d/init-keys.el")
(load "~/.emacs.d/init-tools.el")
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
(setq delete-old-versions t)
;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

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

(add-hook 'eshell-mode-hook 'remove-company-eshell-hook) ;; Remove company from eshell
(defun remove-company-eshell-hook () (company-mode -1))

(global-set-key (kbd "C-c (") 'company-complete-common-or-cycle)
(company-tng-configure-default)
(setq company-selection-wrap-around t
      company-tooltip-align-annotations t
      company-minimum-prefix-length 2
      company-dabbrev-downcase 0
      company-idle-delay 0
      company-tooltip-limit 7)
(company-quickhelp-mode)
(eval-after-load 'company
  '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))
(define-key company-active-map (kbd "M-h") 'company-show-doc-buffer)
(setq company-quickhelp-delay nil)

;; Do not use company in text modes
(add-hook 'markdown-mode-hook (lambda () (company-mode -1)) 'append)
(add-hook 'org-mode-hook (lambda () (company-mode -1)) 'append)
(add-hook 'LaTeX-mode-hook (lambda () (company-mode -1)) 'append)

;; Define markdown citation formats
(defvar markdown-cite-format)
(setq markdown-cite-format
      '(
        (?\C-m . "[@%l]")
        (?p . "[@%l]")
        (?t . "@%l")
        )
      )

;; wrap reftex-citation with local variables for markdown format
(defun markdown-reftex-citation ()
  (interactive)
  (let ((reftex-cite-format markdown-cite-format)
        (reftex-cite-key-separator "; @"))
    (reftex-citation)))

;; bind modified reftex-citation to C-c[, without enabling reftex-mode
;; https://www.gnu.org/software/auctex/manual/reftex/Citations-Outside-LaTeX.html#SEC31
(add-hook
 'markdown-mode-hook
 (lambda ()
   (define-key markdown-mode-map "\C-c[" 'markdown-reftex-citation)))

;; Diminish
(use-package diminish)
(diminish 'ivy-mode)
(diminish 'projectile-mode)
;; (diminish 'helm-mode)
(diminish 'smartparens-mode)
(diminish 'auto-revert-mode)
(diminish 'reftex-mode)
(diminish 'markdown-mode)
(diminish 'pandoc-mode)
(diminish 'which-key-mode)

;; Narrow region
(put 'narrow-to-region 'disabled nil)

;; Add custom variables somewhere else
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(put 'set-goal-column 'disabled nil)
