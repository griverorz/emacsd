;; Tech preferences
(setq load-prefer-newer t)
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

(getenv "PATH")
(setenv "PATH"
	(concat
	 "/usr/texbin" ":"
	 (getenv "PATH")))
(setenv "PATH" (concat "/usr/texbin:/usr/local/bin:" (getenv "PATH")))
(setenv "PATH" "/usr/local/bin:/Library/TeX/texbin/:$PATH" t)


;; Set my data
(when (string-equal user-login-name "gonzalorivero")
  (setq user-full-name "Gonzalo Rivero"
        user-mail-address "griverorz(at)gmail.com")
  (setq default-directory "/Users/gonzalorivero/"))

(when (string-equal user-login-name "grivero")
  (setq user-full-name "Gonzalo Rivero"
        user-mail-address "grivero(at)pewresearch.com")
  (setq default-directory "/Users/grivero/"))


;; Marmalade
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-estable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; Install missing packages if needed
(setq package-selected-packages
      '(swiper
        counsel-projectile
        use-package
        smartparens
        expand-region
        beacon
        smex
        flx
        which-key
        magit
        org-ref
        pinentry
        helm-org-rifle
        openwith
        markdown-mode
        doom-themes
        polymode
        polymode-markdown
        poly-R
        multiple-cursors
        org-superstar
        auctex
        ess
        go-mode
        go-eldoc
        haskell-mode
        pandoc-mode
        emmet-mode
        web-mode
        elpy
        writeroom-mode
        company-quickhelp
        company-go
        web-mode
        js2-mode
        emmet-mode
        exec-path-from-shell
        simple-httpd
        ivy-xref))

(unless (cl-every 'package-installed-p package-selected-packages)
  (message "Missing packages detected, please wait...")
  (package-refresh-contents)
  (package-install-selected-packages))


;; Package shell initialize
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-variables '("PATH"))
  (exec-path-from-shell-initialize))


;; My elisp files
(defun er-byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

(unless (package-installed-p 'use-package)
  (package install 'use-package))
(setq use-package-verbose t)
(use-package use-package)
(setq load-prefer-newer t)


;; Load init files
(load "~/.emacs.d/init-flycheck.el")
(load "~/.emacs.d/init-server.el")
(load "~/.emacs.d/init-look.el")
(load "~/.emacs.d/init-keys.el")
(load "~/.emacs.d/init-tools.el")
(load "~/.emacs.d/init-org.el")
(load "~/.emacs.d/init-latex.el")
(load "~/.emacs.d/init-docker.el")
(load "~/.emacs.d/init-ess.el")
(load "~/.emacs.d/init-js.el")
(load "~/.emacs.d/init-html.el")
(load "~/.emacs.d/init-haskell.el")
(load "~/.emacs.d/init-mail.el")
(load "~/.emacs.d/init-python.el")
(load "~/.emacs.d/init-markdown.el")
(load "~/.emacs.d/init-defuns.el")


;; Open files in correct mode and default to text
(add-to-list 'auto-mode-alist '("\\.el\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.jags\\'" . jags-mode))
(add-to-list 'auto-mode-alist '("\\.R\\'" . R-mode))
(add-to-list 'auto-mode-alist '("\\.stan\\'" . stan-mode))
(add-to-list 'auto-mode-alist '("\\.jags\\'" . jags-mode))
(add-to-list 'auto-mode-alist '("\\.jl\\'" . ess-julia-mode))
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


;; Backups
(setq delete-old-versions nil)
;; Enable backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;; Disable auto save
(setq auto-save-default nil)
;; Disables lock files
(setq create-lockfiles nil)


;; Add custom variables somewhere else
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

