;; Yasnippet
(use-package yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;; Bound trigger to C-TAB
(define-key yas-minor-mode-map (kbd "C-c C-x y") 'yas-insert-snippet) 
(define-key yas-minor-mode-map (kbd "TAB") nil)

;; Recent
(require 'recentf)
(add-to-list 'recentf-exclude
             (file-expand-wildcards
              (expand-file-name "~/journal/*")))

;; Magit
(use-package magit)
(global-set-key (kbd "C-c g") 'magit-status)

;; Avy mode
(use-package avy :ensure t
  :diminish (avy-mode . "")
  :bind (("C-\"" . avy-goto-char)
         ("C-:" . avy-goto-word-1)         
         ("C-'" . avy-goto-line)))

;; Ivy mode
(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  :config
  (ivy-mode 1)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; ;; no regexp by default
  (setq ivy-initial-inputs-alist ())
  ;; Minibuffers
  (setq enable-recursive-minibuffers t)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy))))

(use-package ivy-hydra)
(use-package smex)
(use-package flx)

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-o") 'swiper)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c f") 'find-file-in-project)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "C-c C-o") 'ivy-occur)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-x b") 'counsel-ibuffer)
(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x M-b") 'ibuffer)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(use-package counsel
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

;; Projectile
(use-package projectile
  :ensure t
  :pin melpa
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-require-project-root nil)
  (setq projectile-require-project-files-functions nil)
  (setq projectile-track-known-projects-automatically nil))

(projectile-global-mode)
(counsel-projectile-mode)
(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'ivy)
(setq projectile-use-native-indexing t)

;; Merge command
(setq smerge-command-prefix "C-c v")

;; Auto-fill mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; End sentence with single space
(setq sentence-end-double-space nil)

;; Lambda mode
(defconst lisp--prettify-symbols-alist
  '(("lambda"  . ?λ)))
(defconst python--prettify-symbols-alist
  '(("lambda"  . ?λ)))
(global-prettify-symbols-mode +1)

;; Package guide
(use-package which-key
  :init
  (which-key-mode))

(which-key-setup-minibuffer)
(setq which-key-popup-type 'minibuffer)
(setq which-key-idle-delay 1.)

;; Combo for dired 
(use-package find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
(global-set-key (kbd "s-<f1>")
                (lambda ()
                  (interactive)
                  (dired "~/")))
(setq insert-directory-program (executable-find "gls"))

 ;; EPA
(setq epg-gpg-program  "/usr/local/bin/gpg")
(use-package epa-file :defer t)
(use-package pinentry :defer t)
(setq epa-file-select-keys nil)
(setq epa-pinentry-mode 'loopback)
(pinentry-start)

;; Open with
(use-package openwith :defer t)
(openwith-mode t)
(setq openwith-associations '(("\\.pdf\\'" "open" (file))))

;; Open in osx finder
(use-package reveal-in-osx-finder :defer t)

;; Autoload flyspell
(eval-after-load "ispell"
  '(when (executable-find ispell-program-name)
   (add-hook 'text-mode-hook 'turn-on-flyspell)))

;; Default to Spanish
(setq default-input-method "spanish-prefix")


;; Company
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

;; Use hippie mode
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; expand region
(use-package expand-region :defer t)
(global-set-key (kbd "M-=") 'er/expand-region)
