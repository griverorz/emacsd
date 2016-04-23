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
(setq x-select-enable-clipboard t)

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
(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

;; Open files in correct mode and default to text
(add-to-list 'auto-mode-alist '("\\.el\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.jags\\'" . R-mode))
(add-to-list 'auto-mode-alist '("\\.R\\'" . R-mode))
(add-to-list 'auto-mode-alist '("\\.stan\\'" . R-mode))
(add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode))
(add-to-list 'auto-mode-alist '("\\.bugs\\'" . R-mode))
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))
(setq major-mode 'text-mode)

;; Journal
(defun launch-journal () (interactive) (org-capture nil "j"))
(define-key global-map "\C-cd" 'launch-journal)

;; Lambda mode
(require 'pretty-lambdada)
(pretty-lambda-for-modes)

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

;; Comments
(defun comment-or-uncomment-region-or-line ()
  (interactive)
  (if (not mark-active)
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position))
    (if (< (point) (mark))
        (comment-or-uncomment-region (point) (mark))
      (comment-or-uncomment-region (mark) (point)))))
(global-set-key (kbd "M-/") 'comment-or-uncomment-region-or-line)

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

;; Package guide
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
(guide-key-mode 1)  ; Enable guide-key-mode

;; Markdown
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(put 'upcase-region 'disabled nil)

;; Pandoc
(load "pandoc-mode")

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

;; Combo for dired 
(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
(global-set-key (kbd "s-<f1>")
                (lambda ()
                  (interactive)
                  (dired "~/")))

 ;; EPA
(require 'epa)
(require 'epa-file)
(setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
(epa-file-name-regexp-update)

;; Abbreviation mode
(setq save-abbrevs t)
(setq abbrev-file-name "~/.emacs/abbrev_defs")

;; Copy to clipboard
(setq interprogram-cut-function
      (lambda (text &optional push)
	(let* ((process-connection-type nil)
	       (pbproxy (start-process "pbcopy" "pbcopy" "/usr/bin/pbcopy")))
	  (process-send-string pbproxy text)
	  (process-send-eof pbproxy))))

(put 'narrow-to-region 'disabled nil)

(setq-default indent-tabs-mode nil)
