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
(setq package-archives '(("gnu"           . "https://elpa.gnu.org/packages/")
                         ("marmalade"     . "https://marmalade-repo.org/packages/")
			 ("melpa-estable" . "https://stable.melpa.org/packages/")
                         ("melpa"         . "https://melpa.org/packages/")))
(package-initialize)

;; Package shell initialize
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; My elisp files
(unless (package-installed-p 'use-package)
 (package-install 'use-package))
(setq use-package-verbose t)
(require 'use-package)
(setq load-prefer-newer t)

;; Enable clipboard
(defun pbcopy ()
  (interactive)
  (let ((deactivate-mark t))
    (call-process-region (point) (mark) "pbcopy")))

(defun pbpaste ()
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

(defun pbcut ()
  (interactive)
  (pbcopy)
  (delete-region (region-beginning) (region-end)))

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
(load "~/.emacs.d/init_js.el")
;; (load "~/.emacs.d/init_haskell.el")
;; (load "~/.emacs.d/init_mail.el")
(load "~/.emacs.d/init_python.el")

;; Yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
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
;; (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . rmd-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
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
  (org-capture nil "j")
  (set-input-method "spanish-prefix"))
(define-key global-map "\C-cd" 'launch-journal)


;; Enable backup files.
(setq make-backup-files t)
(setq delete-old-versions t)
(setq version-control t)
(setq backup-directory-alist
      `((".*" . , "~/.emacs.d/backups/")))
(setq auto-save-file-name-transforms
      `((".*" , "~/.emacs.d/backups/")))

;; Winner mode
(use-package winner
	     :ensure t)

;; End sentence with single space
(setq sentence-end-double-space nil)

;; Binds
(use-package helm-descbinds
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

;; Autocomplete
(add-hook 'after-init-hook 'global-company-mode)
(setq company-global-modes '(not python-mode))
(global-set-key (kbd "C-c (") 'company-complete-common-or-cycle)
(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)

;; Imenu
(use-package imenu-list
  :ensure t
  :bind (("C-c `" . imenu-list-smart-toggle))
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize nil))

;; Multi-term replacement for ansi-term
(require 'multi-term)
(autoload 'multi-term "multi-term" nil t)
(autoload 'multi-term-next "multi-term" nil t)

(setq multi-term-program "/bin/bash")   ;; use bash

;; multiterm
(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term) ;; create a new one

;; Tags
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "Directory: ")
  (eshell-command 
   (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))


(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo find-tag.              
   If buffer is modified, ask about save before running etags."
     (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
        ad-do-it
      (error (and (buffer-modified-p)
                  (not (ding))
                  (y-or-n-p "Buffer is modified, save it? ")
                  (save-buffer))
             (er-refresh-etags extension)
             ad-do-it))))

(defun er-refresh-etags (&optional extension)
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)
  (shell-command (format "etags *.%s" (or extension "el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently          
    (visit-tags-table default-directory nil)))

;; Virtualenvs
(push "~/.virtualenvs/default/bin" exec-path)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Langtools
(setq langtool-language-tool-jar "/Applications/LanguageTool-4.2/languagetool-commandline.jar")
(require 'langtool)

(defun langtool-autoshow-detail-popup (overlays)
  (when (require 'popup nil t)
    ;; Do not interrupt current popup
    (unless (or popup-instances
                ;; suppress popup after type `C-g` .
                (memq last-command '(keyboard-quit)))
      (let ((msg (langtool-details-error-message overlays)))
        (popup-tip msg)))))

(setq langtool-autoshow-message-function
      'langtool-autoshow-detail-popup)

(put 'narrow-to-region 'disabled nil)

(setenv "PATH"
        (concat
         "~/.virtualenvs/default/bin" ":"
         (getenv "PATH")
         ))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (jedi xref-js2 writeroom-mode use-package-el-get switch-window smex smart-mode-line skewer-mode projectile pretty-lambdada pos-tip polymode pandoc-mode org-ref multi-term markdown-mode magit langtool js-doc indium imenu-list ido-ubiquitous guide-key fold-this flycheck expand-region exec-path-from-shell elpy dockerfile-mode docker-compose-mode docker diminish autopair auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "#99968b" :slant italic)))))
