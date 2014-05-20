(add-to-list 'load-path "~/.emacs.d")

;; Marmalade
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" .
                          "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(el-get 'sync)

;; Set my data
(setq user-full-name "Gonzalo Rivero")
(setq user-mail-address "griverorz(at)gmail.com")

;; Smexs
(load "smex")
(smex-initialize)

;; Server
(load "server")
(unless (server-running-p) (server-start))

;; Load init files
(load "~/.emacs.d/init_look.el")
(load "~/.emacs.d/init_latex.el")
(load "~/.emacs.d/init_ess.el")
(load "~/.emacs.d/init_org.el")
(load "~/.emacs.d/init_haskell.el")
;; (load "~/.emacs.d/init_mail.el")
(load "~/.emacs.d/init_python.el")
(load "~/.emacs.d/init_keys.el")

;; Yasnippet
(require 'yasnippet)
(add-to-list 'yas/root-directory "~/.emacs.d/yasnippets/")
(setq yas-snippet-dirs
      "~/.emacs.d/snippets")
;; (yas-load-directory yas-root-directory)
(provide 'setup-snippet)
(yas-global-mode 1)
;; Bound trigger to C-TAB
(define-key yas-minor-mode-map (kbd "C-i") 'yas/expand) 
(define-key yas-minor-mode-map (kbd "TAB") nil)

;; Personal elisp lib dir
(add-to-list 'load-path "~/.emacs.d/")
;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

;; Open files in correct mode and default to text
(add-to-list 'auto-mode-alist '("\\.el\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.jags\\'" . R-mode))
(add-to-list 'auto-mode-alist '("\\.stan\\'" . R-mode))
(add-to-list 'auto-mode-alist '("\\.bugs\\'" . R-mode))
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
(setq major-mode 'text-mode)

;; Lambda mode
(require 'pretty-lambdada)
(pretty-lambda-for-modes)

;; ;; Enable backup files.
(setq make-backup-files t)
(setq delete-old-versions t)
(setq version-control t)
(setq backup-directory-alist (quote ((".*" . "~/Documents/backups_emacs/"))))

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

;; Markdown
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(put 'upcase-region 'disabled nil)

;; Pandoc
(load "pandoc-mode")
(add-hook 'markdown-mode-hook 'turn-on-pandoc)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

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
(set-face-background 'ac-candidate-face "#366060")
(set-face-foreground 'ac-selection-face "#1f1f1f")
(set-face-background 'ac-selection-face "#8cd0d3")
(set-face-foreground 'ac-selection-face "#1f1f1f")

;; Multi-term as replacement for ansi-term
(require 'multi-term)
(autoload 'multi-term "multi-term" nil t)
(autoload 'multi-term-next "multi-term" nil t)

(setq multi-term-program "/bin/bash")   ;; use bash

;; multiterm
(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term) ;; create a new one

;; Combo for dired 
(global-set-key (kbd "s-<f1>")
                (lambda ()
                  (interactive)
                  (dired "~/")))

;; Twittering mode
(require 'twittering-mode)      
(setq twittering-use-master-password t)

 ;; EPA
(require 'epa)
(require 'epa-file)
(setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
(epa-file-name-regexp-update)

;; Abbreviation mode
(setq save-abbrevs t)
(setq abbrev-file-name "~/.emacs/abbrev_defs")
(put 'narrow-to-region 'disabled nil)

;; Kill-ring
(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))

