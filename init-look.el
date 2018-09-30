;; Font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 150
                    :weight 'normal
                    :width 'normal)

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'monokai t)

(custom-set-faces
 '(font-lock-comment-face ((t (:foreground "#99968b" :slant italic)))))

;; Color cursor
(set-cursor-color "#ff0000")

;; Powerline
(setq sml/no-confirm-load-theme t)
(sml/setup)
(setq sml/theme 'dark)

;; Menu bar mode
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (blink-cursor-mode -1))

(setq inhibit-startup-message t
      sentence-end-double-space nil
      shift-select-mode nil
      mouse-yank-at-point t
      uniquify-buffer-name-style 'forward
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      ediff-window-setup-function 'ediff-setup-windows-plain
      oddmuse-directory (concat user-emacs-directory "oddmuse")
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      diff-switches "-u")

;; Activate menu only in GUI
(if (display-graphic-p)
    (progn
      ;; if graphic
      (scroll-bar-mode -1)
      (menu-bar-mode 1))
  ;; if terminal
  (menu-bar-mode -1))
(tool-bar-mode -1)

;; Delete seleted text when typing
(delete-selection-mode 1)

;; Truncate long lines
(add-hook 'LaTeX-mode-hook #'visual-line-mode)
(add-hook 'markdown-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook #'visual-line-mode)

;; Autload flyspell
(eval-after-load "ispell"
  '(when (executable-find ispell-program-name)
   (add-hook 'text-mode-hook 'turn-on-flyspell)))

;; Remove yes-no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Parenthesis
(require 'smartparens-config)
(show-smartparens-global-mode t)
(add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)
(define-key smartparens-mode-map (kbd "C-c M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-c M-b") 'sp-backward-sexp)

(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'lisp-mode-hook #'smartparens-mode)
(add-hook 'LaTeX-mode-hook #'smartparens-mode)
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'jags-mode-hook #'smartparens-mode)
(add-hook 'julia-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
(add-hook 'markdown-mode-hook #'smartparens-mode)
(add-hook 'rmd-mode-hook #'smartparens-mode)
(add-hook 'js2-mode-hook #'smartparens-mode)
(add-hook 'ess-mode-hook #'smartparens-mode)

;; Cua
(cua-selection-mode t)
(cua-mode t)
(define-key cua-global-keymap "\M-\r" 'cua-set-rectangle-mark)

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;; Show line/column-number in the mode line
(line-number-mode 1)
(column-number-mode 1)

(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

;; ;; Turn on auto-fill-mode by default in all major modes
(setq auto-fill-mode 1)

;; Default fill column 
(setq-default fill-column 80)

;; Navigation
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; Disable bells
(setq ring-bell-function 'ignore)

;; Default to utf-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Multicursor
(require 'multiple-cursors)
(global-set-key (kbd "M-n") 'mc/mark-next-like-this)
(global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c M-p") 'mc/mark-all-like-this)


;; iBuffer groups
(setq ibuffer-saved-filter-groups
    '(("home"
       ("emacs-config" (or (filename . ".emacs.d")
                           (filename . "emacs-config")))
       ("Org" (or (mode . org-mode)
                  (filename . "OrgMode")))
       ("ESS" (or (mode . ess-mode)
		  (mode . iESS)))
       ("LaTeX" (mode . latex-mode))
       ("Dired" (mode . dired-mode))
       ("Lisp" (mode . lisp-mode))
       ("Python" (mode . python-mode))
       ("Twitter" (mode . twittering-mode))
       ("Help" (or (name . "\*Help\*")
                   (name . "\*Apropos\*")
                   (name . "\*info\*"))))))

(add-hook 'ibuffer-mode-hook 
          '(lambda ()
	     (ibuffer-auto-mode 1)
             (ibuffer-switch-to-saved-filter-groups "default")))

(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-expert t)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

;; Date and time in status bar
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)

;; Defaut is 4
(setq-default
    indent-tabs-mode nil
    tab-width 4
    tab-stop-list (quote (4 8))
)
