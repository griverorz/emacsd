;; Font
(set-frame-font "Source Code Pro-13")

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)

(load "color-theme-wombat")
(load "color-theme-zenburn")
(load "color-theme-tangotango")

(setq my-color-themes (list
  'color-theme-zenburn
  'color-theme-wombat
  'color-theme-tangotango
))

(defun my-theme-set-default () ; Set the first row
  (interactive)
  (setq theme-current my-color-themes)
  (funcall (car theme-current)))

(defun my-describe-theme () ; Show the current theme
  (interactive)
  (message "%s" (car theme-current)))

;; Set the next theme (fixed by Chris Webber - tanks)
(defun my-theme-cycle ()            
  (interactive)
  (setq theme-current (cdr theme-current))
  (if (null theme-current)
      (setq theme-current my-color-themes))
  (funcall (car theme-current))
  (message "%S" (car theme-current)))

(setq theme-current my-color-themes)
(setq color-theme-is-global nil) ; Initialization
(my-theme-set-default)
(global-set-key [f4] 'my-theme-cycle)

(custom-set-faces
 '(font-lock-comment-face ((t (:foreground "#99968b" :slant italic)))))

(require 'powerline)
(powerline-vim-theme)

;; Color cursor
(set-cursor-color "#ff0000")

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
(global-visual-line-mode 1)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Tweaks from starter kit
(ido-mode t)
(ido-ubiquitous-mode t)						

;; Autload flyspell
(eval-after-load "ispell"
  '(when (executable-find ispell-program-name)
   (add-hook 'text-mode-hook 'turn-on-flyspell)))

;; Remove yes-no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Parenthesis
(show-paren-mode t)
(setq show-paren-style 'expression) ; alternatives are 'parenthesis' and 'mixed'
(load "autopair")
(autopair-global-mode)
(setq autopair-blink t)
(set-face-attribute 'show-paren-match-face nil 
        :weight 'bold :underline nil :overline nil :slant 'normal)
(set-face-foreground 'show-paren-mismatch-face "red")

;; Cua
(cua-selection-mode t)
(cua-mode t)
(define-key cua-global-keymap "\M-\r" 'cua-set-rectangle-mark)

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;; ;; Show line/column-number in the mode line
(line-number-mode 1)
(column-number-mode 1)

;; ;; Turn on auto-fill-mode by default in all major modes
(setq auto-fill-mode 1)

;; Default fill column 
(setq-default fill-column 80)

;; ;; Navigation
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; Switch window
(require 'switch-window)

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

;; Set margins
;; (defun toggle-margin-right ()
;;   (interactive)
;;   (if (eq (cdr (window-margins)) nil)
;;       (set-window-margins nil 0 (- (window-body-width) fill-column))
;;     (set-window-margins nil 0 0)))
