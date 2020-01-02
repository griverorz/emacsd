;; Font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 140
                    :weight 'normal
                    :width 'normal)

;; Theme
(require 'doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
(load-theme 'doom-molokai t)

;; Enable custom neotree theme (all-the-icons must be installed!)
(doom-themes-neotree-config)
;; or for treemacs users
(doom-themes-treemacs-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

;; Modeline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(setq doom-modeline-icon (display-graphic-p))
(setq doom-modeline-unicode-fallback t)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-buffer-file-name-style 'buffer-name)
(setq doom-modeline-project-detection 'project)
(setq doom-modeline-buffer-file-name-style 'relative-to-project)

;; Whether display environment version.
(setq doom-modeline-env-version t)

(defun enable-doom-modeline-icons (_frame)
  (setq doom-modeline-icon t))

(add-hook 'after-make-frame-functions
          #'enable-doom-modeline-icons)


;; Italic
(custom-set-faces
 '(font-lock-comment-face ((t (:foreground "#99968b" :slant italic)))))

;; Color cursor
(set-cursor-color "#ff0000")

;; Menu bar mode
(when (display-graphic-p)
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
      (menu-bar-mode -1))
  ;; if terminal
  (menu-bar-mode -1))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq ns-use-proxy-icon nil)

;; highlight current line
(global-hl-line-mode +1)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; assuming you are using a dark theme
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)


;; Delete seleted text when typing
(delete-selection-mode 1)

;; Truncate long lines
(add-hook 'LaTeX-mode-hook #'visual-line-mode)
(add-hook 'markdown-mode-hook #'visual-line-mode)
(add-hook 'markdown-mode-hook 'imenu-add-menubar-index)
(add-hook 'org-mode-hook #'visual-line-mode)
(setq imenu-auto-rescan t)

;; Autoload flyspell
(eval-after-load "ispell"
  '(when (executable-find ispell-program-name)
   (add-hook 'text-mode-hook 'turn-on-flyspell)))

;; Remove yes-no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Parenthesis
(require 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode 1)
(show-smartparens-global-mode 1)

(sp-pair "\"" nil :unless '(sp-point-after-word-p))
(sp-pair "'" nil :unless '(sp-point-after-word-p))

(custom-set-faces
 '(sp-show-pair-match-face
   ((t (:background "#DCA3A3" :foreground "#2F2F2F"))))
 '(sp-show-pair-match-content-face
   ((t (:background "#2F2F2F")))))

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

(bind-keys
 :map smartparens-mode-map
 ("C-M-a" . sp-beginning-of-sexp)
 ("C-M-e" . sp-end-of-sexp)
      
 ("C-M-f" . sp-forward-sexp)
 ("C-M-b" . sp-backward-sexp)
 
 ("C-M-n" . sp-next-sexp)
 ("C-M-p" . sp-previous-sexp)
 
 ("C-S-f" . sp-forward-symbol)
 ("C-S-b" . sp-backward-symbol)

 ("C-M-t" . sp-transpose-sexp)
 ("C-M-k" . sp-kill-sexp)
 ("C-M-w" . sp-copy-sexp)
 ("C-M-d" . delete-sexp)
 
 ("M-[" . sp-backward-unwrap-sexp)
 ("M-]" . sp-unwrap-sexp)

 ("C-x C-t" . sp-transpose-hybrid-sexp))

;; Cua
(cua-selection-mode t)
(cua-mode t)
(define-key cua-global-keymap "\M-\r" 'cua-set-rectangle-mark)

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;; Show line/column-number in the mode line
(line-number-mode 1)
(column-number-mode 1)

;; Column inidicator
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
;; (setq auto-fill-mode 1)

;; Default fill column 
;; (setq-default fill-column 80)

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
(use-package multiple-cursors) 
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

;; Beacon
(use-package beacon
    :config
    (beacon-mode 1)
    (setq beacon-blink-delay 0.2)
    (setq beacon-blink-duration 0.2)
    (setq beacon-blink-when-point-moves 7)
    (setq beacon-blink-when-window-changes nil)
    (setq beacon-blink-when-window-scrolls nil)
    (setq beacon-push-mark 5)
    (setq beacon-size 15)
)

