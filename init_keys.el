;; Enable mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] '(lambda ()
			       (interactive)
			       (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
			       (interactive)
			       (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  )

;; It's all about the project.
(global-set-key (kbd "C-c f") 'find-file-in-project)

;; Occur 
(global-set-key (kbd "C-c o") 'occur)

;; You know, like Readline.
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; expand region
(require 'expand-region)
(global-set-key (kbd "M-=") 'er/expand-region)

;; Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c M-x") 'execute-extended-command)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'esk-cleanup-buffer)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)

;; Jump to a definition in the current file. (Protip: this is awesome.)
(global-set-key (kbd "C-x C-i") 'imenu)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; Helm
(global-set-key (kbd "C-c h") 'helm-mini)

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x C-m") 'shell)

;; If you want to be able to M-x without meta (phones, etc)
(global-set-key (kbd "C-c x") 'execute-extended-command)

;; Help should search more than just commands
(define-key 'help-command "a" 'apropos)

;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c e") 'esk-eval-and-replace)

;; M-S-6 is awkward
(global-set-key (kbd "C-c q") 'join-line)

;; So good!
(global-set-key (kbd "C-c g") 'magit-status)

;; elpy keys
(define-key elpy-mode-map (kbd "M-n") nil)
(define-key elpy-mode-map (kbd "M-p") nil)
(define-key elpy-mode-map (kbd "<C-down>") nil)
(define-key elpy-mode-map (kbd "<C-up>") nil)
(define-key elpy-mode-map (kbd "C-c C-f") 'python-shell-send-defun)
(define-key elpy-mode-map (kbd "C-c C-r") 'elpy-shell-send-region-or-buffer)

;; terminal keys
;;(define-key input-decode-map "\e[1;2D" [S-left])  
;;(define-key input-decode-map "\e[1;5D" [C-S-left])  

;;(define-key input-decode-map "\e[1;2C" [S-right])  
;;(define-key input-decode-map "\e[1;5C" [C-S-right])  

;;(define-key input-decode-map "\e[1;2B" [S-down])  
;;(define-key input-decode-map "\e[1;5B" [C-S-down])  

;;(define-key input-decode-map "\e[1;2A" [S-up])  
;;(define-key input-decode-map "\e[1;5A" [C-S-up])  

;;(define-key input-decode-map "\e[1;2F" [S-end])  
;;(define-key input-decode-map "\e[1;2H" [S-home])

