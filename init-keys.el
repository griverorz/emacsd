
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


(global-set-key (kbd "<f5>") 'toggle-frame-fullscreen)

;; It's all about the project.
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c f") 'find-file-in-project)

;; Pop to mark
(bind-key "C-x p" 'pop-to-mark-qcommand)
(setq set-mark-command-repeat-pop t)

;; Occur 
(global-set-key (kbd "C-c o") 'occur)

;; You know, like Readline.
(global-set-key (kbd "C-M-h") 'backward-kill-word)

;; expand region
(use-package expand-region)
(global-set-key (kbd "M-=") 'er/expand-region)

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
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Help should search more than just commands
(define-key 'help-command "a" 'apropos)

;; Apropos
(global-set-key (kbd "C-c M-.") 'xref-find-apropos)

;; M-S-6 is awkward
(global-set-key (kbd "C-c q") 'join-line)

;; So good!
(global-set-key (kbd "C-c g") 'magit-status)

;; Not sure anymore
(define-key input-decode-map "\e[1;2A" [S-up])

;; Let's try with the default commands
(define-key global-map (kbd "C-x \;") 'comment-line)
(define-key global-map (kbd "C-c M-t") 'writeroom-mode)

;; Use hippie
(global-set-key [remap dabbrev-expand] 'hippie-expand)


