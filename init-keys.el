;; Enable mouse support
(when (display-graphic-p)
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


;; Enable full screen
(global-set-key (kbd "<f5>") 'toggle-frame-fullscreen)

;; Occur 
(global-set-key (kbd "C-c o") 'occur)

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

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; Enables writeroom
(define-key global-map (kbd "C-c M-t") 'writeroom-mode)

(with-eval-after-load 'writeroom-mode
  (define-key writeroom-mode-map (kbd "C-M-<") #'writeroom-decrease-width)
  (define-key writeroom-mode-map
    (kbd "C-M->") #'writeroom-increase-width)
  (define-key writeroom-mode-map (kbd "C-M-=") #'writeroom-adjust-width))

(advice-add 'text-scale-adjust :after
            #'visual-fill-column-adjust)

