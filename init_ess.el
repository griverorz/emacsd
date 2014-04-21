;; Basic ESS
(require 'ess-site)
(require 'ess-eldoc)
(ess-toggle-underscore nil)

;; Not to indent comments in R mode
(add-hook 'ess-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline)))

;;;;; If you want all help buffers to go into one frame do
(setq ess-help-own-frame 'one)
(setq ess-help-reuse-window t)
;; ESS will not print the evaluated comands, also speeds up the evaluation 
;; (setq ess-eval-visibly nil)

;; Don't prompt each time you start an interactive R session
(setq ess-ask-for-ess-directory nil) 

;; Speedbar
;; (speedbar-add-supported-extension ".R")



;; I don't like if another frame pops up showing the inferior-ess buffer,
;; rather I want unconditionally that the buffer is shown in the current frame.
(defun ess-buffer-visible-other-frame (buf) nil)
(setq ess-display-buffer-reuse-frames nil)
(setq inferior-ess-own-frame t)
(setq inferior-ess-same-window nil)
(setq split-window-preferred-function nil) ; discourage horizontal splits
(setq pop-up-windows nil)
(setq-default display-buffer-reuse-frames nil)
