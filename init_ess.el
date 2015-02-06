;; Basic ESS
(autoload 'R-mode "ess-site.el" t)

;; Do not load data or save envir
(setq inferior-R-args "--no-restore-history --no-restore-data --no-save ")

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

(add-to-list
 'display-buffer-alist
 '("*.R" . (display-buffer-reuse-window
	    . ((reusable-frames . t)))))

;; Point to julia
(setq inferior-julia-program-name "/Applications/Julia-0.3.1.app/Contents/Resources/julia/bin/julia")
