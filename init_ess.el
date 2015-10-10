;; ESS
(add-to-list 'load-path "~/.emacs.d/ess/lisp")
(require 'ess-site)
(autoload 'R "ess-site.el" "ESS" t)
(autoload 'R-mode "ess-site.el" t)
(autoload 'Rd-mode "ess-site.el" "ESS" t)
(ess-toggle-underscore nil)

;; Do not load data or save envir
(setq inferior-R-args "--no-restore-history --no-restore-data --no-save ")

;; Do not load data or save envir
(setq inferior-R-args "--no-restore-history --no-restore-data --no-save ")

;; Not to indent comments in R mode
(add-hook 'ess-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline)))


;; Not to indent comments in R mode
(define-key ess-mode-map (kbd "C-c C-x p") "%>%")

;; If you want all help buffers to go into one frame do
(setq ess-help-reuse-window nil)

;; Don't prompt each time you start an interactive R session
(setq ess-ask-for-ess-directory nil) 

;; I don't like if another frame pops up showing the inferior-ess buffer,
;; rather I want unconditionally that the buffer is shown in the current frame.
(defun ess-buffer-visible-other-frame (buf) nil)
(setq ess-display-buffer-reuse-frames t)
(setq inferior-ess-own-frame t)
(setq inferior-ess-same-window nil)
(setq split-window-preferred-function nil) ; discourage horizontal splits
(setq pop-up-windows nil)
;; (setq ess-use-tracebug nil)
(define-key compilation-minor-mode-map [(?n)] 'next-error-no-select)
(define-key compilation-minor-mode-map [(?p)] 'previous-error-no-select)


;; (add-to-list
;;  'display-buffer-alist
;;  '("*.R" . (display-buffer-reuse-window
;; 	    . ((reusable-frames . t)))))

;; Point to julia
(setq inferior-julia-program-name "/Applications/Julia-0.3.1.app/Contents/Resources/julia/bin/julia")
