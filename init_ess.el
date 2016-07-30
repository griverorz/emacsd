;; ESS
(add-to-list 'load-path "~/.emacs.d/ess/lisp")

(setq load-path 
      (append '("~/.emacs.d/polymode" "~/.emacs.d/polymode/modes")
	      load-path))

(require 'ess-site)
(autoload 'R "ess-site.el" "ESS" t)
(autoload 'R-mode "ess-site.el" t)
(autoload 'Rd-mode "ess-site.el" "ESS" t)
(ess-toggle-underscore nil)

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
;; (defun ess-buffer-visible-other-frame (buf) nil)
;; (setq ess-display-buffer-reuse-frames t)
;; (setq inferior-ess-own-frame t)
;; (setq inferior-ess-same-window nil)
;; (setq split-window-preferred-function nil) ; discourage horizontal splits
;; (setq pop-up-windows nil)
;; (setq ess-use-tracebug nil)
(define-key compilation-minor-mode-map [(?n)] 'next-error-no-select)
(define-key compilation-minor-mode-map [(?p)] 'previous-error-no-select)

(defun my-ess-start-R ()
  (interactive)
  (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
      (progn
        (delete-other-windows)
        (setq w1 (selected-window))
        (setq w1name (buffer-name))
        (setq w2 (split-window w1 nil t))
        (R)
        (set-window-buffer w2 "*R*")
        (set-window-buffer w1 w1name))))

(defun my-ess-eval ()
  (interactive)
  (my-ess-start-R))

(add-hook 'ess-mode-hook
          '(lambda()
             (local-set-key [(shift return)] 'my-ess-eval)))

(add-hook 'inferior-ess-mode-hook
          '(lambda()
             (local-set-key [C-up] 'comint-previous-input)
             (local-set-key [C-down] 'comint-next-input)))

(add-hook 'Rnw-mode-hook
          '(lambda()
             (local-set-key [(shift return)] 'my-ess-eval)))
(require 'ess-site)

;; Adapted with one minor change from Felipe Salazar at
;; http://www.emacswiki.org/emacs/EmacsSpeaksStatistics

(setq ess-ask-for-ess-directory nil)
(setq ess-local-process-name "R")
(setq ansi-color-for-comint-mode 'filter)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)

;; (setq ess-eval-visibly-p 'no-wait)

;; Create comments
(defun fill-to-end (char)
  (interactive "cFill Character:")
  (save-excursion
    (end-of-line)
    (while (< (current-column) 80)
      (insert-char char))))
