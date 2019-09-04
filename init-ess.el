;; ESS
(add-to-list 'load-path "~/.emacs.d/ess/lisp")
(unless (getenv "LANG") (setenv "LANG" "en_US.UTF-8"))

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/stata"))
(setq inferior-R-program-name "/usr/local/bin/R")
(setq exec-path (append exec-path '("/usr/local/stata")))

(use-package ess-site)
(autoload 'R "ess-site.el" "ESS" t)
(autoload 'R-mode "ess-site.el" t)
(autoload 'Rd-mode "ess-site.el" "ESS" t)
(ess-toggle-underscore nil)
(setq ess-R-argument-suffix "=")
(defalias 'newname 'oldname)

(use-package ess-site :ensure ess)

;; Smartparens
(add-hook 'ess-post-run-hook (lambda () (smartparens-mode 1)))
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


;; Do not load data or save envir
(setq inferior-R-args "--no-restore-history --no-restore-data --no-save")

;; Not to indent comments in R mode
(add-hook 'ess-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline)))

;; ESS syntax highlight  
(setq ess-R-font-lock-keywords 
	  '((ess-R-fl-keyword:keywords . t)
	    (ess-R-fl-keyword:constants . t)
	    (ess-R-fl-keyword:modifiers . t)
	    (ess-R-fl-keyword:fun-defs . t)
	    (ess-R-fl-keyword:assign-ops . t)
	    (ess-fl-keyword:fun-calls . t)
	    (ess-fl-keyword:numbers . t)
	    (ess-fl-keyword:operators . t)
	    (ess-fl-keyword:delimiters . t)
	    (ess-fl-keyword:= . t)
	    (ess-R-fl-keyword:F&T . t)
	    (ess-R-fl-keyword:%op% . t)))

(setq inferior-ess-r-font-lock-keywords 
	  '((ess-S-fl-keyword:prompt . t)
 	    (ess-R-fl-keyword:messages . t)
	    (ess-R-fl-keyword:modifiers . nil)
	    (ess-R-fl-keyword:fun-defs . t)
	    (ess-R-fl-keyword:keywords . nil)
	    (ess-R-fl-keyword:assign-ops . t)
	    (ess-R-fl-keyword:constants . t)
	    (ess-fl-keyword:matrix-labels . t)
	    (ess-fl-keyword:fun-calls . nil)
	    (ess-fl-keyword:numbers . nil)
	    (ess-fl-keyword:operators . nil)
	    (ess-fl-keyword:delimiters . nil)
	    (ess-fl-keyword:= . t)
	    (ess-R-fl-keyword:F&T . nil)))

;; Not to indent comments in R mode
(defun then_R_operator ()
  "R - %>% operator or 'then' pipe operator"
  (interactive)
  (just-one-space 1)
  (insert "%>%")
  (reindent-then-newline-and-indent))
(define-key ess-mode-map (kbd "C-c C-x p") 'then_R_operator)

;; If you want all help buffers to go into one frame do
(setq ess-help-reuse-window nil)

;; Don't prompt each time you start an interactive R session
(setq ess-ask-for-ess-directory nil) 

;; I don't like if another frame pops up showing the inferior-ess buffer,
;; rather I want unconditionally that the buffer is shown in the current frame.
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
             (local-set-key [(super return)] 'my-ess-eval)))

(add-hook 'inferior-ess-mode-hook
          '(lambda()
             (local-set-key [C-up] 'comint-previous-input)
             (local-set-key [C-down] 'comint-next-input)))

(add-hook 'Rnw-mode-hook
          '(lambda()
             (local-set-key [(shift return)] 'my-ess-eval)))

;; Adapted with one minor change from Felipe Salazar at
;; http://www.emacswiki.org/emacs/EmacsSpeaksStatistics
(setq ess-ask-for-ess-directory nil)
(setq ess-local-process-name "R")
(setq ansi-color-for-comint-mode 'filter)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)
(setq ess-use-flymake t)

;; Create comments
(defun fill-to-end (char)
  (interactive "cFill Character:")
  (save-excursion
    (end-of-line)
    (while (< (current-column) 80)
      (insert-char char))))

;; Let you use markdown buffer easily
(setq ess-nuke-trailing-whitespace-p nil)  


;; Fold
(defun rmd-fold-block (span)
  "Fold the contents of the current R block"
  (interactive)
  (and (eq (oref pm/chunkmode :mode) 'r-mode)
       (pm-with-narrowed-to-span nil
                                 (goto-char (point-min))
                                 (fold-this (point) (point-max)))))

(defun rmd-unfold-all-blocks ()
  fold-this-unfold-all)

(defun rmd-fold-all-blocks (arg)
  "Fold all R blocks in an Rmarkdown file"
  (interactive "P")
  (save-restriction
    (widen)
    (save-excursion
      (pm-map-over-spans
       'rmd-fold-block (point-min)
       ;; adjust this point to fold prior regions
       (if arg (point) (point-max))))))

;; Company
(setq ess-use-company t)
(setq ess-use-company 'script-only)
