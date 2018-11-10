;; ESS
(add-to-list 'load-path "~/.emacs.d/ess/lisp")
(unless (getenv "LANG") (setenv "LANG" "en_US.UTF-8"))

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/stata"))
(setq exec-path (append exec-path '("/usr/local/stata")))

(use-package ess-site)
(autoload 'R "ess-site.el" "ESS" t)
(autoload 'R-mode "ess-site.el" t)
(autoload 'Rd-mode "ess-site.el" "ESS" t)
(ess-toggle-underscore nil)
(setq ess-R-argument-suffix "=")

;; Set autocomplete using company
(setq ess-use-company 'script-only)

;; Smartparens
(add-hook 'ess-R-post-run-hook (lambda () (smartparens-mode 1)))

;;; ESS
(defun my-ess-hook ()
  ;; ensure company-R-library is in ESS backends
  (make-local-variable 'company-backends)
  (cl-delete-if (lambda (x) (and (eq (car-safe x) 'company-R-args))) company-backends)
  (push (list 'company-R-args 'company-R-objects 'company-R-library :separate)
        company-backends))

(add-hook 'ess-mode-hook 'my-ess-hook)

;; Do not load data or save envir
(setq inferior-R-args "--no-restore-history --no-restore-data --no-save ")

;; Not to indent comments in R mode
(add-hook 'ess-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline)))


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

;; Create comments
(defun fill-to-end (char)
  (interactive "cFill Character:")
  (save-excursion
    (end-of-line)
    (while (< (current-column) 80)
      (insert-char char))))

;; Polymode
;; Just an Emacs personal dir containing polymode packages etc.
(setq MY-EMACS  "~/.emacs.d/") 

(defun my-emacs  (subfolder)
  "Get path to personal dir + subfolder"
  (concat (expand-file-name MY-EMACS) "/" subfolder))

;; ESS Markdown
(defun rmd-mode ()
  "ESS Markdown mode for rmd files"
  (interactive)
  (setq load-path 
        (append (list (my-emacs "polymode/") 
                      (my-emacs "polymode/modes/"))
                load-path))
  (require 'poly-R)
  (require 'poly-markdown)
  (poly-markdown+r-mode))

;; Let you use markdown buffer easily
(setq ess-nuke-trailing-whitespace-p nil)  

(defun rmd-fold-block ()
  "Fold the contents of the current R block, in an Rmarkdown file (can be undone
   with fold-this-unfold-at-point)"
  (interactive)
  (and (eq (oref pm/chunkmode :mode) 'r-mode)
       (pm-with-narrowed-to-span nil
         (goto-char (point-min))
         (forward-line)
         (fold-this (point) (point-max)))))

(defun rmd-fold-all-blocks (arg)
  "Fold all R blocks in an Rmarkdown file (can be undone with
   fold-this-unfold-all)"
  ;; Interactive, with a prefix argument
  (interactive "P")
  (save-restriction
    (widen)
    (save-excursion
      (pm-map-over-spans
       'rmd-fold-block (point-min)
       ;; adjust this point to fold prior regions
       (if arg (point) (point-max))))))
