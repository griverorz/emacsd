;; ESS
;; (add-to-list 'load-path "~/.emacs.d/ess/lisp")
(unless (getenv "LANG") (setenv "LANG" "en_US.UTF-8"))

(use-package ess-site)
(autoload 'R "ess-site.el" "ESS" t)
(autoload 'R-mode "ess-site.el" t)
(autoload 'Rd-mode "ess-site.el" "ESS" t)
(ess-toggle-underscore nil)
(setq ess-R-argument-suffix "=")
(defalias 'newname 'oldname)
(setq ess-use-flymake nil) ;; disable Flymake
(add-hook 'ess-mode-hook
          (lambda () (flycheck-mode t)))

(use-package ess-site :ensure ess)

;; Smartparents
(add-hook 'ess-post-run-hook (lambda () (smartparens-mode 1)))

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

;; Let you use markdown buffer easily
(setq ess-nuke-trailing-whitespace-p nil)  

;; Company
(setq ess-use-company t)
(setq ess-use-company 'script-only)


;; Tags
(require 'ess-r-xref)
(require 'xref)

;; ;; Lintr
;; ;; 2019-10-05 ess and flymake
;; (setq flycheck-lintr-linters '(
;;     ;; "absolute_paths_linter=NULL"
;;     ;; "assignment_linter=NULL"
;;     "closed_curly_linter=NULL"
;;     ;; "commas_linter=NULL"
;;     ;; "commented_code_linter=NULL"
;;     "infix_spaces_linter=NULL"
;;     ;; "line_length_linter=80"
;;     ;; "no_tab_linter=NULL"
;;     ;; "object_usage_linter=NULL"
;;     "camel_case_linter=NULL"
;;     "snake_case_linter=NULL"
;;     ;; "multiple_dots_linter=NULL"
;;     "object_name_linter=NULL"
;;     ;; "object_length_linter=NULL"
;;     "open_curly_linter=NULL"
;;     ;; "single_quotes_linter=NULL"
;;     ;; "spaces_inside_linter=NULL"
;;     ;; "spaces_left_parentheses_linter=NULL"
;;     ;; "trailing_blank_lines_linter=NULL"
;;     ;; "trailing_whitespace_linter=NULL"
;; ))


