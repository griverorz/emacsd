;; ESS
;; (add-to-list 'load-path "~/.emacs.d/ess/lisp")
(unless (getenv "LANG") (setenv "LANG" "en_US.UTF-8"))

(use-package ess-site)
(autoload 'R "ess-site.el" "ESS" t)
(autoload 'R-mode "ess-site.el" t)
(autoload 'Rd-mode "ess-site.el" "ESS" t)
(setq ess-R-argument-suffix "=")
(defalias 'newname 'oldname)
(setq ess-use-flymake nil) ;; disable Flymake
(add-hook 'ess-mode-hook
          (lambda () (flycheck-mode t)))

(use-package ess-site :ensure ess)

;; Do not load data or save envir
(setq inferior-ess-r-program "/usr/local/bin/R") 
(setq inferior-R-args "--no-restore-history --no-restore-data --no-save")

;; Insert pipe in dplyr
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


;; Let you use markdown buffer easily
(setq ess-nuke-trailing-whitespace-p nil)  


;; Company
(setq ess-use-company t)
(setq ess-use-company 'script-only)


;; Tags
(require 'ess-r-xref)
(require 'xref)
