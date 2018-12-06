;; Basic configuration
(use-package org)
(use-package org-ref)

;; Citation
(define-key org-mode-map (kbd "C-c [") 'org-reftex-citation)
(setq org-ref-show-citation-on-enter nil)
(setq org-ref-show-broken-links t)

;; Document org
(setq org-directory "~/Documents/org/")

;; Export to latex
(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

;; Log
(setq org-log-done t)

;; Change ellipsis
(setq org-ellipsis "⤵")

;; Bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-switchb)
(global-set-key "\C-cB" 'helm-org-agenda-files-headings)

;; Modules
(setq org-expiry-inactive-timestamps t)

;; Tags shortcuts
(setq org-tag-alist '(("@work" . ?w) 
                      ("@home" . ?h) 
                      ("@research" . ?r)
                      ("@errands" . ?e) 
                      ("@write" . ?m)
                      ("@reading" . ?b)))

;; Set org files
(setq org-agenda-files (list (concat org-directory "home.org")
                             (concat org-directory "notes.org")
			                 (concat org-directory "learn.org")
                             (concat org-directory "todo.org")
                             (concat org-directory "work.org")
                             (concat org-directory "research.org")))

(setq org-archive-location (concat org-directory "archive/archived.org::"))

;; Capture 
(setq org-default-notes-file (concat org-directory "notes.org"))

(setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline "~/Documents/org/todo.org" "Tasks")
         "* TODO %?\n  %i\n")
        ("j" "Journal" plain
         (file+olp+datetree "~/Documents/org/journal.gpg")
         "%?\nEntered on %U\n")))


;; Navigation
(setq org-goto-interface 'outline
      org-goto-max-level 10)
(use-package imenu)

;; Indentation in org-mode
(setq org-hide-leading-stars t)
(setq org-startup-truncated nil)
(setq org-startup-indented t)
(setq org-startup-folded 0)
(add-hook 'org-mode-hook 'org-indent-mode)

(bind-key "C-c j" 'org-clock-goto) ;; jump to current task from anywhere
(bind-key "C-c C-w" 'org-refile)
(setq org-cycle-include-plain-lists 'integrate)

;; Refile
(setq org-reverse-note-order t)
(setq org-refile-use-outline-path nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-use-cache nil)
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
(setq org-blank-before-new-entry nil)

;; Change keybindings for timestamps
(global-set-key (kbd "C-c <up>") 'org-timestamp-up)
(global-set-key (kbd "C-c <down>") 'org-timestamp-down-down)
(global-set-key (kbd "C-c <left>") 'org-timestamp-down-day)
(global-set-key (kbd "C-c <right>") 'org-timestamp-up-day)

;; Journal
(defun launch-journal ()
    (interactive)
    (org-capture nil "j")
    (activate-input-method 'spanish-prefix))
(define-key global-map "\C-cd" 'launch-journal)

;; New post
(defun new-post ()
  (interactive)
  (setq md-major-mode (quote markdown-mode))
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (writeroom-mode)    
    (funcall (and md-major-mode))
    (setq buffer-offer-save t)))
(define-key global-map "\C-cp" 'new-post)

;; Org bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; imenu
(eval-after-load "org"
  '(define-key org-mode-map (kbd "C-c `") 'counsel-imenu))
