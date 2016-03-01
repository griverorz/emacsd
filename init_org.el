;; Basic configuration
(require 'org)
(setq org-directory "~/Documents/org/")

;; Log
(setq org-log-done t)

;; Bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Modules
(setq org-expiry-inactive-timestamps t)

;; This makes it easier to add links from outside. 
(defun my/yank-more ()
  (interactive)
  (insert "[[")
  (yank)
  (insert "][more]]"))
(global-set-key (kbd "<f6>") 'my/yank-more)

;; Tags shortcuts
(setq org-tag-alist '(("@work" . ?w) 
                      ("@home" . ?h) 
                      ("@research" . ?r)
                      ("@errands" . ?e) 
                      ("@coding" . ?c)
                      ("@email" . ?m)
                      ("@reading" . ?b)
                      ("@computer" . ?l)))

;; Set org files
(setq org-agenda-files (list (concat org-directory "home.org")
                             (concat org-directory "notes.org")
			     (concat org-directory "learn.org")
                             (concat org-directory "work.org")
                             (concat org-directory "research.org")))

;; Capture 
(setq org-default-notes-file (concat org-directory "notes.org"))

(setq org-capture-templates 
      '(
	("t" "Todo" entry (file+headline (concat org-directory "notes.org") "Tasks") "* TODO %?\n  %i\n")
        ("j" "Journal" plain (file+datetree (concat org-directory "journal.org")) "%?\nEntered on %U\n")))



;; Visualization
(add-hook 'org-mode-hook 
          (lambda ()
            (set-face-attribute 'org-level-1 nil :height 1.4)
            (set-face-attribute 'org-level-2 nil :height 1.2)
            (set-face-attribute 'org-level-3 nil :height 1.1)
            (set-face-attribute 'org-level-4 nil :height 1.1)
            (set-face-attribute 'org-level-5 nil :height 1.1)))


;; Indentation in org-mode
(setq org-startup-indented t)

;; Faces in Org-mode
(add-hook 'org-mode-hook 
          (lambda ()
            (set-face-attribute 'org-level-1 nil :height 1.5)
            (set-face-attribute 'org-level-2 nil :height 1.2)
            (set-face-attribute 'org-level-3 nil :height 1.1)
            (set-face-attribute 'org-level-4 nil :height 1.1)
            (set-face-attribute 'org-level-5 nil :height 1.1)))


;; Navigation
(setq org-goto-interface 'outline
      org-goto-max-level 10)
(require 'imenu)
(setq org-startup-folded nil)
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

;; Journal
(defun launch-journal ()
  (interactive)
  (org-capture nil "j")
  (activate-input-method 'spanish-prefix))
(define-key global-map "\C-cd" 'launch-journal)

(defun new-post ()
  (interactive)
  (setq md-major-mode (quote markdown-mode))
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (center-text-mode)    
    (funcall (and md-major-mode))
    (setq buffer-offer-save t)))
(define-key global-map "\C-cp" 'new-post)
