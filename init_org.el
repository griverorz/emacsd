;; Basic configuration
(require 'org)
(setq org-directory "~/Documents/org/")

(setq org-log-done t)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
 
(setq org-log-done t)

;; Tags shortcuts
(setq org-tag-alist '(("@work" . ?w) 
                      ("@research" . ?r)
                      ("@coding" . ?c)
                      ("@email" . ?m)
                      ("@reading" . ?b)
                      ("@computer" . ?l)))

;; Set org files
(setq org-agenda-files (list (concat org-directory "notes.org")
                             (concat org-directory "work.org")))

;; Capture 
(define-key global-map "\C-cc" 'org-capture)
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
	
