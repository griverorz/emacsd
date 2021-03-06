;; Basic configuration
(use-package org)
(use-package org-ref  :defer t)
(use-package org-crypt :defer t)

;; Document org
(setq org-directory "~/org/")

;; Export to latex
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(setq org-latex-pdf-process (list "latexmk -output-directory=%o -shell-escape -bibtex -f -pdf %f"))

;; Log
(setq org-log-done t)

;; Change ellipsis
(setq org-ellipsis "⤵")

;; Bindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c C-.") 'org-time-stamp)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c C-b") 'counsel-org-agenda-headlines)
(define-key org-mode-map (kbd "C-x t") #'counsel-org-tag)
(define-key org-mode-map (kbd "C-'") nil)

;; Modules
(setq org-expiry-inactive-timestamps t)

;; Tags shortcuts
(setq org-tag-alist '(("overhead" . ?o) 
                      ("home" . ?h) 
                      ("research" . ?r)
                      ("crypt" . ?c)
                      ("errand" . ?e)
                      ("work" . ?p)
                      ("email" . ?w)
                      ("meeting" . ?t)
                      ("coding" . ?d)
                      ("reading" . ?b)))

;; Tags next to text
(setq org-tags-column 0)
(setq org-agenda-tags-column 0)

;; Set org files
(setq org-agenda-files (list (concat org-directory "home.org")
                             (concat org-directory "projects.org")
                             (concat org-directory "management.org")
                             (concat org-directory "work.org")
                             (concat org-directory "research.org")))

;; Location of archive
(setq org-archive-location (concat org-directory "archive/archived.org::"))

;; Capture 
(setq org-default-notes-file (concat org-directory "inbox.org"))

;; Setting org templates
(setq org-capture-templates
       `(
         ("t" "Todo" entry (file "~/org/inbox.org")
          "* TODO %? %^g")
         
         ("m" "Meeting" entry (file "~/org/inbox.org")
          ,(concat "** TODO Meeting about %^{topic}:meeting:\n"
                   "SCHEDULED: %U\n\n"
                   "*Attendees:*\n- [X] Gonzalo Rivero\n- [ ] %^{person}\n\n"
                   "*Notes:*\n%?"))

         ("b" "Beamer" entry (file "~/org/inbox.org")
          ,(concat "* %^{title}\n"
                   ":PROPERTIES:\n"
                   ":EXPORT_LaTeX_CLASS: beamer\n"
                   ":EXPORT_FILE_NAME: %^{filename}\n"
                   ":END:\n"
                   "#+OPTIONS: toc:nil\n"
                   "#+LATEX_HEADER: \\beamertemplatenavigationsymbolsempty\n"
                   "** %?"
                   ))
       
        ("w" "Writing" entry (file+headline "~/org/inbox.org" "Writing")
         ,(concat "* %^{title}\n"
                 ":PROPERTIES:\n"
                 ":AUTHOR: Gonzalo Rivero\n"
                 ":TITLE: %\\1\n"
                 ":CREATED: %U\n"
                 ":END:\n%?")))
        )

;; Navigation
(setq org-goto-interface 'outline
      org-goto-max-level 10)

;; Indentation in org-mode
(setq org-hide-leading-stars t)
(setq org-startup-truncated nil)
(setq org-startup-indented t)
(setq org-startup-folded 1)
(add-hook 'org-mode-hook 'org-indent-mode)

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

;; Encryption
(use-package org
    :bind ("C-c d" . org-decrypt-entry)
    :init (org-crypt-use-before-save-magic)
    :custom
    (org-tags-exclude-from-inheritance (quote ("crypt")))
    (org-crypt-key "37391085395F13E60C8A33D5DD8FBAC75F59750A")
    (auto-save-default nil))

;; Custom list of todo
(setq org-todo-keywords
      '((sequence "TODO(t)" "ONGOING(o)" "|" "DONE(d)" "CANCELLED(c)")))

;; Custom filter to not print habits in agenda
(defun gr-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

;; Custom agenda view
(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header
                  "High-priority unfinished tasks:")))
          (agenda "" ((org-agenda-skip-function
                       '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-span 1)))
          (tags-todo "-PRIORITY=\"A\"" ((org-agenda-skip-function
                                         '(gr-org-skip-subtree-if-habit))
                                        (org-agenda-overriding-header "TODO tasks:")))))))

;; Org bullets
(require 'org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

;; Imenu
(eval-after-load "org"
  '(define-key org-mode-map (kbd "C-c `") 'counsel-imenu))

;; Org babel
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate t)

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (R . t)
     (js . t)
     (haskell . t)
     (python . t)
     (latex . t)
     (ditaa . t)))
  )

;; ;; Disable flymake warning
;; (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

;; Journal
(use-package org-journal
  :ensure t
  :custom
  (org-journal-dir "~/journal/")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-enable-encryption nil)
  (org-journal-encrypt-journal nil)
  (org-journal-enable-agenda-integration t))

(defun org-journal-save-entry-and-exit()
  "Simple convenience function.
  Saves the buffer of the current day's entry and kills the window
  Similar to org-capture like behavior"
  (interactive)
  (save-buffer)
  (kill-buffer-and-window))

(define-key org-journal-mode-map
  (kbd "C-x C-s")
  'org-journal-save-entry-and-exit)

;; Org-rifle
(global-set-key [f8] 'counsel-org-goto-all)
