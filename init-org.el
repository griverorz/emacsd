;; Basic configuration
(use-package org)
(use-package org-ref)
(use-package org-journal)

;; Citation
(define-key org-mode-map (kbd "C-c [") 'org-reftex-citation)
(setq org-ref-show-citation-on-enter nil)
(setq org-ref-show-broken-links t)

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
(define-key org-mode-map (kbd "C-c C-b") #'counsel-org-agenda-headlines)
(define-key org-mode-map (kbd "C-c C-c") #'counsel-org-tag)

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
                             (concat org-directory "writing.org")
                             (concat org-directory "projects.org")
                             (concat org-directory "management.org")
                             (concat org-directory "work.org")
                             (concat org-directory "research.org")))

(setq org-archive-location (concat org-directory "archive/archived.org::"))
(setq org-icalendar-combined-agenda-file
      (concat org-directory "org-calendar"))

;; Capture 
(setq org-default-notes-file (concat org-directory "inbox.org"))

(setq org-capture-templates
      '(
        ("t" "Todo" entry (file "~/org/inbox.org")
         "* TODO %? %^g")

        ("M" "Short memo" entry (file "~/org/inbox.org")
         "* Memo: %^{title} %^g \n:PROPERTIES:\n:TO: %^{To}\n:CREATED: %U\n:END:\n%i%?\n")

        ("m" "Meeting" entry (file "~/org/inbox.org")
         "** TODO Meeting about %^{topic} :meeting:\nSCHEDULED: %U\n\n *Attendees:*\n- [X] Gonzalo Rivero\n- [ ] %^{person}\n\n *Notes:*\n%?")

        ("w" "Writing" entry (file+headline "~/org/inbox.org" "Writing")
         "* %^{title}\n:PROPERTIES:\n:AUTHOR: Gonzalo Rivero\n:TITLE: %\\1\n:CREATED: %U\n:END:\n%?")
        ))

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

(bind-key "C-c C-w" 'org-refile)
(setq org-cycle-include-plain-lists 'integrate)

;; CLOCK
;; Set idle time before alerts to 10
(setq org-clock-idle-time 10)
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Do not prompt to resume an active clock, just resume it
(setq org-clock-persist-query-resume nil)

;; Set default column view headings: Task Priority Effort Clock_Summary
(setq org-columns-default-format "%50ITEM(Task) %2PRIORITY %10Effort(Effort){:} %10CLOCKSUM")

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
    (org-crypt-key "1E3A49578FE54AAC231A3248E75FC0192DD79909")
    (auto-save-default nil))

;; Do not inherit
(setq org-tags-exclude-from-inheritance '("work"))

;; Journal
(use-package org-journal
  :ensure t
  :defer t
  :custom
  (org-journal-dir "~/journal/")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-enable-encryption nil)
  (org-journal-encrypt-journal nil))

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

(define-key org-mode-map (kbd "C-'") nil)

;; Org bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; imenu
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
