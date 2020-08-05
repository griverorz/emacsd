;; Yasnippet
(use-package yasnippet :defer t)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;; Recent
(require 'recentf)
(add-to-list 'recentf-exclude
             (file-expand-wildcards
              (expand-file-name "~/journal/*")))

;; Magit
(use-package magit :defer t)

;; Avy mode
(use-package avy :ensure t
  :diminish (avy-mode . "")
  :bind (("C-\"" . avy-goto-char)
         ("C-'" . avy-goto-line)))
  
;; Ivy mode
(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  :config
  (ivy-mode 1)
  ;; (set-face-attribute 'ivy-current-match nil :inherit 'hl-line foo)
  (setq ivy-format-function #'ivy-format-function-arrow)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  (setq ivy-count-format "")
  ;; ;; no regexp by default
  (setq ivy-initial-inputs-alist ())
  ;; Minibuffers
  (setq enable-recursive-minibuffers t)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy))))

(use-package smex)
(use-package flx)

(global-set-key (kbd "M-o") 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "C-c C-o") 'ivy-occur)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-x b") 'counsel-ibuffer)
(global-set-key (kbd "C-x C-b") 'counsel-switch-buffer)
(global-set-key (kbd "C-x M-b") 'ibuffer)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(use-package counsel
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

;; Projectile
(use-package projectile
  :ensure t
  :pin melpa
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-require-project-root nil)
  (setq projectile-require-project-files-functions nil)
  (setq projectile-track-known-projects-automatically nil))

(projectile-global-mode)
(counsel-projectile-mode)
(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'ivy)
(setq projectile-use-native-indexing t)

;; Merge command
(setq smerge-command-prefix "C-c v")

;; Auto-fill mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; Docker mode
(use-package docker
  :ensure t
  :defer t)
(use-package dockerfile-mode
  :ensure t
  :defer t)
(use-package docker-compose-mode
  :ensure t
  :defer t)

;; End sentence with single space
(setq sentence-end-double-space nil)

;; Lambda mode
(defconst lisp--prettify-symbols-alist
  '(("lambda"  . ?λ)))
(defconst python--prettify-symbols-alist
  '(("lambda"  . ?λ)))
(global-prettify-symbols-mode +1)


;; Package guide
(use-package which-key
  :init
  (which-key-mode))

(which-key-setup-minibuffer)
(setq which-key-popup-type 'minibuffer)
(setq which-key-idle-delay 1.)

;; Combo for dired 
(use-package find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
(global-set-key (kbd "s-<f1>")
                (lambda ()
                  (interactive)
                  (dired "~/")))
(setq insert-directory-program (executable-find "gls"))

 ;; EPA
(setq epg-gpg-program  "/usr/local/bin/gpg")
(use-package epa-file :defer t)
(use-package pinentry :defer t)
(setq epa-file-select-keys nil)
(setq epa-pinentry-mode 'loopback)
(pinentry-start)

;; Copy to clipboard
(setq *is-a-mac* (eq system-type 'darwin))
(setq *cygwin* (eq system-type 'cygwin) )
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(defun copy-to-x-clipboard ()
  (interactive)
  (if (region-active-p)
      (progn
        (cond
         ((and (display-graphic-p) x-select-enable-clipboard)
          (x-set-selection 'CLIPBOARD
                           (buffer-substring (region-beginning) (region-end))))
         (t (shell-command-on-region (region-beginning) (region-end)
                                     (cond
                                      (*cygwin* "putclip")
                                      (*is-a-mac* "pbcopy")
                                      (*linux* "xsel -ib")))
            ))
        (message "Yanked region to clipboard!")
        (deactivate-mark))
        (message "No region active; can't yank to clipboard!")))

(defun paste-from-x-clipboard()
  (interactive)
  (cond
   ((and (display-graphic-p) x-select-enable-clipboard)
    (insert (x-selection 'CLIPBOARD)))
   (t (shell-command
       (cond
        (*cygwin* "getclip")
        (*is-a-mac* "pbpaste")
        (t "xsel -ob"))
       1))
   ))


(defun insert-random-string (NUM)
  "Insert a random alphanumerics string of length 5.
The possible chars are: A to Z, a to z, 0 to 9.
Call `universal-argument' before for different count.
URL `http://ergoemacs.org/emacs/elisp_insert_random_number_string.html'
sVersion 2018-08-03"
  (interactive "P")
  (let* (($charset "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
         ($baseCount (length $charset)))
    (dotimes (_ (if (numberp NUM) (abs NUM) 5))
      (insert (elt $charset (random $baseCount))))))


(use-package openwith :defer t)
(openwith-mode t)
(setq openwith-associations '(("\\.pdf\\'" "open" (file))))

;; Open in osx finder
(use-package reveal-in-osx-finder :defer t)

;; Current date
(defun insert-current-date () (interactive)
       (insert (shell-command-to-string "echo -n $(date +%B,\\ %d\\ %Y)")))
