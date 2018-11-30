;; Yasnippet
(use-package yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;; Magit
(use-package magit)

;; Ivy mode
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-c C-s") 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "C-c M-x") 'counsel-M-x)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; Projectile
(use-package projectile
  :ensure t
  :pin melpa
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(projectile-global-mode)
(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'ivy)

;; Bound trigger to C-TAB
(define-key yas-minor-mode-map (kbd "C-c C-x y") 'yas-insert-snippet) 
(define-key yas-minor-mode-map (kbd "TAB") nil)

;; Merge command
(setq smerge-command-prefix "C-c v")

;; Auto-fill mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; Docker mode
(use-package docker
	     :ensure t)

(use-package dockerfile-mode
	     :ensure t)

(use-package docker-compose-mode
	     :ensure t)

;; End sentence with single space
(setq sentence-end-double-space nil)

;; Lambda mode
(defconst lisp--prettify-symbols-alist
  '(("lambda"  . ?λ)))

(defconst python--prettify-symbols-alist
  '(("lambda"  . ?λ)))

(global-prettify-symbols-mode +1)

;; Helm
(use-package helm
	     :ensure t
	     :init
	     (progn
	       (require 'helm-config)
	       (setq helm-candidate-number-limit 100)
	       ;; From https://gist.github.com/antifuchs/9238468
	       (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
		         helm-input-idle-delay 0.01  ; this actually updates things quickly.
		         helm-yas-display-key-on-candidate t
		         helm-quick-update t
		         helm-mode-fuzzy-match t
		         helm-mode-fuzzy-find t
		         helm-M-x-fuzzy-match t
		         helm-ff-skip-boring-files t)
	       (helm-mode))
	     :bind (
		        ("C-c h" . helm-mini)
		        ("C-h a" . helm-apropos)
		        ("C-x C-b" . helm-buffers-list)
                ("C-x C-f" . helm-find-files)
		        ("C-x b" . helm-buffers-list)
		        ("M-y" . helm-show-kill-ring)
		        ("M-x" . helm-M-x)
		        ("C-x c o" . helm-occur)
                ("C-c s" . helm-projectile-ag)
		        ("C-x c y" . helm-yas-complete)
		        ("C-x c Y" . helm-yas-create-snippet-on-region)
		        ("C-x c SPC" . helm-all-mark-rings)))

(use-package helm-swoop)

(use-package helm-projectile)
(helm-projectile-on) 

;; Package guide
(use-package which-key
  :init
  (which-key-mode))

(which-key-setup-minibuffer)
(setq which-key-popup-type 'minibuffer)
(setq which-key-idle-delay 1.)

;; Markdown
(setq markdown-open-command "/usr/local/bin/mark")
(use-package reftex)
(autoload 'markdown-mode
  "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(put 'upcase-region 'disabled nil)
(setq markdown-hide-urls t)
(add-hook 'org-mode-hook 'turn-on-reftex)

(setq reftex-external-file-finders
      '(("bib" . "kpsewhich -format=.bib %f")))


;; Pandoc
(load "pandoc-mode")
(add-hook 'markdown-mode-hook 'pandoc-mode)
(add-hook 'org-mode-hook 'pandoc-mode)

(add-hook 'markdown-mode-hook
      (lambda ()
        (setq tab-width 2)))


;; Combo for dired 
(use-package find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
(global-set-key (kbd "s-<f1>")
                (lambda ()
                  (interactive)
                  (dired "~/")))

 ;; EPA
(use-package epa)
(use-package epa-file)
(setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
(epa-file-name-regexp-update)

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

(defun my/paste-in-minibuffer ()
  (local-set-key (kbd "M-y") 'paste-from-x-clipboard)
  )


(defun copy-from-clipboard-and-cc-kill-ring ()
  "paste from clipboard and cc the content into kill ring"
  (interactive)
  (let (str)
    (with-temp-buffer
      (paste-from-x-clipboard)
      (setq str (buffer-string)))
    ;; finish the paste
    (insert str)
    ;; cc the content into kill ring at the same time
    (kill-new str)
    ))



;; Langtools
(setq langtool-language-tool-jar
      "/Applications/LanguageTool-4.2/languagetool-commandline.jar")
(use-package langtool)

(defun langtool-autoshow-detail-popup (overlays)
  (when (require 'popup)
    ;; Do not interrupt current popup
    (unless (or popup-instances
                ;; suppress popup after type `C-g` .
                (memq last-command '(keyboard-quit)))
      (let ((msg (langtool-details-error-message overlays)))
        (popup-tip msg)))))

(setq langtool-autoshow-message-function
      'langtool-autoshow-detail-popup)


;; Multi-term replacement for ansi-term
(use-package multi-term)
(autoload 'multi-term "multi-term" nil t)
(autoload 'multi-term-next "multi-term" nil t)

(setq multi-term-program "/bin/bash")   ;; use bash

;; multiterm
(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term) ;; create a new one


;; Tags
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command 
   (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))


(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo find-tag.              
   If buffer is modified, ask about save before running etags."
     (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
        ad-do-it
      (error (and (buffer-modified-p)
                  (not (ding))
                  (y-or-n-p "Buffer is modified, save it? ")
                  (save-buffer))
             (er-refresh-etags extension)
             ad-do-it))))

(defun er-refresh-etags (&optional extension)
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)
  (shell-command (format "etags *.%s" (or extension "el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently          
    (visit-tags-table default-directory nil)))


;; Imenu
(use-package imenu-list
  :ensure t
  :ensure t
  :bind (("C-c `" . imenu-list-smart-toggle))
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize nil))

(defun insert-random-string (NUM)
  "Insert a random alphanumerics string of length 5.
The possible chars are: A to Z, a to z, 0 to 9.
Call `universal-argument' before for different count.
URL `http://ergoemacs.org/emacs/elisp_insert_random_number_string.html'
Version 2018-08-03"
  (interactive "P")
  (let* (($charset "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
         ($baseCount (length $charset)))
    (dotimes (_ (if (numberp NUM) (abs NUM) 5))
      (insert (elt $charset (random $baseCount))))))


(require 'openwith)
(openwith-mode t)
(setq openwith-associations '(("\\.pdf\\'" "Skim.app" (file))))
