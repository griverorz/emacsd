;; Smex
(load "smex")
(smex-initialize)

;; Yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)
;; Bound trigger to C-TAB
(define-key yas-minor-mode-map (kbd "C-c C-x y") 'yas-insert-snippet) 
(define-key yas-minor-mode-map (kbd "TAB") nil)


;; Winner mode
(use-package winner
	     :ensure t)

;; End sentence with single space
(setq sentence-end-double-space nil)

;; Lambda mode
(require 'pretty-lambdada)
(pretty-lambda-for-modes)

;; Helm
(use-package helm
	     :ensure t
	     :init
	     (progn
	       (require 'helm-config)
	       (setq helm-candidate-number-limit 100)
	       ;; From https://gist.github.com/antifuchs/9238468
	       (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
		     helm-input-idle-delay 0.01  ; this actually updates things
					; reeeelatively quickly.
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
		    ("C-x b" . helm-buffers-list)
		    ("C-x C-f" . helm-find-files)		    
		    ("M-y" . helm-show-kill-ring)
		    ("M-x" . helm-M-x)
		    ("C-x c o" . helm-occur)
		    ("C-x c s" . helm-swoop)
		    ("C-x c y" . helm-yas-complete)
		    ("C-x c Y" . helm-yas-create-snippet-on-region)
		    ("C-x c b" . my/helm-do-grep-book-notes)
		    ("C-x c SPC" . helm-all-mark-rings)))
(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

;; Package guide
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
(guide-key-mode 1)  ; Enable guide-key-mode

;; Markdown
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(put 'upcase-region 'disabled nil)

;; Pandoc
(load "pandoc-mode")
(add-hook 'markdown-mode-hook 'turn-on-pandoc)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

;; Combo for dired 
(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
(global-set-key (kbd "s-<f1>")
                (lambda ()
                  (interactive)
                  (dired "~/")))

 ;; EPA
(require 'epa)
(require 'epa-file)
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
          (x-set-selection 'CLIPBOARD (buffer-substring (region-beginning) (region-end))))
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


