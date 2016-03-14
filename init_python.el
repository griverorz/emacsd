;; elpy
(package-initialize)
(elpy-enable)
(elpy-use-ipython)

(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

(setq py-python-command-args
        '("--gui=wx" "--matplotlib=wx" "--colors=Linux"))
(setq python-version-checked t)
(setq python-python-command "ipython")

(setq py-load-pymacs-p nil)
;; (require 'python-mode)

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(setq python-shell-completion-native-enable nil)

;; Avoid annoying and useless warnings
(with-no-warnings
  (require 'cl))

;; jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(setq jedi:tooltip-method nil)

;; python indent
(setq python-indent-offset 4)
(set-variable 'py-indent-offset 4)
(set-variable 'python-indent-guess-indent-offset nil)
(set-variable 'indent-tabs-mode nil)

;; pymacs
(setq ropemacs-enable-shortcuts 'nil)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location "/opt/virtual_env/")


(defun elpy-eval-region-or-line ()
    "Evaluate the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
	(python-shell-send-string (elpy-shell--region-without-indentation beg end))
        (next-line)))

(add-hook 'elpy-mode-hook
          (lambda ()
	    (local-set-key (kbd "C-c C-l") 'elpy-eval-region-or-line)))

(defvar elpy-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Alphabetical order to make it easier to find free C-c C-X
    ;; bindings in the future. Heh.
    

    (define-key map (kbd "C-M-x")   'python-shell-send-defun)
    (define-key map (kbd "C-c <")   'python-indent-shift-left)
    (define-key map (kbd "C-c >")   'python-indent-shift-right)
    (define-key map (kbd "C-c C-l") 'elpy-eval-region-or-line)
    (define-key map (kbd "C-c C-z") 'elpy-shell-switch-to-shell)
    (define-key map (kbd "C-c C-d") 'elpy-doc)
    (define-key map (kbd "C-c C-f") 'find-file-in-project)
    ;; (define-key map (kbd "C-c C-i") 'yasnippet-expand)
    (define-key map (kbd "C-c C-j") 'idomenu)
    (define-key map (kbd "C-c C-n") 'elpy-flymake-forward-error)
    (define-key map (kbd "C-c C-o") 'elpy-occur-definitions)
    (define-key map (kbd "C-c C-p") 'elpy-flymake-backward-error)
    (define-key map (kbd "C-c C-q") 'elpy-show-defun)
    (define-key map (kbd "C-c C-r") 'elpy-refactor)
    (define-key map (kbd "C-c C-s") 'elpy-rgrep-symbol)
    (define-key map (kbd "C-c C-t") 'elpy-test)
    (define-key map (kbd "C-c C-v") 'elpy-check)
    (define-key map (kbd "C-c C-w") 'elpy-doc-websearch)
    ;; (define-key map (kbd "C-c C-z") 'python-shell-switch-to-shell)
    
    (define-key map (kbd "<C-down>") 'elpy-nav-forward-definition)
    (define-key map (kbd "<C-up>")  'elpy-nav-backward-definition)
    ;; (define-key map (kbd "M-,")     'iedit-mode
    (define-key map (kbd "M-.")     'elpy-goto-definition)
    ;; (define-key map (kbd "M-a")     'elpy-nav-backward-statement)
    ;; (define-key map (kbd "M-e")     'elpy-nav-forward-statement)
    ;; (define-key map (kbd "M-n")     'elpy-nav-forward-definition)
    ;; (define-key map (kbd "M-p")     'elpy-nav-backward-definition)

    map)
  "Key map for the Emacs Lisp Python Environment.")
