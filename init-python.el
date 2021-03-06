;; Elpy
(elpy-enable)
(setq elpy-modules (delete 'elpy-module-flymake elpy-modules))
(setq elpy-shell-starting-directory 'current-directory)

(exec-path-from-shell-copy-env "PYTHONPATH")

;; Elpy hooks
(defvar elpy-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Alphabetical order to make it easier to find free C-c C-X
    ;; bindings in the future. Heh.
    
    (define-key map (kbd "C-c C-s") 'helm-swoop)
    (define-key map (kbd "C-M-x")   'python-shell-send-defun)
    (define-key map (kbd "C-c <")   'python-indent-shift-left)
    (define-key map (kbd "C-c >")   'python-indent-shift-right)
    (define-key map (kbd "C-c C-l") 'elpy-eval-region-or-line)
    (define-key map (kbd "C-c C-z") 'elpy-shell-switch-to-shell)
    (define-key map (kbd "C-c C-d") 'elpy-doc)
    (define-key map (kbd "C-c C-f") 'find-file-in-project)
    (define-key map (kbd "C-c C-j") 'idomenu)
    (define-key map (kbd "C-c C-o") 'elpy-occur-definitions)
    (define-key map (kbd "C-c C-q") 'elpy-show-defun)
    (define-key map (kbd "C-c C-r") 'elpy-refactor)
    (define-key map (kbd "C-c C-t") 'elpy-test)
    (define-key map (kbd "C-c C-v") 'elpy-check)
    (define-key map (kbd "C-c C-w") 'elpy-doc-websearch)
    (define-key map (kbd "M-.")     'elpy-goto-definition)
    (define-key map (kbd "M-a")     'elpy-nav-backward-statement)
    (define-key map (kbd "M-e")     'elpy-nav-forward-statement)
    (define-key map (kbd "M-n")     'elpy-nav-forward-definition)
    (define-key map (kbd "M-p")     'elpy-nav-backward-definition)

    map)
  "Key map for the Emacs Lisp Python Environment.")

;; Jedi
(add-hook 'python-mode-hook 'jedi:setup)

;; Python executable
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt --pylab")
(setq elpy-rpc-python-command "python3")
(setq elpy-rpc-backend "jedi")

;; LSP
;; (use-package lsp-python-ms
;;   :ensure t
;;   :init (setq lsp-python-ms-auto-install-server t)
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-python)
;;                           (lsp))))  ; or lsp-deferred

;; Indent
(setq python-indent-offset 4)
(set-variable 'py-indent-offset 4)
(set-variable 'python-indent-guess-indent-offset nil)
(set-variable 'indent-tabs-mode nil)

(setq flycheck-check-syntax-automatically '(mode-enabled save))
(flycheck-add-next-checker 'python-flake8 'python-mypy)


