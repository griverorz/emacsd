;; elpy
(package-initialize)
(elpy-enable)
(elpy-use-ipython)
;;(elpy-clean-modeline)
 
;; (require 'ipython)
;; (setq py-python-command-args '("--matplotlib" "--colors" "LightBG"))
(setq python-version-checked t)
(setq python-shell-interpreter "ipython")
(setq python-python-command "ipython")

(require 'python)
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

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

;; pymacs
(setq ropemacs-enable-shortcuts 'nil)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)




