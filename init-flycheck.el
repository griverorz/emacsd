;; Flycheck diagnostic at point
(use-package flycheck
  :ensure t
  :config
  :init (global-flycheck-mode)
  (flymake-mode nil)
  (setq flycheck-check-syntax-automatically '(save)))
