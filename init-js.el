(use-package js2-mode
  :defer t)
(use-package xref-js2
  :defer t)
(use-package indium
  :defer t)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
;; (define-key js-mode-map (kbd "M-.") nil)


