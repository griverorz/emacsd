(use-package js2-mode)
(use-package xref-js2)
(use-package indium)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)


