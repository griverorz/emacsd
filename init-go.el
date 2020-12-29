(add-to-list 'exec-path "/Users/gonzalorivero/go/bin")
(add-hook 'before-save-hook 'gofmt-before-save)

(require 'go-eldoc) ;; Don't need to require, if you install by package.el
(add-hook 'go-mode-hook 'go-eldoc-setup)
