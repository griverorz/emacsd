;; Haskell mode
;; (autoload 'haskell-mode "haskell-mode.el" "Major mode for editing Haskell files" t)

;; (require 'haskell-mode-autoloads)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;; Tidal
;; (require 'package)
;; (add-to-list 'package-archives 
;;    '("marmalade" .
;;      "http://marmalade-repo.org/packages/"))
;; (package-initialize)
;; (setq load-path (cons "~/.emacs.d/tidal/" load-path))

;; (custom-set-variables
;;  '(haskell-process-suggest-remove-import-lines t)
;;  '(haskell-process-auto-import-loaded-modules t)
;;  '(haskell-process-log t))
;; (eval-after-load 'haskell-mode '(progn
;; 				  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
;; 				  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;; 				  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
;; 				  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
;; 				  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
;; 				  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
;; 				  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
;; (eval-after-load 'haskell-cabal '(progn
;; 				   (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;; 				   (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;; 				   (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;; 				   (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

;; (custom-set-variables '(haskell-process-type 'cabal-repl))
