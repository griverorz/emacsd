;; Haskell mode
(autoload 'haskell-mode "haskell-mode.el" "Major mode for editing Haskell files" t)

(require 'haskell-mode-autoloads)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;; Tidal
(require 'package)
(add-to-list 'package-archives 
   '("marmalade" .
     "http://marmalade-repo.org/packages/"))
(package-initialize)
(setq load-path (cons "~/.emacs.d/tidal/" load-path))
;;(require 'tidal)
(setq tidal-interpreter "/usr/local/bin/ghci")
