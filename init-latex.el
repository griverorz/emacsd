;; TeX master file
(setenv "PATH" (concat "/Users/gonzalorivero/.cabal/bin:" (getenv "PATH")))
(setenv "PATH" (concat "/usr/texbin:/usr/local/bin:" (getenv "PATH")))
(setenv "PATH" "/usr/local/bin:/Library/TeX/texbin/:$PATH" t)

(require 'tex)
(setq TeX-parse-self t); Enable parse on load.
(setq TeX-auto-save t); Enable parse on save.
(setq-default TeX-master nil)
(TeX-global-PDF-mode t)
(setq TeX-PDF-mode t); PDF mode (rather than DVI-mode)
(setq-default TeX-master nil) ; Query for master file.

; AUCTeX hyperref autoref customization
(load "tex-site")
(load "reftex-vars.el" t t)
(load "reftex-sel.el" t t)
(load "reftex-ref.el" t t)
(load "reftex.el" t t)
(setq reftex-cite-format 'natbib)

;; RefTeX and default bibliography
(put 'downcase-region 'disabled nil)

;; use skim for PDF
(add-hook 'LaTeX-mode-hook 
	  (lambda ()
	    ;; Enable source-specials for Control-click forward/reverse search.
	    (TeX-PDF-mode 1)
	    (TeX-source-correlate-mode 1)
	    (setq TeX-source-correlate-method 'synctex)

	    (setq TeX-view-program-list
		  '(("Skim" 
		     "open -a Skim.app %o"))
		  TeX-view-program-selection
		  '((output-pdf "Skim")))))
(setq TeX-PDF-mode t)

;; Turn RefTeX on
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; Autoload corrector
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; Autoload fold
(autoload 'TeX-fold-mode
  "tex-fold"
  "Minor mode for hiding and revealing macros and environments."
  t)

(add-hook 'TeX-mode-hook
          (lambda ()
            (setq TeX-command-extra-options "-shell-escape")
            )
          )

;; set special font highlighting for \cite* commands
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (font-lock-add-keywords nil  '(("\\(\\\\citep\\)\\s-*{" 1 font-lock-keyword-face t)))
            (font-lock-add-keywords nil  '(("\\(\\\\citet\\)\\s-*{" 1 font-lock-keyword-face t)))
            (font-latex-add-keywords '(("citep" "*[[{")) 'reference)
            (font-latex-add-keywords '(("citet" "*[[{")) 'reference)
            ))
