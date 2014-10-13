
;; TeX master file
(setenv "PATH" (concat "/Users/gonzalorivero/.cabal/bin:" (getenv "PATH")))
(setenv "PATH" (concat "/usr/texbin:/usr/local/bin:" (getenv "PATH")))

;; LaTeX path
(setq exec-path (append '("/usr/texbin" "/usr/local/bin") exec-path))
(setq-default TeX-master nil) ; Query for master file.
(custom-set-variables '(LaTeX-command "latex -synctex=1") )

; AUCTeX hyperref autoref customization
(load "tex-site")
(load "reftex-vars.el" t t)
(load "reftex-sel.el" t t)
(load "reftex-ref.el" t t)
(load "reftex.el" t t)
(setq reftex-cite-format 'natbib)

;; RefTeX and default bibliography
(put 'downcase-region 'disabled nil)

(setq reftex-bibpath-environment-variables
'("/Users/gonzalorivero/Library/texmf/bibtex/bib"))

(setq reftex-default-bibliography
'("/Users/gonzalorivero/Documents/bib/ccss.bib"))

;; use skim for PDF
(add-hook 'LaTeX-mode-hook 
	  (lambda ()
	    ;; Enable source-specials for Control-click forward/reverse search.
	    (TeX-PDF-mode 1)
	    (TeX-source-correlate-mode 1)
	    (setq TeX-source-correlate-method 'synctex)
            
	    (setq TeX-view-program-list
		  '(("Skim" 
		     "/Applications/Skim.app/Contents/SharedSupport/displayline -g %n %o %b"))
		  TeX-view-program-selection
		  '((output-pdf "Skim")))))

(setq TeX-PDF-mode t)

;; Auto-raise Emacs on activation (from Skim, usually)
(defun raise-emacs-on-aqua()
(shell-command "osascript -e 'tell application \"Emacs\" to activate' &"))
(add-hook 'server-switch-hook 'raise-emacs-on-aqua)

;; Turn RefTeX on
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; Faces in LaTeX mode
(add-hook 'latex-mode-hook 
          (lambda ()
            (set-face-attribute 'font-latex-sectioning-5-face nil :inherit nil :foreground "#b58900")
            (set-face-attribute 'font-latex-sectioning-0-face nil :height 3)
            (set-face-attribute 'font-latex-sectioning-1-face nil :height 2)
            (set-face-attribute 'font-latex-sectioning-2-face nil :height 1.5)
            (set-face-attribute 'font-latex-sectioning-3-face nil :height 1.2)
            (set-face-attribute 'font-latex-sectioning-4-face nil :height 1.0)))

;; Autoload corrector
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; Autoload fold
(autoload 'TeX-fold-mode "tex-fold" "Minor mode for hiding and revealing macros and environments." t)

