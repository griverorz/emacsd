;; TeX master file
(setenv "PATH" (concat "/Users/gonzalorivero/.cabal/bin:" (getenv "PATH")))
(setenv "PATH" (concat "/usr/texbin:/usr/local/bin:" (getenv "PATH")))

;; LaTeX path
(setq exec-path (append '("/usr/texbin" "/usr/local/bin") exec-path))
(setq-default TeX-master nil) ; Query for master file.

; AUCTeX hyperref autoref customization
(load "tex-site")
(load "reftex-vars.el" t t)
(load "reftex-sel.el" t t)
(load "reftex-ref.el" t t)
(load "reftex.el" t t)
(setq reftex-cite-format 'natbib)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -syntex=1")
 '(TeX-command-list (quote (("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-command nil (latex-mode context-mode ams-tex-mode)) ("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (plain-tex-mode texinfo-mode ams-tex-mode) :help "Run plain TeX") ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX") ("Makeinfo" "makeinfo %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with Info output") ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with HTML output") ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil (ams-tex-mode) :help "Run AMSTeX") ("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt once") ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt until completion") ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX") ("View" "%V" TeX-run-discard-or-function nil t :help "Run Viewer") ("Print" "%p" TeX-run-command t t :help "Print the file") ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command) ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file") ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file") ("Check" "lacheck %s" TeX-run-compile nil (latex-mode) :help "Check LaTeX file for correctness") ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document") ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files") ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files") ("Other" "" TeX-run-command t t :help "Run an arbitrary command") ("Jump to PDF" "%V" TeX-run-discard-or-function nil t :help "Run Viewer")))))

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
