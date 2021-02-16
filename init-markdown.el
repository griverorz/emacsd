;; Truncate long lines
(add-hook 'markdown-mode-hook #'visual-line-mode)
(add-hook 'markdown-mode-hook 'imenu-add-menubar-index)

;; Define markdown citation formats
(defvar markdown-cite-format)
(setq markdown-cite-format
      '(
        (?\C-m . "[@%l]")
        (?p . "[@%l]")
        (?t . "@%l")
        )
      )

;; Wrap reftex-citation with local variables for markdown format
(defun markdown-reftex-citation ()
  (interactive)
  (let ((reftex-cite-format markdown-cite-format)
        (reftex-cite-key-separator "; @"))
    (reftex-citation)))

;; Bind modified reftex-citation to C-c[, without enabling reftex-mode
;; https://www.gnu.org/software/auctex/manual/reftex/Citations-Outside-LaTeX.html#SEC31
(add-hook
 'markdown-mode-hook
 (lambda ()
   (define-key markdown-mode-map "\C-c[" 'markdown-reftex-citation)))

;; Markdown
(setq markdown-open-command "/usr/local/bin/mark")
(use-package reftex :defer t)
(autoload 'markdown-mode
  "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(put 'upcase-region 'disabled nil)
(add-hook 'org-mode-hook 'turn-on-reftex)

(setq reftex-external-file-finders
      '(("bib" . "kpsewhich -format=.bib %f")))


;; Pandoc
(load "pandoc-mode")
(add-hook 'markdown-mode-hook 'pandoc-mode)
(add-hook 'org-mode-hook 'pandoc-mode)
