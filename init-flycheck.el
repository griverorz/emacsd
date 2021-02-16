;; Flycheck diagnostic at point
(defun flycheck-display-error-messages-truncated (errors)
  (when (and errors (flycheck-may-use-echo-area-p))
    (let ((messages (seq-map #'flycheck-error-format-message-and-id errors)))
      (message (string-join messages "\n\n") ;; here is the relevant modification
               flycheck-error-message-buffer)
      ;; We cannot rely on `display-message-or-buffer' returning the right
      ;; window. See URL `https://github.com/flycheck/flycheck/issues/1643'.
      (-when-let ((buf (get-buffer flycheck-error-message-buffer)))
        (with-current-buffer buf
          (unless (derived-mode-p 'flycheck-error-message-mode)
            (flycheck-error-message-mode)))))))

;; Uses Flycheck
(use-package flycheck
  :ensure t
  :config
  :init (global-flycheck-mode)
  (flymake-mode nil)
  (setq flycheck-check-syntax-automatically '(save))
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-truncated)
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))
