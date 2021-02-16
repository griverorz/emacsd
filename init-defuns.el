(defun insert-random-string (NUM)
  "Insert a random alphanumerics string of length 5.
The possible chars are: A to Z, a to z, 0 to 9.
Call `universal-argument' before for different count.
URL `http://ergoemacs.org/emacs/elisp_insert_random_number_string.html'
sVersion 2018-08-03"
  (interactive "P")
  (let* (($charset "abcdef0123456789")
         ($baseCount (length $charset)))
    (dotimes (_ (if (numberp NUM) (abs NUM) 32))
      (insert (elt $charset (random $baseCount))))))


;; Current date
(defun insert-current-date () (interactive)
       (insert (shell-command-to-string "echo -n $(date +%B,\\ %d\\ %Y)")))


;; Create comments
(defun fill-to-end (char)
  (interactive "cFill Character:")
  (save-excursion
    (end-of-line)
    (while (< (current-column) 80)
      (insert-char char))))
