;;; Boo mode for emacs
;;; Author: Isak Andersson

(defvar boo-tab-width tab-width "Boo tab width (default same value as 'tab-width'")

(defun line-ends-with-colon? ()
  (let ((looking-at-colon nil))
    (save-excursion
      (move-end-of-line 1)
      (unless (= 0 (current-column))
        (skip-chars-backward " ")
        (unless (= 0 (current-column))
          (backward-char))        
        (setq looking-at-colon (= ?: (following-char)))))
    looking-at-colon))

(defun boo-indent-function ()
  (let ((should-indent nil)
        (should-go-deeper nil)
        (previous-indent 0))
    (save-excursion
      (forward-line -1)
      (move-end-of-line 1)
      (unless (= 0 (current-column))
        (skip-chars-backward " ")
        (unless (= 0 (current-column))
          (setq should-indent t))
        (backward-char))
      (when (= (following-char) ?:)
          (setq should-go-deeper t))
      (back-to-indentation)
      (setq previous-indent (current-column)))
    (if should-indent
        (if should-go-deeper
            (indent-to (+ previous-indent boo-tab-width))
          (indent-to previous-indent)))))

(defun boo-indent ()
  (interactive aoe aoe)
  (boo-indent-function))

(defun boo-mark-trailing-sexp ()
  "Marks the sexp that the cursor is currently looking at"
  (back-to-indentation)
  (save-excursion
    (let ((start-col (current-column))
          (keep-looking t))
      (while keep-looking
        (forward-line 1)
        (back-to-indentation)
        (unless (= 0 (current-column))
          (setq keep-looking (> (current-column) start-col)))
        (message (number-to-string (current-column)))))
    (forward-line -1)
    (move-end-of-line 1)
    (set-mark (point))))

(defun boo-mark-inline-sexp ()
  "Marks the sexp that the cursor is currently inside")

(defun boo-mark-sexp ()
  (interactive)
  (if (line-ends-with-colon?)
      (boo-mark-trailing-sexp)
    (boo-mark-inline-sexp)))

(define-derived-mode boo-mode prog-mode "Boo"
  "Major mode for editing Boo source code"
  (setq-local indent-line-function 'boo-indent-function)
  (setq-local comment-start "#"))

(provide 'boo-mode)
