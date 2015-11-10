;;; Boo mode for emacs
;;; Author: Isak Andersson

(defvar boo-tab-width tab-width "Boo tab width (default same value as 'tab-width'")

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
  (interactive)
  (boo-indent-function))

(define-derived-mode boo-mode prog-mode "Boo"
  "Major mode for editing Boo source code"
  (setq-local indent-line-function 'boo-indent-function)
  (setq-local comment-start "#"))

(provide 'boo-mode)
