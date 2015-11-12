;;; Boo mode for emacs
;;; Author: Isak Andersson

(defvar boo-tab-width tab-width "Boo tab width (default same value as 'tab-width'")

(defun boo-deindent ()
  (back-to-indentation)
  (unless (= 0 (current-column))
    (backward-char)
    (if (looking-at-p "\t")
        (delete-char 1)
      (progn
        (delete-char 1)
        (delete-backward-char (1- boo-tab-width))))))

(defun line-is-empty? ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

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
  (interactive)
  (boo-indent-function))

(defun boo-mark-trailing-block ()
  "Marks the block that the cursor is currently looking at"
  (back-to-indentation)
  (save-excursion
    (let ((start-col (current-column))
          (keep-looking t))
      (if (= 0 start-col)
          (while keep-looking
            (forward-line 1)
            (setq keep-looking (not (eobp)))
            (when keep-looking
              (back-to-indentation)
              (when (= 0 (current-column))
                (setq keep-looking (not (line-ends-with-colon?))))))
        (while keep-looking
          (forward-line 1)
          (setq keep-looking (not (eobp)))
          (when keep-looking
            (back-to-indentation)
            (unless (line-is-empty?)
              (setq keep-looking (> (current-column) start-col)))))))
    (unless (eobp)
      (forward-line -1)
      (move-end-of-line 1))
    (set-mark (point))))

(defun boo-mark-inline-block ()
  "Marks the block that the cursor is currently inside"
  (message "NYI: boo-mark-inline-block"))

(defun boo-mark-block ()
  (interactive)
  (if (line-ends-with-colon?)
      (boo-mark-trailing-block)
    (boo-mark-inline-block)))

(defun boo-looking-at-control-flow ()
  (or (looking-at-p "if")
      (looking-at-p "unless")
      (looking-at-p "while")))

(defun boo-single-line->multi-line ()
  (move-end-of-line 1)
  (delete-char (- (skip-chars-backward " ")))
  (unless (or (search-backward "if" (save-excursion (back-to-indentation) (point)) t)
              (search-backward "unless" (save-excursion (back-to-indentation) (point)) t)
              (search-backward "while" (save-excursion (back-to-indentation) (point)) t))
    (error "No control flow keyword found!"))
  (kill-line)
  (back-to-indentation)
  (let ((conditional (car kill-ring)))
    (save-excursion (newline))
    (insert conditional)
    (move-end-of-line 1)
    (insert ?:)
    (forward-line 1)
    (boo-indent)
    (move-end-of-line 1)
    (delete-char (- (skip-chars-backward " ")))
    (error "lol")))

(defun boo-multi-line->single-line ()
  (back-to-indentation)
  (kill-line)
  (let ((conditional (car kill-ring)))
    (delete-char (- (skip-chars-backward " \t")))
    (delete-backward-char 1)
    (forward-line 1)
    (back-to-indentation)
    (boo-deindent)
    (move-end-of-line 1)
    (delete-char (- (skip-chars-backward " ")))
    (insert ?\s)
    (insert conditional)
    (delete-char (- (skip-chars-backward " ")))
    (backward-char)
    (when (looking-at-p ":")
      (message "aoecraohercaoh")
      (delete-char 1))))

(defun boo-toggle-single-line-control-flow ()
  "Toggles single line and multi line control flow in boo, (trailing if vs if and indent"
  (interactive)
  (if (save-excursion (back-to-indentation) (boo-looking-at-control-flow))
      (boo-multi-line->single-line)
    (boo-single-line->multi-line)))

(define-derived-mode boo-mode prog-mode "Boo"
  "Major mode for editing Boo source code"
  (setq-local indent-line-function 'boo-indent-function)
  (setq-local comment-start "#")
  (define-key boo-mode-map (kbd "C-c m b") 'boo-mark-block)
  (define-key boo-mode-map (kbd "C-c s l c") 'boo-toggle-single-line-control-flow))

(provide 'boo-mode)
