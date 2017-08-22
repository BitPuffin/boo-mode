;;; Boo mode for emacs
;;; Author: Isak Andersson

(defvar boo-tab-width tab-width "Boo tab width (default same value as 'tab-width')")

(defun boo--de-indent ()
  "Removes one level of indentation"
  (back-to-indentation)
  (unless (= 0 (current-column))
    (backward-char)
    (if (looking-at-p "\t")
        (delete-char 1)
      (progn
        (delete-char 1)
        (delete-backward-char (1- boo-tab-width))))))

(defun boo--line-is-empty-p ()
  "Checks if the current line the cursor is on is either empty, or contains nothing but spaces"
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun boo--kill-to-eol-bypass-ring ()
  (let ((region-end (save-excursion (move-end-of-line 1) (point))))
    (delete-and-extract-region (point) region-end)))

(defun boo--skip-indentation-backward ()
  "Skips tabs and spaces backwards"
  (skip-chars-backward " \t"))

(defun boo--line-ends-with-colon-p ()
  (let ((looking-at-colon nil))
    (save-excursion
      (move-end-of-line 1)
      (unless (= 0 (current-column))
        (boo--skip-indentation-backward)
        (unless (= 0 (current-column))
          (backward-char))        
        (setq looking-at-colon (= ?: (following-char)))))
    looking-at-colon))

(defvar boo-de-indent-words (list "return" "pass" "continue" "break"))

(defun boo-indent-function ()
  (back-to-indentation)
  (when (boo--line-is-empty-p)
    (let ((should-indent nil)
          (should-go-deeper nil)
          (previous-indent 0))
      (save-excursion
        (forward-line -1)
        (move-end-of-line 1)
        (unless (= 0 (current-column))
          (boo--skip-indentation-backward)
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
            (indent-to previous-indent))))))

(defun boo-indent ()
  "Indent appropriately"
  (interactive)
  (boo-indent-function))

(defun boo--mark-trailing-block ()
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
                (setq keep-looking (not (boo--line-ends-with-colon-p))))))
        (while keep-looking
          (forward-line 1)
          (setq keep-looking (not (eobp)))
          (when keep-looking
            (back-to-indentation)
            (unless (boo--line-is-empty-p)
              (setq keep-looking (> (current-column) start-col)))))))
    (unless (eobp)
      (forward-line -1)
      (move-end-of-line 1))
    (set-mark (point))))

(defun boo--mark-inline-block ()
  "Marks the block that the cursor is currently inside"
  (back-to-indentation)
  (let ((start-indent (current-column))
        (keep-looking t)
        (reached-eob nil))
    (if (and (not (boo--line-is-empty-p)) (= 0 start-indent))
        (mark-whole-buffer)
      (save-excursion
        (while keep-looking
          (forward-line 1)
          (if (eobp)
              (progn
                (setq keep-looking nil)
                (setq reached-eob t))
            (unless (boo--line-is-empty-p)
              (back-to-indentation)
              (when (< (current-column) start-indent)
                (setq keep-looking nil)))))
        (unless reached-eob
          (forward-line -1)
          (move-end-of-line 1))
        (set-mark (point)))
      (setq keep-looking t)
      (while keep-looking
        (forward-line -1)
        (unless (boo--line-is-empty-p)
          (move-end-of-line 1)
          (boo--skip-indentation-backward)
          (backward-char)
          (when (= (following-char) ?:)
            (back-to-indentation)
            (when (< (current-column) start-indent)
              (setq keep-looking nil))))))))

(defun boo-mark-block ()
  "Marks the traling block if cursor is on the line that starts it. Otherwise it marks the enclosing block."
  (interactive)
  (if (boo--line-ends-with-colon-p)
      (boo--mark-trailing-block)
    (boo--mark-inline-block)))

(defun boo--looking-at-control-flow-p ()
  (or (looking-at-p "if")
      (looking-at-p "unless")
      (looking-at-p "while")))

(defun boo--single-line->multi-line ()
  (move-end-of-line 1)
  (delete-char (- (boo--skip-indentation-backward)))
  (unless (or (search-backward "if" (save-excursion (back-to-indentation) (point)) t)
              (search-backward "unless" (save-excursion (back-to-indentation) (point)) t)
              (search-backward "while" (save-excursion (back-to-indentation) (point)) t))
    (error "No control flow keyword found!"))
  (let ((conditional (boo--kill-to-eol-bypass-ring)))
    (back-to-indentation)
    (save-excursion (newline))
    (insert conditional)
    (move-end-of-line 1)
    (insert ?:)
    (forward-line 1)
    (boo-indent)
    (move-end-of-line 1)
    (delete-char (- (boo--skip-indentation-backward)))))

(defun boo--multi-line->single-line ()
  (back-to-indentation)
  (let ((conditional (boo--kill-to-eol-bypass-ring)))
    (delete-char (- (boo--skip-indentation-backward)))
    (delete-backward-char 1)
    (forward-line 1)
    (back-to-indentation)
    (boo--de-indent)
    (move-end-of-line 1)
    (delete-char (- (boo--skip-indentation-backward)))
    (insert ?\s)
    (insert conditional)
    (delete-char (- (boo--skip-indentation-backward)))
    (backward-char)
    (when (looking-at-p ":")
      (message "aoecraohercaoh")
      (delete-char 1))))

(defun boo-toggle-single-line-control-flow ()
  "Toggles single line and multi line control flow in boo, (trailing if vs if and indent"
  (interactive)
  (if (save-excursion (back-to-indentation) (boo--looking-at-control-flow-p))
      (boo--multi-line->single-line)
    (boo--single-line->multi-line)))

(define-derived-mode boo-mode prog-mode "Boo"
  "Major mode for editing Boo source code"
  (setq-local indent-line-function 'boo-indent-function)
  (setq-local comment-start "#")
  (define-key boo-mode-map (kbd "C-c m b") 'boo-mark-block)
  (define-key boo-mode-map (kbd "C-c s l c") 'boo-toggle-single-line-control-flow)

  (mapc (lambda (i) ; set up syntax table with a list of pairs where car is a character and cdr is the syntax descriptor
          (modify-syntax-entry (car i) (cdr i) boo-mode-syntax-table))
        '((?/  . ". 124")
          (?*  . ". 23b")
          (?#  . "<")
          (?\n . ">")
          (?.  . ".")
          (?+  . ".")
          (?-  . ".")
          (?<  . ".")
          (?>  . ".")
          (?=  . ".")
          (?:  . ".")
          (?\; . ".")
          (?_  . "_")
          (?\" . "\"")
          (?'  . "\"")
          (?\( . "()")
          (?\) . ")(")
          (?\[ . "(]")
          (?\] . ")[")
          (?{  . "(}")
          (?}  . "){")))
  (let ((kwds (list (regexp-opt '("def"
                                  "module"
                                  "import"
                                  "class"
                                  "constructor"
                                  "destructor"
                                  "struct"
                                  "if"
                                  "elif"
                                  "else"
                                  "public"
                                  "private"
                                  "protected"
                                  "override"
                                  "virtual"
                                  "return"
                                  "interface"
                                  "macro"
                                  "as"
                                  "cast"
                                  "raise"
                                  "except"
                                  "try"
                                  "super"
                                  "yield"
                                  "namespace"
                                  "unless"
                                  "match"
                                  "case"
                                  "otherwise"
                                  "each" ;temporary
                                  "in"
                                  "for"
                                  "while"
                                  "var"
                                  "of"
                                  "static"
                                  "null"
                                  "is"
                                  "self"
                                  "continue"
                                  "get"
                                  "set")
                                'words))))
    (setq-local font-lock-defaults (list kwds nil nil nil))))

(provide 'boo-mode)
