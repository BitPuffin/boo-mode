(require 'f)

(defvar boo-mode-support-path
  (f-dirname load-file-name))

(defvar boo-mode-features-path
  (f-parent boo-mode-support-path))

(defvar boo-mode-root-path
  (f-parent boo-mode-features-path))

(add-to-list 'load-path boo-mode-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'boo-mode)
  (require 'espuds)
  (require 'ert))

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
