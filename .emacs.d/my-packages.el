(require 'package)
(add-to-list 'package-archives
		 '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
		 '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar required-packages
  '(
    magit
    yasnippet
    undo-tree
    smart-tabs-mode
    php-mode
    js2-mode
    rust-mode
    )
  "a list of packages to ensure are installed at launch")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

;; function to check if all our packages are installed
(defun packages-installed-p ()
  (loop for p in required-packages
	when (not (package-installed-p p)) do (return nil)
	finally (return t)))

;; if not all packages are installed, find and install the missing ones
(unless (packages-installed-p)
  ; check for new packages
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" "done.")
  ; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
	(package-install p))))
