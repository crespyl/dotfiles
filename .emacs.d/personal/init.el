(require 'cl)

;;
;; function to help make sure we have all the packages we want
;;
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it's not.
Return a list of installed packages or nil for every package not installed"
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         package
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         nil)))
   packages))

;; make sure we've downloaded the package archive descriptions
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; basic 'quality of life' packages
(ensure-package-installed 'smart-tabs-mode)

;; core toolkit packages
(ensure-package-installed 'helm 'magit)

;; language modes
(ensure-package-installed 'php-mode 'js2-mode 'less-css-mode 'twig-mode 'mustache-mode)

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;
;; customization
;;

;; smart-tabs-mode
(setq-default tab-width 6)
(setq cua-auto-tabify-rectangles nil)
(setq-default indent-tabs-mode t)
(setq indent-tabs-mode t)

(defadvice align (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice align-regexp (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice indent-relative (around smart-tabs activate)
  (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice indent-according-to-mode (around smart-tabs activate)
  (let ((indent-tabs-mode indent-tabs-mode))
    (if (memq indent-line-function
              '(indent-relative
                indent-relative-maybe))
        (setq indent-tabs-mode nil))
    ad-do-it))

(defmacro smart-tabs-advice (function offset)
  `(progn
     (defvaralias ',offset 'tab-width)
     (defadvice ,function (around smart-tabs activate)
	 (cond
	  (indent-tabs-mode
	   (save-excursion
	     (beginning-of-line)
	     (while (looking-at "\t*\\( +\\)\t+")
		 (replace-match "" nil nil nil 1)))
	   (setq tab-width tab-width)
	   (let ((tab-width fill-column)
		   (,offset fill-column)
		   (wstart (window-start)))
	     (unwind-protect
		   (progn ad-do-it)
		 (set-window-start (selected-window) wstart))))
	  (t
	   ad-do-it)))))

(smart-tabs-advice js2-indent-line js2-basic-offset)

(smart-tabs-insinuate 'c 'javascript)

;; disable flyspell in programming modes
(defun adjust-prelude-prog-mode-defaults ()
  (flyspell-prog-mode))

(add-hook 'prelude-prog-mode-hook 'adjust-prelude-prog-mode-defaults t)

;; colors/themes
(disable-theme 'zenburn)

;; load custom keybindings
(load "bindings.el")

;; start server
(server-start)
