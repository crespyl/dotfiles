(load "~/.emacs.d/my-packages.el")

;; magit
(require 'magit)
(define-key global-map (kbd "C-c m") 'magit-status)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(yas-load-directory "~/.emacs.d/snippets")
(add-hook 'term-mode-hook (lambda()
			    (setq yas-dont-activate t)))

;; smart tabs mode
(require 'smart-tabs-mode)
(setq-default tab-width 6)
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
(smart-tabs-advice rust-mode-indent-line c-basic-offset)
(smart-tabs-insinuate 'c 'javascript)

;; undo tree mode
(require 'undo-tree)
(global-undo-tree-mode)
(define-key global-map (kbd "M-p") 'undo-tree-visualize-undo-to-x)
(define-key global-map (kbd "M-n") 'undo-tree-visualize-redo-to-x)

;; php mode
(require 'php-mode)
(add-hook 'php-mode-hook '(lambda () (c-set-style "cc-mode")))
