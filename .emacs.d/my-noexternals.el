;; setup rust racer autocomplete
(setq racer-rust-src-path "/home/peter/workspace/rust/rust/src")
(setq racer-cmd "/home/peter/workspace/rust/racer/target/release/racer")
(add-to-list 'load-path "~/workspace/rust/racer/editors")
(eval-after-load "rust-mode" '(require 'racer))

;; remove bars
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; windmove
(global-set-key (kbd "C-c C-h") 'windmove-left)
(global-set-key (kbd "C-c C-j") 'windmove-down)
(global-set-key (kbd "C-c C-k") 'windmove-up)
(global-set-key (kbd "C-c C-l") 'windmove-right)
