;; Set file load paths and load additional files
(setq emacs-root "~/src/emacs/")
(setq custom-file "~/.emacs.d/custom.el")

;; Stuff I've pulled from the internet. I require or load them as needed
;; (labels ((add-path (p)
;;                     (add-to-list 'load-path
;;                                 (concat emacs-root p))))
;;   (add-path "lisp")
;;   (add-path "net")
;;   (add-path "net/scala")
;;   (add-path "net/slime")
;;   (add-path "net/sml-mode")
;;   (add-path "net/haskell-mode")
;;   (add-path "net/org/lisp")
;;   (add-path "net/org2blog")
;;   (add-path "net/tuareg")
;;   (add-path "net/simplenote")
;;   (add-path "net/php-mode")
;;   (add-path "net/emacs-color-theme-solarized")
;; )


(load "keys")
(load "functions")
(load "modes")

;; Set user interface preferences
(setq visible-bell t)
(set-frame-parameter nil 'font-backend '(xft))

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Tab behavior
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Other behavior
(setq x-select-enable-clipboard t)
(setq make-backup-files nil)
(setq-default fill-column 80)
(setq-default auto-fill-function 'do-auto-fill)
(setq-default show-trailing-whitespace t)

;; Global mode setings
(show-paren-mode t)
(global-hl-line-mode t)
(global-auto-complete-mode t)
(global-linum-mode t)
(column-number-mode t)
(autopair-global-mode)
