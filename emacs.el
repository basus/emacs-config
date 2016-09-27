;; Require Common Lisp compatibility
(require 'cl)

;; Load local configurations
(add-to-list 'load-path "~/src/emacs/")
(load "~/.emacs.d/custom.el")
(load "packages")
(load "keys")
(load "modes")
(load "functions")
(load "fira.el")

;; User interface preferences
(when window-system
  (if (fboundp 'tabbar-mode) (tabbar-mode -1))
  (tool-bar-mode      -1)
  (scroll-bar-mode    -1)
  (blink-cursor-mode  -1)
  (menu-bar-mode       1)
  )

;; Miscellaneous behavior
(setq
 visible-bell               t
 vc-follow-symlinks         t
 x-select-enable-clipboard  t
 make-backup-files          nil
 coding-system-for-read     'utf-8
 coding-system-for-write    'utf-8
 confirm-kill-emacs         'y-or-n-p
 initial-major-mode         'fundamental-mode
)

;; Default behaviors
(setq-default
 show-trailing-whitespace  t
 indent-tabs-mode          nil
 fill-column               80
 auto-fill-function        'do-auto-fill
 )

;; Global mode settings
(savehist-mode)
(show-paren-mode)
(global-linum-mode)
(column-number-mode)
(global-hl-line-mode)

;; Aquamacs specific settings
(if (boundp 'aquamacs-version)
    (progn
      (setq shell-command-switch "-lc")
      (set-default-font "Fira Code 14")
      (customize-set-variable 'aquamacs-autoface-mode nil)
      (customize-set-variable 'aquamacs-additional-fontsets nil t)
      (customize-set-variable 'aquamacs-customization-version-id 307 t)
      (customize-set-variable 'aquamacs-tool-bar-user-customization nil t)
      (exec-path-from-shell-initialize))

  (set-default-font "Inconsolata for Powerline 11")
)

;; Start server
(server-start)
