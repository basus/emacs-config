;; Initialize the package manager
(require 'package)
(setq package-archives
      '(("org"       . "http://orgmode.org/elpa/")
        ("gnu"       . "http://elpa.gnu.org/packages/")
        ("melpa"     . "https://melpa.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Auctex, defer loading until use
(use-package auctex :ensure t :defer t
  :config
  (setq TeX-PDF-mode   t
        TeX-auto-save  t
        TeX-parse-self t)
  (setq-default TeX-master nil)
  )

;; Boogie friends for interaction with Z3
(use-package boogie-friends :ensure f :defer t
  :commands
  (boogie-friends z3-smt2-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.z3$" . z3-smt2-mode))
  :config
  (setq flycheck-z3-smt2-executable (executable-find "z3"))
  )

;; Counsel
(use-package counsel :ensure t)

;; Evil mode
(use-package evil :ensure t
  :init
  (setq evil-want-fine-undo t)
  (setq evil-want-C-i-jump nil)
  (setq evil-disable-insert-state-bindings t)

  :config
  (evil-mode 1)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'message-mode 'motion)
  (evil-set-initial-state 'help-mode 'emacs)

  (use-package evil-escape :ensure t
    :diminish
    (evil-escape-mode)
    :config
    (evil-escape-mode)
    (setq-default evil-escape-key-sequence "xq"
                  evil-escape-delay 0.2)
    (setq evil-escape-unordered-key-sequence t))

  (use-package evil-matchit :ensure t
    :commands
    evilmi-jump-items
    :config
    (global-evil-matchit-mode 1))

  (use-package evil-surround :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-visual-mark-mode :ensure t
    :config
    (evil-visual-mark-mode))

  (use-package evil-visualstar :ensure t
    :config
    (global-evil-visualstar-mode t))

  (setq evil-insert-state-cursor  '("#268bd2" bar)  ;; blue
        evil-normal-state-cursor  '("#b58900" box)  ;; blue
        evil-visual-state-cursor  '("#cb4b16" box)  ;; orange
        evil-replace-state-cursor '("#859900" hbar) ;; green
	evil-emacs-state-cursor   '("#d33682" box)) ;; magenta
  )

;; Needed for proper editing on OS X, especially with OCaml
(use-package exec-path-from-shell :ensure t
  :defer 5
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  )

;; Hydra for repeatable keybindings that emulate custom prefix keys
(use-package hydra :ensure t
  :config
  (use-package ivy-hydra :ensure t)
  )

;; General mode for better keybindings everywhere
(use-package general :ensure t)

;; Geiser mode, for use with Racket
(use-package geiser :ensure t :defer t
  :config
  (setq geiser-active-implementations '(racket)))

;; Ivy mode for completion
(use-package ivy :ensure t
  :commands ivy-switch-buffer
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((nil . ivy--regex-fuzzy)
	  (t   . ivy--regex-ignore-order)))
  )

;; Key chord, maybe removable with proper keybinding setup
(use-package key-chord :ensure t)

;; Magit
(use-package magit :ensure t
  :commands
  (magit-blame
   magit-commit
   magit-commit-popup
   magit-diff-popup
   magit-diff-unstaged
   magit-fetch-popup
   magit-init
   magit-log-popup
   magit-pull-popup
   magit-push-popup
   magit-revert
   magit-stage-file
   magit-status
   magit-unstage-file
   magit-blame-mode)

  :config
  (global-git-commit-mode)
  (setq magit-last-seen-setup-instructions "1.4.0"))

;; Markdown mode
(use-package markdown-mode :ensure t :defer t)

;; OCP-indent for proper OCaml environment setup
(eval-and-compile
  (defun opam-share ()
    (substring
     (shell-command-to-string "opam config var share 2> /dev/null")
     0 -1)))

(use-package ocp-indent :ensure t :defer t
  :load-path (lambda () ( concat ( opam-share ) "/emacs/site-lisp") )
)

;; Pollen mode for my personal website
(use-package pollen-mode :ensure f :defer t
  :commands pollen-mode)

;; Python mode
(use-package python-mode :ensure t :defer t)

;; Swiper for Ivy-enhanced incremental search
(use-package swiper :ensure t
  :commands swiper
  )

;; Tuareg mode
(use-package tuareg :ensure t :defer t
  :config
  (add-hook 'tuareg-mode-hook
            (lambda () (setq indent-line-function 'ocp-indent-line))))

;; Which-key, mostly for use with general
(use-package which-key :ensure t
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  ;; simple then alphabetic order.
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (setq which-key-popup-type 'side-window
	which-key-side-window-max-width 0.33
	which-key-idle-delay 0.5)
  )

;; Yasnippet for text expansions
(use-package yasnippet
  :diminish yas-minor-mode
  :defer 10
  :init
  (with-eval-after-load 'yasnippet
    (progn
      (setq yas-snippet-dirs (append yas-snippet-dirs
				     '("~/.emacs.d/snippets")))))
  :config
  (yas-global-mode)
  (setq yas-indent-line nil))


;; Color theme -- zenburn for now
(use-package zenburn-theme :ensure t
  :init
  ;; to make the byte compiler happy.
  ;; emacs25 has no color-themes variable
  (setq color-themes '())
  :config
  ;; load the theme, don't ask for confirmation
  (load-theme 'zenburn t))
