;; Markdown setup
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdwn" . markdown-mode))

;; XML setup
(add-to-list 'auto-mode-alist '("\\.\\(xml\\|xsl\\|rng\\)\\'" . nxml-mode))

;; Org-mode setup
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; Ruby mode
(autoload 'ruby-mode "ruby-mode" "Ruby Mode" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-hook 'ruby-mode-hook
          (lambda()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (untabify (point-min) (point-max))
                           (delete-trailing-whitespace)
                           )))
            (set (make-local-variable 'indent-tabs-mode) 'nil)
            (set (make-local-variable 'tab-width) 2)
            (imenu-add-to-menubar "IMENU")
            (define-key ruby-mode-map (kbd "RET") 'newline-and-indent)
            (require 'ruby-electric)
            (ruby-electric-mode t)
            ))

;; Python setup
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\t" 'python-indent-line)))
(add-hook 'python-mode-hook 'hs-minor-mode)
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(autoload 'python-mode "python" "Python editing mode." t)

;; JavaScript setup
(autoload 'js2-mode' "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-bounce-indent-p t)
(setq inferior-js-program-command "node")
(add-hook 'js2-mode-hook '(lambda ()
			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
			    (local-set-key "\C-cb" 'js-send-buffer)
			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
			    (local-set-key "\C-cl" 'js-load-file-and-go)
                (setq autopair-dont-activate t)
			    ))

;; JS Comint mode
(setq inferior-js-program-command "node-repl")

;; Slime setup
(setq inferior-lisp-program "sbcl")
(require 'slime-autoloads)
(slime-setup)
(add-to-list 'auto-mode-alist '("\\.lisp$" . slime-mode))
(add-hook 'slime-mode-hook 'lisp-mode)

;; C setup
(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq c-basic-offset 8))

;; C++ setup
(defun cpp-mode ()
  "C++ mode adjusted for indentation and style"
  (interactive)
  (c++-mode)
  (c-set-style "K&R")
  (setq c-basic-offset 8))

(add-to-list 'auto-mode-alist '("\\.cpp$" . cpp-mode))


;; Ibuffer groupings
(setq ibuffer-saved-filter-groups
      (quote(("default"
              ("dired" (mode . dired-mode))
              ("Python" (or
                         (mode . python-mode)
                         (name . "\\.py$")
                         (name . "*Python*")))
              ("Common Lisp" (or
                              (mode . lisp-mode)
                              (name . "\\.lisp$")
                              (name . "^\\*inferior-lisp\\*$")
                              (name . "\\*SLIME")))
              ("C" (or
                    (filename . "\\.c$")))
              ("C++" (or
                      (filename . "\\.cpp$")))
              ("Headers" (or
                          (filename ."\\.h$")))
              ("Makefiles" (or
                            (mode . GNUmakefile)
                            (filename . "\\Makefile*")))
              ("Elisp" (or
                        (mode . emacs-lisp-mode)))
              ("Haskell" (or
                          (mode . haskell-mode)
                          (name . "\\.hs$")))
              ("HTML" (or
                       (name . "\\.html")
                       (name . "\\.css$")
                       (name . "\\.markdown$")))
              ("JavaScript" (or
                             (name . "\\.js$")
                             (name . "\\.pjs$")))
              ("PHP" (or
                      (name . "\\.php$")
                      (mode . php-mode)))
              ("Org" (or
                      (mode . org-mode)))
              ("Processing" (or
                             (name . "\\.pde$")))
              ("Ruby" (or
                       (mode . Ruby)
                       (name . "\\.rb$")))
              ("Shells" (or
                         (mode . shell-mode)))
              ("Latex" (or
                        (mode . LaTeX)
                        (name . "\\.tex$")
                        (name . "\*tex-")))
              ("Magit" (or
                        (name . "\*magit")))
              ("Mode events" (or
                             (name . "\\*Pymacs\\*")
                             (name . "\\*slime-events\\*")))
              ("Emacs" (or
                        (name . "\\*scratch\\*")
                        (name . "\\*Messages\\*")
                        (name . "\\*Completions\\*")
                        (name . "\\*GNU")))
              ("ML" (or
                     (name . "\\.ml[ily]?$")))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")
            (setq ibuffer-show-empty-filter-groups nil)
            (ibuffer-auto-mode 1)))

;; Shell mode
(ansi-color-for-comint-mode-on)

;; Processing mode
(autoload 'processing-mode "processing-mode" "Processing mode" t)
(add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))
(setq processing-location "~/processing/")

;; Smalltalk mode
(autoload 'smalltalk-mode "smalltalk-mode" "Smalltalk mode" t)

;; BNF mode
(autoload 'bnf-mode "bnf-mode" "Backus-Naur form mode" t)
(add-to-list 'auto-mode-alist '("\\.bnf$" . bnf-mode))

;; Standard ML setup
(autoload 'sml-mode "sml-mode" "Major mode for editing SML" t)
(autoload 'run-sml "sml-proc" "Run an inferior SML process" t)
(add-to-list 'auto-mode-alist '("\\.\\(sml\\|sig\\)\\'" . sml-mode))

;; Tuareg mode for OCaml
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(autoload 'tuareg-imenu-set-imenu "tuareg-imenu" 
  "Configuration of imenu for tuareg" t)
(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))
;; Haskell
(autoload 'haskell-mode "haskell-mode" "Haskell Mode" t)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Prolog setup
(setq prolog-system 'gnu)
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                               auto-mode-alist))
