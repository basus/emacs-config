;; Mode-specific settings

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
              ("Data" (or
                       (filename . "\\.data\\(<[0-9]+>\\)?$")))
              ("Elisp" (or
                        (mode . emacs-lisp-mode)))
              ("GNU plot" (or
                           (name . "\\.gp\\(<[0-9]+>\\)?$")
                           (name . "\\.gnu\\(<[0-9]+>\\)?$")))
              ("Haskell" (or
                          (mode . haskell-mode)
                          (name . "\\.hs$")))
              ("HTML" (or
                       (name . "\\.html")
                       (name . "\\.css")))
              ("Java" (or
                             (name . "\\.java")
                             (name . "\\.class$")))
              ("JavaScript" (or
                             (name . "\\.js$")
                             (name . "\\.pjs$")))
              ("PHP" (or
                      (name . "\\.php$")
                      (mode . php-mode)))
              ("OCaml" (or
                     (name . "\\.ml[ily]?\\(<[0-9]+>\\)?$")
                     (name . "_tags\\(<[0-9]+>\\)?$")
                     (name . "_oasis\\(<[0-9]+>\\)?$")
                     ))
              ("Org" (or
                      (mode . org-mode)))
              ("Pollen" (or
                         (mode . pollen-mode)
                         (name . "\\.pm$")))
              ("Processing" (or
                             (name . "\\.pde$")))
              ("Racket" (or
                         (name . "\\.rkt$")
                         (mode . Scheme)
                         (name . "\\* Racket REPL \\*")))
              ("Restructured Text" (or
                                    (name . "\\.rst$")))
              ("Ruby" (or
                       (mode . Ruby)
                       (name . "\\.rb$")))
              ("Shell Script" (or
                               (name . "\\.sh\\(<[0-9]+>\\)?$")))
              ("Shells" (or
                         (mode . shell-mode)))
              ("LaTeX" (or
                        (mode . latex-mode)
                        (mode . tex-mode)
                        (mode . bibtex-mode)
                        (mode . TeX-output-mode)
                        (name . "\\.tex")
                        (name . "\*tex-")
                        (name . "\\.cls$")))
              ("Magit" (or
                        (name . "\*magit")))
              ("Markdown" (or
                           (name . "\\.markdown$")
                           (name . "\\.md$")
                           (name . "\\.mdwn$")))
              ("Mode events" (or
                             (name . "\\*Pymacs\\*")
                             (name . "\\*slime-events\\*")))
              ("Emacs" (or
                        (name . "\\*scratch\\*")
                        (name . "\\*Messages\\*")
                        (name . "\\*Completions\\*")
                        (name . "\\*GNU")))
              ))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")
            (setq ibuffer-show-empty-filter-groups nil)
            (ibuffer-auto-mode 1)))

;; Enable evil-aware paredit in Lisp-related modes
(add-hook 'geiser-mode-hook 'evil-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)

;; Custom C style
(defun custom-c-setup ()
  (setq c-default-style "linux")
  (setq c-electric-pound-behavior (quote (alignleft)))
  (c-set-offset 'substatement-open 0)   ; body of "if" in java/c
  (c-set-offset 'inline-open 0)         ; body of function in java
  (setq c-continued-statement-offset 4) ; multi-line statement
  (setq c-tab-always-indent t)          ; TAB always indents
  (setq c-brace-offset 0)               ; no effect afaik
  (setq c-basic-offset 4)
  (setq c-brace-imaginary-offset 0)
  (setq c-argdecl-indent 0)
  (setq c-label-indent 2)
  (setq c-label-offset 9)
  (setq indent-tabs-mode nil)
  (modify-syntax-entry ?_ "w" c-mode-syntax-table)
  (local-set-key "\C-\\\C-e" 'wrap-define)
  (auto-fill-mode nil)
)

(add-hook 'c-mode-common-hook 'custom-c-setup)

;; Insert a ◊ (lozenge) for use with pollen
(add-hook 'pollen-mode-hook
          (lambda () (key-chord-define evil-insert-state-map "zz"
                                       (lambda () (interactive)
                                         (insert (decode-char 'ucs 9674))))))
(add-to-list 'auto-mode-alist '("\\.pm$" . pollen-mode))

;; Eclim mode for interaction with Eclipse
(global-eclim-mode)

;; Pylint support via flymake
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

;; LaTeX related configuration
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

;; Use HTML mode instead of HTML-helper-mode
(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))
