;; General movement and editing
(global-set-key (kbd "M-<left>") 'other-window-backward)
(global-set-key (kbd "M-<right>") 'other-window)
(global-set-key (kbd "\M-<") 'beginning-of-buffer)
(global-set-key (kbd "\M->") 'end-of-buffer)

;; Character based actions
(global-set-key "\M-f" 'forward-char)
(global-set-key "\M-b" 'backward-char)
(global-set-key "\M-d" 'delete-char)
(global-set-key "\M-w" 'backward-delete-char)

;; Word based actions
(global-set-key "\C-f" 'forward-word)
(global-set-key (kbd "C-<right>") 'forward-word)
(global-set-key "\C-b" 'backward-word)
(global-set-key (kbd "C-<left>") 'backward-word)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-d" 'kill-word)

;; Editing actions
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-x\C-l" 'kill-ring-save)
(global-set-key "\C-v" 'yank-pop)
(global-set-key "\M-r" 'replace-string)
(global-set-key "\M-l" 'wrap)
(global-set-key "\M-c" 'compile)

;; Indenting actions
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key "\C-i" (lambda () (interactive) (insert "    ")))
(global-set-key (kbd "TAB") 'indent-according-to-mode)
(global-set-key (kbd "C-x a r") 'align-regexp)

;; Repositioning Buffer in window
(global-set-key "\C-mp" 'line-to-top)
(global-set-key "\C-mw" 'scroll-n-lines-behind)
(global-set-key "\C-ms" 'scroll-n-lines-ahead)
(global-set-key "\C-ma" 'point-to-top)
(global-set-key "\C-md" 'point-to-bottom)

;; UI actions
(global-set-key [f11] 'toggle-fullscreen)