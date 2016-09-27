;; Window movement commands, prefixed with C-w
(defhydra hydra-window ()
   "Window Control Commands"
   ( "|"   split-window-right)
   ( "-"   split-window-below )
   ( "+"   balance-windows )
   ( "d"   delete-window )
   ( "D"   delete-other-windows )
   ( "o"   other-window)
   ( "O"   other-window-backward)
   )
(general-define-key
 :states '(normal visual insert emacs motion)
 "C-w" 'hydra-window/body)

;; Commands and actions involving multiple buffers
(general-define-key
 :prefix "C-c"
 "b"   '(ivy-switch-buffer          :which-key "buffers")
 "B"   '(ibuffer                    :which-key "iBuffer")
 "f"   '(counsel-find-file          :which-key "files"  )
 "r"   '(counsel-recentf            :which-key "recent" )
 "g"   '(magit-status               :which-key "Magit"  )
 "k"   '(kill-buffer                :which-key "kill buffer")

 ;; Complex operations, or ones that shouldn't be triggered by accident
 "C-e" '(save-buffers-kill-terminal :which-key "exit")
 )

;; Operations within a buffer
(general-define-key
 :prefix "C-x"
 "a"  '(align-regexp           :which-key "align")
 "e"  '(next-error             :which-key "next error")
 "k"  '(kill-region            :which-key "cut")
 "c"  '(kill-ring-save         :which-key "copy")
 "s"  '(counsel-grep-or-swiper :which-key "search forward" )
 "S"  '(swiper                 :which-key "search backward")
 "r"  '(replace-string         :which-key "replace string")
 "R"  '(replace-regex          :which-key "replace regex")

 ;; More complex operations
 "C-s" '(save-buffer :which-key "save buffer")
 "C-c" '(compile     :which-key "compile")
)

;; Indenting actions
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "TAB") 'indent-according-to-mode)

;; Return to normal state from evil's insert state
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)
