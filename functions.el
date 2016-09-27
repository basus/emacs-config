
;; Word wrapping by window size
(defun wrap ()
  "Wrap lines by window size"
  (interactive)
  (setf longlines-wrap-follows-window-size nil)
  (setf fill-column 100)
  (longlines-mode))

;; Move to the previous window
(defun other-window-backward (&optional n)
  "Move to the Nth previous window"
  (interactive "P")
  (other-window (- (prefix-numeric-value n ))))

;; Repositioning the buffer in the current window

(defun line-to-top ()
  "Move current line to top of window"
  (interactive)
  (recenter O))

(defun scroll-n-lines-ahead (&optional n)
  "Scroll ahead N lines (1 by default)."
  (interactive "P")
  (scroll-ahead (prefix-numeric-value n)))

(defun scroll-n-lines-behind (&optional n)
  "Scroll behind N lines (1 by default)."
  (interactive "P")
  (scroll-behind (prefix-numeric-value n)))

(defun point-to-top ()
  "Move cursor to top of current window"
  (interactive)
  (move-to-window-line 0))

(defun point-to-bottom ()
  "Move cursor to last line of window"
  (interactive)
  (move-to-window-line -1))

;; File manipulation
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME." 
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
	(progn 	 (rename-file name new-name 1)
                 (rename-buffer new-name)
                 (set-visited-file-name new-name)
                 (set-buffer-modified-p nil))))))

(defun remove-newlines()
  "Remove newlines from a buffer"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward "\n")
      (replace-string "\n" "")
      (forward-char))))

;; Connect slime automatically on new lisp file
(defun start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))

;; Toggle fullscreen
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))

;; Date and time insertion functions
(setq insert-date-format "%Y-%m-%d %a")
(setq insert-time-format "%H:%M:%S %Z")

(defun insert-date ()
  "Inserts current date at point"
  (interactive "*")
  (insert (format-time-string insert-date-format)))

(defun insert-time ()
  "Inserts current time at point"
  (interactive "*")
  (insert (format-time-string insert-time-format)))

(defun insert-date-time ()
  "Inserts date and time combination at current point"
  (interactive "*")
  (insert-date) (insert " ") (insert-time))

(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

(defun center-body ()
  (interactive)
  (linum-mode -1)
  (let* ((max-text-width 100)
         (margin (max 0 (/ (- (window-width) max-text-width) 2))))
    (setq-local left-margin-width margin)
    (setq-local right-margin-width margin)
    (set-window-buffer nil (current-buffer))))

(defun uncenter-body ()
  (interactive)
  (setq-local left-margin-width 0)
  (setq-local right-margin-width 0)
  (set-window-buffer nil (current-buffer)))

