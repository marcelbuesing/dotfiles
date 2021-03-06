;;;
;;; Switch buffer with C-tab
;;;
(global-set-key (kbd "<C-tab>") 'next-buffer)
(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-buffer)
(global-set-key (kbd "<M-SPC>") 'avy-goto-char)

;;
;; Duplicate line keybinding
;;
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )
(global-set-key (kbd "<M-down>") 'duplicate-line)

;;; Whitespace
(global-whitespace-mode 1)

;;; Max line length
(setq whitespace-line-column 120)

;; Jump between buffers
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
