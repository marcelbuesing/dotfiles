;;; Sammy.el --- summary

;;; Commentary:

;;; Code:

;; Ensure all the modules are available, excluding structured-haskell-mode.
(prelude-require-packages '(paredit 
			    cljsbuild-mode 
			    monokai-theme 
			    multiple-cursors
			    find-file-in-project 
			    auto-complete 
			    yasnippet
			    expand-region))

;; No flyspell anymore (for now)
(setq prelude-flyspell nil)

;; No flycheck for SCSS
(eval-after-load 'flycheck
  '(setq-default flycheck-disabled-checkers '(scss)))
;;;
;;; Use monokai theme
;;;
(require 'monokai-theme)
(load-theme 'monokai t)

;;;
;;; Find file in project (.git)
;;;
(require 'find-file-in-project)
(setq ffip-project-file ".gitmodules")
(setq ffip-find-options "-not -regex \".*vendor.*\" -not -regex \".*generated.*\" -not -regex \".*dist.*\" -not -regex \".*node_modules.*\"")
(setq ffip-patterns (concatenate 'list '("*.haml" "*.erb" "*.sass" "*.scss" "*.xml" "*.yml" "*.js" "*.coffee" "*.json") ffip-patterns))
(setq ffip-limit 6096)
(global-set-key (kbd "C-o") 'find-file-in-project)

;;;
;;; Switch buffer with C-tab
;;;
(global-set-key (kbd "<C-tab>") 'next-buffer)
(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-buffer)

;;;
;;; Store Session when closing
;;;
;;(desktop-save-mode 1)

;;;
;;; auto-complete
;;;
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-modes 'coffee-mode)

;;;
;;; Ace-jump-mode
;;;
(global-set-key (kbd "C-c l") 'ace-jump-line-mode)
(global-set-key (kbd "C-c c") 'ace-jump-char-mode)

;;;
;;; multiple-cursors
;;;
(global-set-key (kbd "C-'") 'mc/mark-next-like-this)
(global-set-key (kbd "C-\"") 'mc/mark-all-like-this)

;;;
;;; Yasnippet
;;;
(require 'yasnippet)
;; (require 'angularsnippet)
(yas-global-mode 1)
;; git clone https://github.com/AndreaCrotti/yasnippet-snippets.git ~/.emacs.d/snippets
(add-to-list 'yas/root-directory "~/.emacs.d/snippets")

;;;
;;; Cleanup Buffer (Autoformat Code)
;;;

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))
(global-set-key (kbd "C-x c") 'cleanup-buffer)

;;;
;;; Pending delete mode
;;;
(pending-delete-mode 1)

;;;
;;; expand-region-mode
;;;
(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)

;;;
;;; Paredit
;;;
(add-hook 'clojure-mode-hook 'paredit-mode)

;;;
;;; dirtree
;;;
(require 'dirtree)
(global-set-key (kbd "C-x d") 'dirtree)

;;;
;;; coffee-mode
;;;
(custom-set-variables '(coffee-tab-width 2))

;; Circumflex
;;(define-key key-translation-map [dead-circumflex] "^")

(provide 'sammy)
;;; sammy.el ends here
;;
