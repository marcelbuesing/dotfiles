;;; Sammy.el --- summary

;;; Commentary:

;;; Code:

;; Ensure all the modules are available, excluding structured-haskell-mode.
(prelude-require-packages '(paredit
                            cider
			    cljsbuild-mode
			    sublime-themes
			    markdown-mode
			    multiple-cursors
			    find-file-in-project
			    auto-complete
			    yasnippet
			    flymake-coffee
			    expand-region
                            auctex))

;; Default Font
(set-default-font "Anonymous Pro Bold 12")

;; No flyspell anymore (for now)
(setq prelude-flyspell nil)

;; No flycheck for SCSS
(eval-after-load 'flycheck
  '(setq-default flycheck-disabled-checkers '(scss)))

;;; Flycheck mode coffeescript
(add-hook 'coffee-mode-hook 'flymake-coffee-load)


;;;
;;; Use monokai theme
;;;
;;(require 'monokai-theme)
(load-theme 'spolsky t)

;;(load-theme 'spolsky t)
;; Configure SHM colors for solarized-dark
;;(set-face-background 'shm-current-face "#073642") ; solarized-base02


;;;
;;; Structured Haskell Mode
;;;
(add-to-list 'load-path "~/.emacs.d/structured-haskell-mode/elisp")
(require 'shm)
(add-hook 'haskell-mode-hook 'structured-haskell-mode)
(setq shm-program-name "~/.emacs.d/structured-haskell-mode/dist/build/structured-haskell-mode/structured-haskell-mode")
(setenv "PATH" (shell-command-to-string "echo $PATH"))

;; Use Haskell-mode specific save
(setq exec-path (append exec-path '("~/.cabal/bin")))

;; Automatic formatting with stylish-haskell
(setq haskell-stylish-on-save t)

;; Use SHM highlighting, so disable global line highlighting.
(eval-after-load 'structured-haskell-mode
  '(global-hl-line-mode 0))

;;;
;;; Autocomplete Haskell
;;;
(require 'ac-haskell-process) ; if not installed via package.el
(add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup)
(add-hook 'haskell-interactive-mode-hook 'ac-haskell-process-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'haskell-interactive-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (add-to-list 'completion-at-point-functions 'auto-complete))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-to-list 'ac-modes 'haskell-interactive-mode)
(add-hook 'haskell-interactive-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'haskell-mode-hook 'set-auto-complete-as-completion-at-point-function)

(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "C-c C-d") 'ac-haskell-process-popup-doc))
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "C-h C-h") 'haskell-hoogle))

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
;;; Avoid other directory listing
;;;
(global-unset-key (kbd "C-x C-d"))

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
(global-set-key (kbd "M-+") 'mc/mark-next-like-this)
(global-set-key (kbd "M-#") 'mc/mark-all-like-this)

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
;;; ido-imenu (emacs rocks #10), easy jump to search result
;;;


(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))
(global-set-key (kbd "C-x f") 'ido-imenu)

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
;;; Clojure mode
;;;
;; Prevent excessive indentation.
(setq clojure-defun-style-default-indent t)

;;;
;;; Cider REPL
;;;
;; Enhance REPL with Paredit and rainbow delimiters
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;;;
;;; dirtree
;;;
;;(require 'dirtree)
;;(global-set-key (kbd "C-x d") 'dirtree)

;;;
;;; coffee-mode
;;;
(custom-set-variables '(coffee-tab-width 2))

;; Circumflex
;;(define-key key-translation-map [dead-circumflex] "^")

;;;
;;; LaTex
;;;
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(setq TeX-PDF-mode t)

(provide 'sammy)
;;; sammy.el ends here
;;
