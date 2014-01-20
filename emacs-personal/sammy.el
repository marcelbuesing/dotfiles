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
			    haskell-mode
			    flycheck-haskell
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
