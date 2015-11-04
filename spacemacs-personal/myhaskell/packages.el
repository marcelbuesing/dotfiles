(defvar myhaskell-packages
  '(
    ;; package myhaskell go here
    ;; shm
    ;; haskell-mode
   )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar myhaskell-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function myhaskell/init-<package-myhaskell>
;;
(defun myhaskell/init-shm()
  ;;;
  ;;; Structured Haskell Mode
  ;;;
  (add-to-list 'load-path "~/.emacs.d/structured-haskell-mode/elisp")
  (require 'shm)
  (add-hook 'haskell-mode-hook 'structured-haskell-mode)
  (setq shm-program-name "~/.emacs.d/structured-haskell-mode/dist/build/structured-haskell-mode/structured-haskell-mode")
  (setenv "PATH" (shell-command-to-string "echo $PATH"))
)

(defun myhaskell/init-haskell-mode()
  ;;;
  ;;; Stylish Haskell
  ;;;

  ;; Use Haskell-mode specific save
  (setq exec-path (append exec-path '("~/.cabal/bin")))

  ;; Automatic formatting with stylish-haskell
  (setq haskell-stylish-on-save t)
)
