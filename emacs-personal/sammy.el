;;; Sammy.el --- summary
;;; Commentary:

;;; Ensure all the modules are available, excluding structured-haskell-mode.
(prelude-require-packages '(paredit cljsbuild-mode solarized-theme))

;; No flyspell anymore (for now)
(setq prelude-flyspell nil)

;; Use solarized
(load-theme 'solarized-dark t)

;;; Structured Haskell mode

;; Load SHM properly
(add-to-list 'load-path "/home/sammy/git/structured-haskell-mode/elisp")
(require 'shm)
(add-hook 'haskell-mode-hook 'structured-haskell-mode)

;; Configure SHM executable location
(setq shm-program-name
      "/home/sammy/git/structured-haskell-mode/dist/build/structured-haskell-mode/structured-haskell-mode")

;; Configure SHM colors for solarized-dark
(set-face-background 'shm-current-face "#073642") ; solarized-base02

;;; Paredit
(add-hook 'clojure-mode-hook 'paredit-mode)

(provide 'sammy)
;;; sammy.el ends here
