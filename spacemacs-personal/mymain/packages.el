(defvar mymain-packages
  '(
    ;; package mymain go here
    multiple-cursors
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar mymain-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function mymain/init-<package-mymain>
;;
(defun mymain/init-mymain-mode()

  (use-package multiple-cursors-mode)
  

  ;;;
  ;;; multiple-cursors
  ;;;
  (global-set-key (kbd "M-+") 'mc/mark-next-like-this)
  (global-set-key (kbd "M-#") 'mc/mark-all-like-this)
)
