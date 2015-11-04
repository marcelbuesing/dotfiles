(defvar mymain-packages
  '(
    ;; package mymain go here
    multiple-cursors
    magit
    restclient
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar mymain-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function mymain/init-<package-mymain>
;;
(defun mymain/init-multiple-cursors()
  ;;;
  ;;; multiple-cursors
  ;;;
  (global-set-key (kbd "M-+") 'mc/mark-next-like-this)
  (global-set-key (kbd "M-#") 'mc/mark-all-like-this)
)

(defun mymain/init-magit()
  (global-magit-file-mode)
  )

(defun mymain/init-restclient()
  )
