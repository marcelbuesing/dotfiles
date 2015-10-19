(defvar mymain-pre-extensions
  '(
    ;; pre extension mymain go here
    )
  "List of all extensions to load before the packages.")

(defvar mymain-post-extensions
  '(
    ;; post extension mymains go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function mymain/init-<extension-mymain>
;;
;; (defun mymain/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
