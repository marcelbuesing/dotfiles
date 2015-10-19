(defvar myhaskell-pre-extensions
  '(
    ;; pre extension myhaskell go here
    )
  "List of all extensions to load before the packages.")

(defvar myhaskell-post-extensions
  '(
    ;; post extension myhaskells go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function myhaskell/init-<extension-myhaskell>
;;
;; (defun myhaskell/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
