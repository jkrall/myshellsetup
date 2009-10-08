;;; Mandatory additions --- you must add these to get things working properly 

;; Tell emacs where the ado-mode is
;;  Change /Universal/Custom/emacs to the place which actually holds the ado-mode
;;  folder. Leave the /lisp at the end of the path!
(setq load-path (cons "/Universal/Custom/emacs/ado-mode/lisp" load-path))

;; The following line turns on maximum context-sensitive highlighting whenever 
;; you open *any* file, Stata-related or not 
(cond ((fboundp 'global-font-lock-mode) 
       ;; turn on font-locking everywhere (why not?)
       (global-font-lock-mode t) 
       ;; turn on maximum decoration
       (setq font-lock-maximum-decoration t))) 

;; Tell the ado-mode where the templates for new ado-files and other pieces can be found. 
;; These should be globally set for a group, since ado-mode needs some of the files to work properly.
;; You will need to change the directory name to the place where you put the templates folder 
(setq ado-site-template-dir "/Universal/Custom/emacs/ado-mode/templates/") 

