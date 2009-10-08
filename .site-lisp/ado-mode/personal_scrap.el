;; Tell emacs to auto-load the ado-mode, so that it is available 
(load-library "ado-mode") 

;;   this gets used when making help files. 
(setq ado-claim-name "Bill Rising") 

;; Copy the ado-signature-template to a location of your choosing, 
;;   change the information, and store it in the file like .ado-signature 
;; Once again, this is definitely for individuals. 
(setq ado-signature-file "~/Custom/emacs/.ado-signature") 

;;; Optional but Highly Recommended Additions 

;; Tell the ado-mode the directory which will hold new, untested Stata programs. 
;; It should be a directory on your adopath within Stata, so that Stata finds
;;  your new ados.
(setq ado-new-dir "~/Custom/Stata/ado/new/") 

;; A directory to store good value labels. This could be shared across a group
;;; This should also be in your adopath within Stata
(setq ado-label-dir "~/Custom/Stata/labels/") 

;;; Optional Additions for the Person Who Wants to Alter the ado-mode 
;;;    Needed if you ever want to edit the source for the ado-mode (ado-mode.el), 
;;;   and have it byte-compile properly. 
;;;

;; Autoload the make-regexp and make-regexps functions for easier changes to the auto-highlighting 
(autoload 'make-regexp "make-regexp" 
"Return a regexp to match a string item in STRINGS.") 
(autoload 'make-regexps "make-regexp" 
"Return a regexp to REGEXPS.") 



