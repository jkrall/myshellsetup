
;; -*- Mode: Emacs-Lisp -*-
;; -*- lisp -*-
;---------------------------------------------------------------------
; .emacs file composed with help from various sources by
; Marius Sundbakken <marius@fix.no> &
; Jan "Amos" Borsodi <jb@ez.no>
;---------------------------------------------------------------------
; Site lisp files are collect from news:comp.emacs, news:gnu.emacs.sources and
; various web sites.
; Project functions by Jan Borsodi

;; TODO:
;; Lookup word at cursor pos to check if it is a class, if it is open header file.
;; Create a variable with params for a known class
;; Auto update from internet
;; Faster auto include
;; Abstract the project functions to allow it to be used with more than just C++
;; Download external packages from news and web sites, parse them to find downloadable files,
;; then try to parse the files to find instructions for starting them.
;; Select auto insert directory depending on mode and project style.
;; Session management for projects.
;; Support for a global .emacs as well as a local.
;; Change the way the keys are set, find a more consistent way of setting them, also
;; make it possible to revert this changes.
;; Make good comments on all my functions and reorganize them in more files/packages, yikes!
;; Automaticly byte-compile all external packages on startup.
;; Find a good name.

;; Changelog
;; 0.7.7
;; Internal changes to make it run on XEmacs (again :), and it seems to work with NT-Emacs too,
;;  unfortunately a lot of things are disabled in XEmacs and NT-Emacs at the moment.
;; ibuffer in options menu
;; Uncomment comments with Alt u
;; M-d to set British dictionary
;; M-a to set American dictionary
;; M-n to set Norwegian dictionary
;; M-p to let flyspell check the buffer
;; The option file is saved to .xemacs-options if XEmacs is used.
;; More auto include classes, including most new Qt classes from 2.2.0
;; Added "Create Custom Files" to project menu, this allows for creating customized autoinsert files
;;  in the project directory amongst other things. Put yourself in a directory and choose 
;;  "Create Custom Files" from the menu, you will then get a directory called .autoinsert which is
;;  a copy of your standard autoinsert directory, edit this files to get custom looks on a specific
;;  project. You will also get a file called .emacs-custom and .emacs-vars, edit these files to suit
;;  your needs.
;; New autoinsert file for PHP coding.
;; Updated PHP mode which can handle "foreach" better.
;; 0.7.6
;; Lots of internal changes to make it run on XEmacs too.
;; 0.7.5
;; More robust initialization, will not fail to init if some packages are missing
;; More flexible autoinsert mechanism, will look for the auto-insert directory in a more dynamic matter
;; dependening on the state.
;; New configuration system, you can now save a global and local config file of how you want
;; emacs too be, you can also specify which packages to start. More options will come later.
;; 0.7.4-2
;; Added newer versions of JDE and PSGML, as well as Speedbar and Semantic.
;; 0.7.4
;; * Menuitem for recently opened files
;; * Rectangular marking with highlight and mouse support
;; * Revive mode, (enhanced desktop mode), saves window properties as well.
;; * History saving, now saves a lot of history buffers on exit and loads them on init
;; * Better scrolling when using page-up and page-down, the cursor now stays in place
;; * Font menu moved to Shift Right Mouse
;; 0.7.3
;; * Highlight current line mode added and updated color styles to set the color
;; * Auto revert mode, automaticly reverts buffers every 5 seconds.
;; * Cursor blinking
;; * Crontab mode
;; 0.7.2
;; * CSS mode included and bound to .css files

;; Add this later
; (defun my-c-mode-common-hook ()
;   ;; my customizations for all of c-mode and related modes
;   (c-set-style "stroustrup")
;   ;; set auto cr mode
;   (c-toggle-auto-hungry-state t)

;   ;; qt keywords and stuff ...
;   ;; set up indenting correctly for new qt kewords (one line)
;   (setq c-C++-access-key "\\<\\(signals\\|public\\|protected\\|private
;   \\|public slots\\|protected slots\\|private slots\\)\\>[ \t]*:")
;   ;; modify the colour of slots to match public, private, etc ...
;   (font-lock-add-keywords 'c++-mode
;   '(("\\<\\(slots\\|signals\\)\\>" . font-lock-type-face)))
;   ;; make new font for rest of qt keywords
;   (make-face 'qt-keywords-face)
;   (set-face-foreground 'qt-keywords-face "green")
;   ;; qt keywords
;   (font-lock-add-keywords 'c++-mode
;   '(("\\<Q_OBJECT\\>" . 'qt-keywords-face)))
;   (font-lock-add-keywords 'c++-mode
;   '(("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)))
;   (font-lock-add-keywords 'c++-mode
;   '(("\\<Q[A-Z][A-Za-z]*" . 'qt-keywords-face)))
;   )
; (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; This emacs config will load these local files automagicly if they are found in
;; the current directory:
;; .emacs-vars          - Override global variables here
;; .emacs-classes	- You can add your own classes here (see .emacs-d-classes for example
;; .emacs-colors	- Override global colors here
;; .emacs-extras	- Add extra code and customization here
;; Since the files loaded are lisp files you can almost anything there but I advice only to do
;; what the file name says.

;; Add .site-lisp and site-lisp in home directory to load path
;; allows people to add emacs files if access to the global site-lisp is not possible
(setq load-path (nconc '( "~/.myshellsetup/.site-lisp" ) load-path ))

;; Various extra packages, they are more than one file and thus needs a separate directory
;; (setq load-path (nconc '( "/usr/share/emacs/site-lisp/gnus" ) load-path ))
(setq load-path (nconc '( "/usr/local/share/emacs/22.0.97/site-lisp/gnus" ) load-path ))
(setq load-path (nconc '( "/usr/local/share/emacs/22.0.97/site-lisp" ) load-path ))
(setq load-path (nconc '( "/usr/local/share/emacs/22.0.97/lisp" ) load-path ))
(setq load-path (nconc '( "/usr/local/share/emacs/22.0.97/lisp/emulation" ) load-path ))
(setq load-path (nconc '( "~/.myshellsetup/.site-lisp/gnus" ) load-path ))

(setq load-path (nconc '( "/usr/share/emacs/site-lisp/semantic" ) load-path ))
(setq load-path (nconc '( "~/.myshellsetup/.site-lisp/semantic" ) load-path ))

(setq load-path (nconc '( "/usr/share/emacs/site-lisp/speedbar" ) load-path ))
(setq load-path (nconc '( "~/.myshellsetup/.site-lisp/speedbar" ) load-path ))

(setq load-path (nconc '( "/usr/share/emacs/site-lisp/psgml" ) load-path ))
(setq load-path (nconc '( "~/.myshellsetup/.site-lisp/psgml" ) load-path ))

(setq load-path (nconc '( "/usr/share/emacs/site-lisp/jde/lisp" ) load-path ))
(setq load-path (nconc '( "~/.myshellsetup/.site-lisp/jde/lisp" ) load-path ))

;; Load the emacs type verifier first
(require 'emacs-type)

;; make compiler in a new frame.
(setq special-display-buffer-names
      (cons "*compilation*" special-display-buffer-names))

(defun yank-and-forward-line ()
  (interactive)
  (let ((old-col (current-column)))
    (yank)
    (forward-line)
    (while (and (not (eolp)) (> old-col 0))
      (forward-char)
      (setq old-col (1- old-col)))))
(global-set-key "\C-t" 'yank-and-forward-line)

;; Switches between source/header files
(defun toggle-source-header()
  "Switches to the source buffer if currently in the header buffer and vice versa."
  (interactive)
  (let ((buf (current-buffer))
	(name (file-name-nondirectory (buffer-file-name)))
	file
	offs)
    (setq offs (string-match c++-header-ext-regexp name))
    (if offs
	(let ((lst c++-source-extension-list)
	      (ok nil)
	      ext)
	  (setq file (substring name 0 offs))
	  (while (and lst (not ok))
	    (setq ext (car lst))
	    (if (file-exists-p (concat file "." ext))
		  (setq ok t))
	    (setq lst (cdr lst)))
	  (if ok
	      (find-file (concat file "." ext))))
      (let ()
	(setq offs (string-match c++-source-ext-regexp name))
	(if offs
	    (let ((lst c++-header-extension-list)
		  (ok nil)
		  ext)
	      (setq file (substring name 0 offs))
	      (while (and lst (not ok))
		(setq ext (car lst))
		(if (file-exists-p (concat file "." ext))
		    (setq ok t))
		(setq lst (cdr lst)))
	      (if ok
		  (find-file (concat file "." ext)))))))))


;; Made by Joe Casadonte (joc)
(defun joc-bounce-sexp ()
  "Will bounce between matching parens just like % in vi"
  (interactive)
  (let ((prev-char (char-to-string (preceding-char)))
	(next-char (char-to-string (following-char))))
	(cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
		  ((string-match "[\]})>]" prev-char) (backward-sexp 1))
		  (t (error "%s" "Not on a paren, brace, or bracket")))))

(global-set-key [(control =)] 'joc-bounce-sexp)

(defun make-makefile()
  "Creates the Makefile from the .pro project file"
  (interactive)
  (let ((project (project-main)))
    (shell-command (concat "tmake -o Makefile "
			   (file-name-nondirectory (buffer-file-name project))))))

;; Finds all functions in a class in a given buffer
;; Creates a list which looks like this.
;; ((classname) func1 func2 ...)
;; where func1 looks like
;; (type1 type2 type3 reference name args)
(defun find-class-functions ( buf )
  (interactive)
  (save-excursion
    (set-buffer buf)
    (beginning-of-buffer)
;;    (message "searching")
    (if (search-forward-regexp (concat "^\\(template[ \t]*<[^>]+>[ \t]*\\)?class[ \t]+\\([a-zA-Z0-9_]+\\)[ \t\n]*"
				       "\\([:][ \t\n]*\\(public\\|protected\\|private\\)?[ \t\n]*\\<[a-zA-Z0-9_]+\\>\\)?"
				       "[ \t\n]*{") nil t)
	(let (start
	      stop
	      (name (match-string-no-properties 2)))
;;	  (message "found first")
	  (setq start (match-end 0))
	  (if ( search-forward "};" nil t)
	      (let ((funclist '()))
		(setq stop (match-beginning 0))
;;		(message "found second")
		(save-restriction
		  (narrow-to-region start stop)
		  (beginning-of-buffer)
		  (while (search-forward-regexp (concat
						 "\\(\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]+\\)?"
						 "\\(\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]+\\)?"
						 "\\(\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]+\\)?"
						 "\\([*&]?\\)[ \t]*"
						 "\\([~]?[a-zA-Z][a-zA-Z0-9_]*\\)[ \t]*"
						 "(\\([^\)]*\\))[ \t]*;")
						nil t)
		    (let (
			  (type1 (match-string-no-properties 2))
			  (type2 (match-string-no-properties 4))
			  (type3 (match-string-no-properties 6))
			  (ref (match-string-no-properties 7))
			  (name (match-string-no-properties 8))
			  (args (match-string-no-properties 9)))
		      (setq funclist (cons (list type1 type2 type3 ref name args) funclist))
		    ))
		  (setq funclist (cons (list name) funclist ))
		  funclist)))))))

;; Removes virtual and static from a type
(defun string-remove-type ( str reg )
  (interactive)
  (let (tmp)
    (if (eq str nil)
	(setq tmp "")
      (if (string-match "\\(virtual\\|static\\)" str)
	  (setq tmp "")
	(if reg
	    (setq tmp (concat str "[ \t]+"))
	  (setq tmp (concat str " ")))))
    tmp))

;; Finds all functions in this buffers class and adds the one's missing from
;; the source file.
(defun expand-class-functions ( buf )
  (interactive)
  (if (string-match c++-header-ext-regexp (buffer-name buf))
      (save-excursion
	(set-buffer buf)
	(let ((lst (find-class-functions buf))
	      item
	      classname)
	  (toggle-source-header)
	  (beginning-of-buffer)
	  (setq classname (car (car lst)))
	  (setq lst (cdr lst))
	  (let (type1 type1_reg
		      type2 type2_reg
		      type3 type3_reg
		      ref ref_reg
		      name
		      args args_reg)
	    (setq lst (nreverse lst))
	    (while lst
	      (setq item (car lst))
	      (setq type1 (car item))
	      (setq item (cdr item))
	      (setq type2 (car item))
	      (setq item (cdr item))
	      (setq type3 (car item))
	      (setq item (cdr item))
	      (setq ref (car item))
	      (setq item (cdr item))
	      (setq name (car item))
	      (setq item (cdr item))
	      (setq args (car item))
	      (setq item (cdr item))
	      (setq type1_reg (string-remove-type type1 t))
	      (setq type2_reg (string-remove-type type2 t))
	      (setq type3_reg (string-remove-type type3 t))
	      (if (eq ref nil)
		  (setq ref_reg "[ \t]*")
		(setq ref_reg (concat "[" ref "]" "[ \t]*")))
	      (setq args_reg (regexp-opt (list args)))
	      (beginning-of-buffer)
	      (if (search-forward-regexp (concat "^" type1_reg type2_reg type3_reg ref_reg
						 classname "::" name "[ \t]*" "(\\([^)]+\\))" ) nil t)
		  (let (args_org
			args_new
			(offs_org 0) (len_org 0)
			(offs_new 0) (len_new 0)
			type1 type2 type3 ref
			(all t)
			(args_reg "\\(\\([a-zA-Z]+\\)[ \t]+\\)?"
				  "\\(\\([a-zA-Z]+\\)[ \t]+\\)?"
				  "\\(\\([a-zA-Z]+\\)[ \t]+\\)?"
				  "\\([&*]\\)?[ \t]*\\([a-zA-Z_][a-zA-Z_]*\\)?\\([ \t]*=[^,]+\\)?"))
		    (setq args_new (match-string 1))
		    (yes-or-no-p "match")
		    (while (and (not offs_org) (not offs_new))
		      (setq offs_org (string-match args_reg args offs_org))
		      (setq type1 (substring args (match-beginning 2) (match-end 2)))
		      (setq type2 (substring args (match-beginning 4) (match-end 4)))
		      (setq type3 (substring args (match-beginning 6) (match-end 6)))
		      (setq ref (substring args (match-beginning 7) (match-end 7)))
;;		      (setq offs_new (string-match args_reg args_new offs_new))
		      (yes-or-no-p (concat type1 ":" type2 ";" type3 ":" ref ))))
		(save-excursion
		  ;;		(message (concat "^" type1_reg type2_reg type3_reg ref_reg
		  ;;				 classname "::" name "[ \t]*" "(" args_reg ")" ))
		    (end-of-buffer)
		    (setq type1_reg (string-remove-type type1 nil))
		    (setq type2_reg (string-remove-type type2 nil))
		    (setq type3_reg (string-remove-type type3 nil))
		    (if (eq ref nil)
			(setq ref_reg "")
		      (setq ref_reg (concat ref)))
		    (if (not (bolp))
			(insert-string "\n"))
					;		(yes-or-no-p "h")
		    (insert-string (concat "\n/*!\n  \n*/\n\n"
					   type1_reg type2_reg type3_reg ref_reg
					   classname "::" name "(" args ")"
					   "\n{\n}\n"))))
	      (setq lst (cdr lst))
	      ))))))

;; Will make sure that the #ifndef and #define that should be present in all c/c++ headers
;; are corrected according their filename.
(defun correct-c-header-define( buf )
  (interactive "b")
  (save-excursion
    (set-buffer buf)
    (if (string-match c++-header-ext-regexp (buffer-name))
	(let ((bufname (buffer-name))
	      defname
	      defstring)
	  (setq defname (concat (upcase (file-name-sans-extension bufname)) "_"
				(upcase (file-name-extension bufname))))
	  (setq defstring (concat
			   "#ifndef " defname "\n"
			   "#define " defname "\n"))
	  (beginning-of-buffer)
	  (if (search-forward-regexp (concat "^#ifndef[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]*[\n]"
					     "#define[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]*[\n]")
				     nil t)
;;	      (if (string-equal (match-string 1) (match-string 2))
	      (replace-match defstring t t ))))))

;; New version, use later
; (defun correct-c-header-define( buf )
;   (interactive "b")
;   (save-excursion
;     (set-buffer buf)
;     (if (string-match "\\.\\(hpp\\|h\\|\hh\\|H\\)$" (buffer-name))
; 	(let* ((bufname (buffer-name))
; 	      (defname (concat (upcase (file-name-sans-extension bufname)) "_"
; 				(upcase (file-name-extension bufname))))
; 	      (defstring (concat
; 			   "#ifndef " defname "\n"
; 			   "#define " defname "\n"))
; 	      (endstring (concat "#endif // " defname)))
; 	  (beginning-of-buffer)
; 	  (if (search-forward-regexp (concat "^#ifndef[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]*[\n]"
; 					     "#define[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \t]*[\n]")
; 				     nil t)
; 	      (replace-match defstring t t)
; 	    (progn
; 	      (beginning-of-buffer)
; 	      (insert defstring)))
; 	  (if (search-forward-regexp "^#endif[ \t]*//[ \t]*[a-zA-Z_][a-zA-Z0-9_]*[ \t]*$" nil t)
; 	      (replace-match endstring t t)
; 	    (progn
; 	      (end-of-buffer)
; 	      (insert endstring)))))))


;; Tries to set the autoinsert directory, it first checks the wanted directory,
;; then the default, if all fails it sets it to nil.
(defun project-select-autoinsert (&optional dir)
  "Returns a valid directory to be used for autoinserting, if none is valid, nil is returned.

The order of directories tested goes as following:
First if a directory is passed as a param it is tested,
next the `project-auto-insert-directory' is tested
and finally the `default-auto-insert-directory'."
  (cond
   ((cond ((stringp dir) (file-exists-p dir)) (t nil)) dir)
   ((file-exists-p project-auto-insert-directory) project-auto-insert-directory)
   ((file-exists-p default-auto-insert-directory) default-auto-insert-directory)
   (t nil)))

(defun project-create-autoinsert-alist ()
  "Creates an autoinsert-alist which can be used to set the `auto-insert-alist'
it is automaticly created from the list specified in `project-autoinsert-alist'"
  (let (autoinsert-list
	(lst project-autoinsert-alist)
	item)
    (while lst
      (setq item (car lst))
      (setq autoinsert-list (append autoinsert-list (eval item)))
      (setq lst (cdr lst)))
    autoinsert-list))

;; Make sure revive resumes on init
(defun resume-try()
  "Tries to resume a buffer if the file exists and adds `save-current-configuration' if it is loaded"
  (if (file-exists-p revive:configuration-file)
      (let ()
	(resume)
	(add-hook 'kill-emacs-hook 'save-current-configuration))))

;; Make sure revive don't save it's configuration on exit when wiping
(defun wipe-try()
  "Wipes the revive configuration and removes the `save-current-configuration' from the exit hook"
  (wipe)
  (if (file-exists-p revive:configuration-file)
      (delete-file revive:configuration-file))
  (remove-hook 'kill-emacs-hook 'save-current-configuration))

;; Will align c/c++ variable declarations in the selected region
;; Example:
;; int a;
;; const QString b;
;; static unsigned int c;
;;
;; will become:
;; int                 a;
;; const QString       b;
;; static unsigned int c;
(defun align-vars(beg end)
  "Aligns c/c++ variable declaration names on the same column, with beginning and end taken from selected region."
  (interactive "r")
  (save-excursion
    (let (bol eol expr-end
	  (max-col 0) col
	  poslist curpos)
      (goto-char end)
      (if (not (bolp))
	  (setq end (line-end-position)))
      (goto-char beg)
      (while (and (> end (point)) (not (eobp)))
	(setq bol (line-beginning-position))
	(setq eol (line-end-position))
	(beginning-of-line)
	(setq expr-end (point))
	(if (search-forward-regexp "^[^/][^/]\\([a-zA-Z][a-zA-Z]*\\)[ \t]+[^;]" eol t)
	    (let ()
	      (setq expr-end (match-end 1))
	      (while (search-forward-regexp "\\([a-zA-Z][a-zA-Z]*\\)[ \t]+[^;]" eol t)
		(setq expr-end (match-end 1)))
	      (goto-char expr-end)
	      (setq col (current-column))
	      (if (search-forward-regexp (concat "\\(\\*\\|&[ \t]*\\)?"
						 "\\([a-zA-Z\\_][a-zA-Z0-9\\_]*\\)\\([\[][0-9]+[\]]\\)?"
						 "\\([ \t]*,[ \t]*\\([a-zA-Z\\_][a-zA-Z0-9\\_]*\\)\\([\[][0-9]+[\]]\\)?\\)*"
						 "[ \t]*;$") eol t)
		  (let ((name-col-end 0))
		    (if (eq (match-beginning 2) (match-beginning 0))
			(setq name-col-end 1))
		    (setq poslist (cons (list expr-end col (match-beginning 0) name-col-end) poslist))
		    (if (> col max-col)
			(setq max-col col))
		    (beginning-of-next-line))
		(beginning-of-next-line)))
	  (beginning-of-next-line)))
      (setq curpos poslist)
      (while curpos
	(let* ((pos (car curpos))
	       (col (car (cdr pos)))
	       (col-end (car (cdr (cdr pos))))
	       (col-end-name (car (cdr (cdr (cdr pos)))))
	       (abs-pos (car pos)))
	  (goto-char abs-pos)
	  (delete-region abs-pos col-end)
	  (insert-string (make-string (+ (+ (- max-col col) 1) col-end-name) 32)))
	(setq curpos (cdr curpos))))))

;; Use the align package from ... instead
(if (emacs-type-is-regular)
    (require 'align nil t)
  (require 'align))

;; Aligns all variable declarations in this buffer
(defun align-vars-buffer()
  "Aligns c/c++ variable declaration names on the same column in this buffer."
  (interactive)
  (save-excursion
    (let (beg end)
      (beginning-of-buffer)
      (setq beg (point))
      (end-of-buffer)
      (setq end (point))
      (align-vars beg end))))

;; Jump to beginning of the next line if possible.
(defun beginning-of-next-line()
  "Moves cursor to the beginning of the next line, or nowhere if at end of the buffer"
  (interactive)
  (end-of-line)
  (if (not (eobp))
      (forward-char 1)))

;; Load default variables
(if (file-exists-p "~/.myshellsetup/.emacs-d-vars")
    (load-file "~/.myshellsetup/.emacs-d-vars"))

;; Load the keys file
(if (file-exists-p "~/.myshellsetup/.emacs-d-keys")
    (load-file "~/.myshellsetup/.emacs-d-keys"))

;; Load local variables
(if (file-exists-p ".emacs-vars")
    (load-file ".emacs-vars"))



(defun option-save-to-file (&optional file) 
  "Save all options from `option-save-alist' to `option-config-file',
each list item is evaluated and the result added to the save buffer"
  (let* ((old-buffer (current-buffer))
	(loadfile (cond
		   (file file)
		   (t option-config-file)))
        (optionbuffer (find-file-noselect loadfile t t))
	(varlst nil))
    (switch-to-buffer optionbuffer)
    (if (> (buffer-size) 0)
        (delete-region 1 (buffer-size)))
    (insert (concat ";; -*- Mode: Emacs-Lisp -*-\n;; -*- lisp -*-\n"
		    ";---------------------------------------------------------------------\n"
		    ";; This file is automaticly generated, please do not modify\n"
		    ";; Version "
		    (option-config-version) "\n"))
    (insert "(progn")
    (mapcar (lambda (x)
              (if x
		  (if (listp x)
		      (progn
			(insert "\n  ")
			(insert (eval x)))
		    (setq varlst (cons x varlst)))))
            option-save-alist)
    (insert ")\n\n")
    (if varlst
	(progn
	  (insert "(setq")
	  (mapcar (lambda (x)
		    (if x
			(if (boundp x)
			  (progn
			    (insert "\n ")
			    (prin1 x optionbuffer)
			    (insert " '")
			    (prin1 (eval x) optionbuffer)))))
		  varlst)
	  (insert ")\n")))
    (option-config-validate)
    (basic-save-buffer)
    (kill-buffer optionbuffer)
    (switch-to-buffer old-buffer)))

(defun option-save-to-file-locally ()
  "Save all options from `option-save-alist' to `option-local-config-file',
each list item is evaluated and the result added to the save buffer"
  (option-save-to-file (concat (option-local-config-dir) option-local-config-file)))

(defun option-load-from-file (&optional file)
  "Load all options from `option-config-file' and evaluate them."
  (let ((loadfile (cond
		   (file file)
		   (t option-config-file))))
    (option-config-validate)
    (if (file-exists-p loadfile)
	(progn
	  (load-file loadfile)
	  t)
      nil)))

(defun option-local-config-dir ()
  (if option-local-config-dir-func
      (eval option-local-config-dir-func)
    "./"))

(defun project-local-config-dir ()
  "./")
;   (let ((main (project-main)))
;     (file-name-directory (buffer-file-name main))))

(defun option-load-from-file-locally ()
  "Load all options from `option-local-config-file' and evaluate them."
  (option-load-from-file (concat (option-local-config-dir) option-local-config-file)))
      
(defun option-config-validate ()
  "Makes sure the `option-config-dirty' flag is cleared (nil)."
  (setq option-config-dirty nil))

(defun option-config-invalidate ()
  "Makes sure the `option-config-dirty' flag is set (t),
call this if you have changed an option which is to be saved in the `option-config-file' or `option-local-config-file'."
  (setq option-config-dirty t))

(defun option-line-smooth-scroll()
  "Returns a string, dependening on wheter `option-smooth-scroll' is enabled,
which is stored by `option-save-to-file'."
  (let (tmp)
    (if option-smooth-scroll
	(setq tmp (concat "(option-smooth-scroll-enable t)"))
      (setq tmp (concat "(option-smooth-scroll-enable nil)")))
    tmp))

(defun option-smooth-scroll-enable (enable)
  "Turns on smooth keyboard scrolling if ENABLE is non-nil and updates `option-smooth-scroll'."
  (if enable
      (setq scroll-margin '0
	    scroll-step '1)
    (setq scroll-margin '2
	  scroll-step '1))
  (option-config-invalidate)
  (setq option-smooth-scroll enable))

(defun option-smooth-scroll-toggle ()
  "Toggles smooth keyboard scrolling, see `option-smooth-scroll-enable' for information."
  (option-smooth-scroll-enable (not option-smooth-scroll)))

(defun option-load-package (pkg)
  "Tries to load a specific package and set a flag it succeeds,
PKG contains (NAME AVAIL LOADED INIT EXIT), where
NAME is the name of the package,
AVAIL is the name of the availability variable,
and LOADED is the name of the loaded variable."
  (let ((avail (eval (cadr pkg)))
	(name-var (car pkg))
	(load-var (caddr pkg)))
    (if avail
	(if (if (emacs-type-is-regular)
		(require name-var nil t)
	      (require name-var))
 	    (progn
 	      (set load-var 't)
 	      (if (cadr (cdr (cdr pkg)))
		  (eval (cadr (cdr (cdr pkg)))))
	      t)
	  nil)
      nil)))

(defun option-unload-package (pkg)
  "Tries to unload a specific package and set a flag it succeeds,
PKG contains (NAME AVAIL LOADED INIT EXIT), where
NAME is the name of the package,
AVAIL is the name of the availability variable,
and LOADED is the name of the loaded variable."
  (let ((avail (eval (cadr pkg)))
	(name-var (car pkg))
	(load-var (caddr pkg)))
    (if avail
	(progn
	  (set load-var 'nil)
	  (if (cadr (cdr (cdr (cdr pkg))))
	      (eval (cadr (cdr (cdr (cdr pkg))))))
	  t)
      nil)))


(defun option-check-packages ()
  (let ()
    (mapcar (lambda (x)
	      (option-check-package (eval x)))
	    option-package-available-alist)))

(defun option-check-package (pkg)
  (let ((dirlst load-path)
	(found nil)
	file
	(avail (cadr pkg))
	(name (car pkg)))
    (while (and dirlst (not found))
    (setq file (concat (car dirlst) "/" (prin1-to-string name) ".el"))
    (if (file-exists-p file)
	(setq found t))
    (setq dirlst (cdr dirlst)))
    (set avail found)
    found))

(defun option-package-start-rect-mark ()
  (autoload 'rm-set-mark "rect-mark"
    "Set mark for rectangle." t)
  (autoload 'rm-exchange-point-and-mark "rect-mark"
    "Exchange point and mark for rectangle." t)
  (autoload 'rm-kill-region "rect-mark"
    "Kill a rectangular region and save it in the kill ring." t)
  (autoload 'rm-kill-ring-save "rect-mark"
    "Copy a rectangular region to the kill ring." t)
  (autoload 'rm-mouse-drag-region "rect-mark"
    "Drag out a rectangular region with the mouse." t)
  (option-enable-keys 'option-keys-rect-mark-alist))

(defun option-package-end-rect-mark ()
  (option-disable-keys 'option-keys-rect-mark-alist))

(defun option-package-start-blank-mode ()
  (blank-mode-on))

(defun option-package-end-blank-mode ()
  (blank-mode-off))

(defun option-package-start-revive ()
  (autoload 'save-current-configuration "revive" "Save status" t)
  (autoload 'resume "revive" "Resume Emacs;; " t)
  (autoload 'wipe "revive" "Wipe Emacs" t)
  ;; Make sure the configuration is saved in a local directory
  (setq revive:configuration-file ".revive.el")
  ;; This is needed to avoid that the save-history buffer is revived as well
  (setq revive:ignore-buffer-pattern "^\\( \\*\\)\\|\\(\\.emacs-histories\\)")
  ;; Save on exit is optional, uncomment to always enable
  ;; (add-hook 'kill-emacs-hook 'save-current-configuration)
  (add-hook 'after-init-hook 'resume-try)
  (option-enable-keys 'option-keys-revive-alist))

(defun option-package-end-revive ()
  (remove-hook 'after-init-hook 'resume-try)
  (option-disable-keys 'option-keys-revive-alist))

(defun option-save-history-toggle ()
  "Toggles the automatic saving of history between sessions"
  (if option-save-history-flag
      (option-save-history-enable nil)
    (option-save-history-enable t)))

(defun option-save-history-enable (enable)
  (if enable
      (progn
	(add-hook 'after-init-hook 'save-history-load)
	(add-hook 'kill-emacs-hook 'save-history-save)
	(setq option-save-history-flag t)
	(option-config-invalidate))
    (progn
      (remove-hook 'after-init-hook 'save-history-load)
      (remove-hook 'kill-emacs-hook 'save-history-save)
      (setq option-save-history-flag nil)
      (option-config-invalidate))))

(defun option-enable-keys (keys)
  "Turns on all keys in the list,
the contents of KEYS is a list of MAPPINGs,
the first entry is used for storing the old keys,
each MAPPING looks like (KEYMAP KEY DEF)."
;; use (lookup-key) too find keys
  (let ((lst (eval keys)))
    (setq lst (cdr lst))
    (mapcar (lambda (x)
	      (define-key (eval (car x)) (cadr x) (caddr x)))
	    lst)))

(defun option-disable-keys (keys)
  "Turns off all keys in the list,
the contents of KEYS is a list of MAPPINGs,
the first entry is used for storing the old keys,
each MAPPING looks like \(KEYMAP KEY DEF\)."
  (mapcar (lambda (x)
	    (define-key (eval (car x)) (cadr x) nil))
	  (cdr (eval keys))))

(defun option-popup-contents ()
  "Creates the content for a popup menu,
the popup menu is used to inform the user of the first time use."
  (list (concat "          This seems to be your first time
      running version " (option-config-version) " of the emacs config.
           Most options are now turned off.
 Do you wish to save a default global configuration?
\(You can change the default options by changing items
in the Options menu and then selecting save global\)") '("OK" . t) '("Cancel" . nil)))

; Try to load the default global configuration file, if it fails ask
; the user what to do.
; The console variant must be implemented as well as NT and Mac specifics
(if (not (option-load-from-file))
    (let ((cont (option-popup-contents))
	  (type (emacs-type)))
      (if (cond
	   ((eq type 'emacs-window)
	    (x-popup-dialog t cont))
	   ((eq type 'xemacs-window)
	    (get-dialog-box-response t cont)))
	  (option-save-to-file))))

; ;; Try to load the default global configuration file, if it fails ask
; ;; the user what to do.
; (if (not (option-load-from-file))
;     (if (x-popup-dialog t (list (concat "          This seems to be your first time
;       running version " (option-config-version) " of the emacs config.
;            Most options are now turned off.
;  Do you wish to save a default global configuration?
; \(You can change the default options by changing items
; in the Options menu and then selecting save global\)") '("OK" . t) '("Cancel" . nil)))
; 	(option-save-to-file)))

;; Next try the local configuration file.
(option-load-from-file-locally)

(option-check-packages)

;; Load the customiziation file
(if (file-exists-p "~/.myshellsetup/.emacs-d-custom")
    (load-file "~/.myshellsetup/.emacs-d-custom"))

;; Load the local customiziation file
(if (file-exists-p ".emacs-custom")
    (load-file ".emacs-custom"))


(defun project-add-include-classes (classnames classinclude)
  "Adds an object-include connection to the projects list"
  (let ()
    (setq project-include-classes (cons (list classnames classinclude) project-include-classes))))

(defun project-add-include-list (classes)
  "Adds a list of object-include to the projects list"
  (let ((inc-classes classes)
	(class))
    (while inc-classes
      (setq class (car inc-classes))
      (project-add-include-classes (car class) (car (cdr class)))
      (setq inc-classes (cdr inc-classes)))))




;; Load default classes
(if (file-exists-p "~/.myshellsetup/.emacs-d-classes")
    (load-file "~/.myshellsetup/.emacs-d-classes"))

;; Load local classes
(if (file-exists-p ".emacs-classes")
    (load-file ".emacs-classes"))

;; Returns the end of the include area, finds the end of the top comment and adds a newline if no includes
;; are present.
(defun end-of-include-place()
  "Finds the end of the includes, or the end of the top comments if no includes are present."
  (let ((pos))
    (save-excursion
      (beginning-of-buffer)
      (let ((count 0))
	(while (search-forward-regexp "^#include[ \t]+[\"<][a-zA-Z0-9\.\-\_]+[\">][ \t]*\n" nil t)
	  (setq count (1+ count)))
	(if (< count 1)
	    (let ()
	      (if (string-match c++-header-ext-regexp (buffer-name))
		  (let (name)
		    (setq name (concat "#ifndef[ \t]+"
;;				       (upcase (file-name-sans-extension (buffer-name)))
				       "[^ ^\t^\n]*"
;;				       "_"
;;				       (upcase (file-name-extension (buffer-name)))
				       "[ \t]*\n"
				       "#define[ \t]+"
;;				       (upcase (file-name-sans-extension (buffer-name)))
				       "[^ ^\t^\n]*"
;;				       "_"
;;				       (upcase (file-name-extension (buffer-name)))
				       "[ \t]*\n"))
;;		    (message name)
		    (search-forward-regexp name nil t))
		(let ()
		  (beginning-of-buffer)
		  (search-forward-regexp "\\(\\(\\(//[^\n]*\n\\)\\|\\(/\\*[^\\*]*\\*/[^\n]*\n\\)\\)*\\)[ \t]*\n")
		  (goto-char (match-end 1))))
	      (insert-string "\n"))))
      (setq pos (point)))
    pos))

;; Checks for known classes and adds includes on the top if none are present
;(defun insert-include( buffer buf )
(defun insert-include()
  "Insert #include on the top of the file if certain class names are found in the file"
  (interactive)
  (if (string-equal mode-name "C++")
      (let ((includes project-include-classes)
	    (include)
	    (include-classes)
	    (include-class)
	    (include-file)
	    (class-exists nil))
	(while includes
	  (setq include (car includes))
	  (setq include-classes (car include))
	  (setq include-file (car (cdr include)))
	  (setq class-exists nil)
	  (while (and (not class-exists) include-classes)
	    (setq include-class (car include-classes))
	    (save-excursion
	      (beginning-of-buffer)
	      (if (search-forward-regexp (concat "\\<" include-class "\\>") nil t)
		  (setq class-exists t)))
	    (setq include-classes (cdr include-classes)))
	  (if class-exists
	      (let ((already-present nil))
		(save-excursion
		  (beginning-of-buffer)
		  (if (search-forward-regexp (concat "^#include[ \t]+"
						     include-file
						     "[ \t]*\n") nil t )
		      (setq already-present t)))
		(if (not already-present)
		    (save-excursion
		      (goto-char (end-of-include-place))
		      (insert-string (concat "#include " include-file "\n"))))))
	  (setq includes (cdr includes))))))

(defun project-looking-at-include()
  (save-excursion
    (let ((ok nil))
      (beginning-of-line)
      (if (looking-at project-c++-include-regexp)
	  (setq ok t))
      ok)))

(defun project-looking-at-forward-class-decl()
  (save-excursion
    (let ((ok nil))
      (beginning-of-line)
      (if (looking-at project-c++-class-decl-regexp)
	  (setq ok t))
      ok)))

(defun project-find-include( class )
  (let ((classes project-include-classes)
	class-include
	class-list
	class-name
	include
	(done nil))
    (while (and classes (not done))
      (setq class-include (car classes))
      (setq class-list (car class-include))
      (message (cadr class-list))
      (while (and class-list (not done))
	(setq class-name (car class-list))
	(if (string-equal class-name class)
	    (setq done t
		  include (cadr class-include)))
	(setq class-list (cdr class-list)))
      (setq classes (cdr classes)))
    include))

;; Fix here
(defun project-try-open-include( include )
  (let (dir
	filename
	include-name
	class-name)
    (if (string-match "\"\\([^\"]*\\)\"" include)
	(let ()
	  (setq include-name (substring include (match-beginning 1) (match-end 1)))
	  (setq class-name (project-try-open-local-include include-name))
	  )
      (if (string-match "<\\([^>]*\\)>" include)
	  (let ()
	    (setq include-name (substring include (match-beginning 1) (match-end 1)))
	    (setq class-name (project-try-open-global-include include-name))
	    )
	))
    class-name))

(defun project-try-open-local-include( include-name )
  (let ((project (project-main))
	proj-dir
	inc-file
	classes
	class)
    (setq proj-dir (file-name-directory (buffer-file-name project)))
    (setq inc-file (concat proj-dir include-name))
    (setq classes (check-file inc-file))
    (if classes
	(setq class (car classes)))
    class))

(defun project-parse-tmake-line( var buf )
  (save-excursion
    (let (elements)
      (set-buffer buf)
      (beginning-of-buffer)
      (while (re-search-forward (concat "\\(.+:\\)?"
					var
					"[ \t]+[+*/-]?=\\([ \t]*[A-Za-z0-9/.]+\\)*") nil t)
	(setq elements (nconc elements (split-string (match-string-no-properties 2)))))
      elements)))

(defun project-find-include-paths( buf )
  (let (paths
	path
	(real-paths nil)
	proj-dir)
    (setq proj-dir (file-name-directory (buffer-file-name buf)))
    (setq paths (project-parse-tmake-line "TMAKE_INCDIR_QT" buf))
    (while paths
      (setq path (car paths))
      (setq real-paths (nconc real-paths (list (concat proj-dir path))))
      (setq paths (cdr paths)))
    real-paths))

(defun project-try-open-global-include( include-name )
  (let ((project (project-main))
	proj-dir
	inc-file
	classes
	(class nil)
	(paths project-include-paths)
	path)
    (setq proj-dir (file-name-directory (buffer-file-name project)))
    (setq paths (nconc paths (project-find-include-paths project)))
    (while (and paths (not class))
      (setq path (car paths))
      (setq path (substitute-in-file-name path))
      (setq inc-file (concat path "/" include-name))
      (if (file-exists-p inc-file)
	  (let ()
	    (setq classes (check-file inc-file))
	    (if classes
		(setq class (car classes)))))
      (setq paths (cdr paths)))
    class))

(defun project-find-class-in-include( include )
  (save-excursion
    (let (class-name)
      (setq class-name (project-find-class-in-classes include))
      (if (not class-name)
	  (setq class-name (project-try-open-include include)))
      class-name)))

(defun project-find-class-in-classes( include )
  (let ((classes project-include-classes)
	class-list
	class-names
	class-name
	class-include
	(done nil))
    (while (and classes (not done))
      (setq class-list (car classes))
      (setq class-include (cadr class-list))
      (if (string-equal class-include include)
	  (setq class-names (car class-list)
		done t))
      (setq classes (cdr classes)))
    (if done
	(setq class-name (car class-names)))
    class-name))

(defun project-convert-include()
  (save-excursion
    (let (include
	  class-name)
      (beginning-of-line)
      (if (looking-at project-c++-include-regexp)
	  (save-restriction
	    (narrow-to-region (match-beginning 0) (match-end 0))
	    (setq include (match-string 1))
	    (setq class-name (project-find-class-in-include include))
	    (if class-name
		(if (re-search-forward ".*\n")
		    (replace-match (concat "class " class-name ";\n"))
	      (message (concat "No class found for include " include)))))
	(if (looking-at project-c++-class-decl-regexp)
	    (save-restriction
	      (narrow-to-region (match-beginning 0) (match-end 0))
	      (setq class-name (match-string 1))
	      (setq include (project-find-include class-name))
	      (if include
		  (replace-match (concat "#include " include "\n"))
		(message (concat "Nothing known about " class-name))))
	  (message "Not a forward class declaration or include file"))))))

;;
(defun project-insert-params( class header body )
  "Insert params to a given class"
;  (interactive)
  (save-excursion
    (let ((includes project-include-params)
	  (include)
	  (include-classes)
	  (include-class)
	  (include-params-header)
	  (include-params-body)
	  (include-params)
	  (done nil)
	  (class-exists nil))
      (while (and includes (not done))
	(setq include (car includes))
	(setq include-classes (car include))
	(setq include-params-header (car (cdr include)))
	(setq include-params-body (car (cddr include)))
	(setq include-params (car (cdddr include)))
	(if (string-match include-classes class)
	    (save-excursion
	      (set-buffer header)
	      (save-excursion
		(beginning-of-buffer)
		(while (search-forward project-params-match nil t)
		  (save-restriction
		    (narrow-to-region (match-beginning 0) (match-end 0))
		    (replace-match (concat " " include-params-header " ")))))
	      (set-buffer body)
	      (save-excursion
		(beginning-of-buffer)
		(while (search-forward project-params-match nil t)
		  (save-restriction
		    (narrow-to-region (match-beginning 0) (match-end 0))
		    (replace-match (concat " " include-params-body " ")))))
	      (save-excursion
		(beginning-of-buffer)
		(while (search-forward project-params-init-match nil t)
		  (save-restriction
		    (narrow-to-region (match-beginning 0) (match-end 0))
		    (replace-match (concat " " include-params " ")))))
	      (setq done t)
	      ))
	(setq includes (cdr includes)))
      (if (not done)
	  (save-excursion
	    (set-buffer header)
	    (save-excursion
	      (beginning-of-buffer)
	      (while (search-forward project-params-match nil t)
		(save-restriction
		  (narrow-to-region (match-beginning 0) (match-end 0))
		  (replace-match ""))))
	    (set-buffer body)
	    (save-excursion
	      (beginning-of-buffer)
	      (while (search-forward project-params-match nil t)
		(save-restriction
		  (narrow-to-region (match-beginning 0) (match-end 0))
		  (replace-match ""))))
	    (save-excursion
	      (beginning-of-buffer)
	      (while (search-forward project-params-init-match nil t)
		(save-restriction
		  (narrow-to-region (match-beginning 0) (match-end 0))
		  (replace-match ""))))))
      )))


(defun check-file( file )
  (let (buf
	lst)
    (setq buf (generate-new-buffer "class-tmp"))
    (save-excursion
      (set-buffer buf)
      (insert-file-contents-literally file)
      (while (search-forward-regexp (concat "class[ \t\n]+\\([a-zA-Z][a-zA-Z0-9_]*[ \t\n]+\\)?\\([a-zA-Z][a-zA-Z0-9_]*\\)[ \t\n]*"
					    "\\(:[^{]*\\)?{") nil t)
	(setq lst (nconc lst (list (match-string 2))))))
    (kill-buffer buf)
    lst))


(defun scan-directory( dir local )
  (let (files
	file
	filename
	classes
	lst)
    (setq files (directory-files dir nil ".\*\.\\(h\\|hh\\|H\\|hpp\\|h++\\)"))
    (while files
      (setq file (car files))
      (setq classes (check-file (concat dir "/" file)))
      (if local
	  (setq filename (concat "\"" file "\""))
	(setq filename (concat "<" file ">")))
      (if classes
	  (setq lst (nconc lst (list (list classes filename)))))
      (setq files (cdr files)))
    lst))

(defun check-file-string( file )
  (let (buf
	lst
	str)
    (setq buf (generate-new-buffer "class-tmp"))
    (save-excursion
      (set-buffer buf)
      (insert-file-contents-literally file)
      (while (search-forward-regexp (concat "class[ \t\n]+\\([a-zA-Z][a-zA-Z0-9_]*[ \t\n]+\\)?\\([a-zA-Z][a-zA-Z0-9_]*\\)[ \t\n]*"
					    "\\(:[^{]*\\)?{") nil t)
	(setq lst (nconc lst (list (match-string 2))))))
    (if lst
	(let* ((lsts lst)
	       cur)
	  (while lsts
	    (setq cur (car lsts))
	    (setq str (concat str " \"" cur "\""))
	    (setq lsts (cdr lsts)))
	  (setq str (concat "(" str ")")))
      (setq str nil))
    (kill-buffer buf)
    str))


(defun scan-directory-string( dir local name )
  (let (files
	file
	filename
	classes
	(lst ""))
    (setq files (directory-files dir nil ".\*\.\\(h\\|hh\\|H\\|hpp\\|h++\\)"))
    (while files
      (setq file (car files))
      (setq classes (check-file-string (concat dir "/" file)))
      (if local
	  (setq filename (concat "\\\"" file "\\\""))
	(setq filename (concat "<" file ">")))
      (if classes
	  (setq lst (concat lst "\n\t(" classes " \"" filename "\")")))
      (setq files (cdr files)))
    (setq lst (concat "(defvar " name " '(" lst "))\n"))
    (setq lst (concat lst "\n" "(project-add-include-list " name ")\n"))
    lst))

;; List of classes known
;; (REGEXP LOWCASE LOCAL RECURSIVE CHECKFILE)
;; (REGEXP LOWCASE nil INCLUDEFILE)
(defvar project-classes
  '(
    ("^eZ[a-zA-Z0-9_]+" (check-local-class ez-class-list))
    ("^Q[a-zA-Z0-9_]+" (check-local-class qt-class-list))
    ))

(defvar qt-parsed-classes nil)

;; (DIR RECURSIVE CHECK OLD)
(defvar qt-class-list '("/usr/lib/qt/include" nil t qt-parsed-classes))

(defvar ez-class-list '("." nil t qt-parsed-classes))

(defvar project-known-classes nil)

(defun check-for-class( word )
  (interactive)
  (let ((classes project-classes)
	(known project-known-classes)
	know
	(ok nil)
	reg
	class)
    (while (and known (not ok))
      (setq know (car known))
      (if (string-match (car know) word)
	  (let ()
	    (if (car (cdr know))
		(message (concat "#include \"" (car (cdr (cdr know))) "\""))
	      (message (concat "#include <" (car (cdr (cdr know))) ">")))
	    (setq ok t )))
      (setq known (cdr known)))
    (if (not ok)
	(while classes
	  (setq class (car classes))
	  (setq reg (car class))
	  (if (string-match reg word)
	      (let ((expr (car (cdr class)))
		    retur
		    file
		    local
		    (cls nil))
		(setq retur (eval (append expr (list word))))
		(message "Found these classes:")
		(setq file (car retur))
		(setq local (car (cdr retur)))
		(setq retur (cdr (cdr retur)))
		(while retur
		  (message (car (car retur)))
		  (setq cls (cons (car (car retur)) cls))
		  (setq retur (cdr retur)))
		(if cls
		    (setq project-known-classes (cons (list (regexp-opt cls) local file) project-known-classes)))
		))
	  (setq classes (cdr classes))))))

(defvar c++-source-extension-list '("c" "cc" "C" "cpp" "c++"))
(defvar c++-header-extension-list '("h" "hh" "H" "hpp"))

(defun check-local-class(class word)
  (interactive)
  (let ((dir (car class))
	(recur (car (cdr class)))
	(check (car (cdr (cdr class))))
	(old (car (cdr (cdr class)))))
    (if (file-exists-p dir)
	(let (name
	      loname
	      hiname
	      (exts c++-header-extension-list)
	      ext
	      (ok nil)
	      include)
	  (while (and exts (not ok))
	    (setq ext (car exts))
	    (setq name (concat word "." ext))
	    (setq loname (concat (downcase word) "." ext))
	    (setq hiname (concat (upcase word) "." ext))
	    (cond
	     ((file-exists-p (concat dir "/" name))
	      (let ()
		(setq ok t)
		(setq include name)))
	     ((file-exists-p (concat dir "/" loname))
	      (let ()
		(setq ok t)
		(setq include loname)))
	     ((file-exists-p (concat dir "/" hiname))
	      (let ()
		(setq ok t)
		(setq include hiname))))
	    (setq exts (cdr exts)))
	  (if ok
	      (let ((buf (find-buffer-visiting (concat dir "/" include)))
		    classes)
		(message include)
		(if buf
		    (save-excursion
		      (set-buffer buf)
		      (beginning-of-buffer)
		      (setq classes (find-classes-in-buffer))
		      (setq classes (cons (or (string-equal dir "") (string-equal dir ".") (string-equal dir nil)) classes))
		      (setq classes (cons include classes))
		      classes)
		  (save-excursion
		    (setq buf (find-file (concat dir "/" include)))
		    (set-buffer buf)
		    (setq classes (find-classes-in-buffer))
		    (setq classes (cons (or (string-equal dir "") (string-equal dir ".") (string-equal dir nil)) classes))
		    (setq classes (cons include classes))
		    (kill-buffer buf)
;		    (message (car (car classes)))
		    classes
		    ))))
	  ))))

(defvar buffer-include-list nil)


(defvar c++-class-decl-regexp (concat 
			       "^"
			       ;; May have a template<>(1)
			       "\\(template[ \t\n]*<[^>]*>\\)?"
			       ;; class declaration and name(3)
			       "[ \t\n]*class\\([ \t\n]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\)+"
			       ;; Ends in <>(4) if template
			       "\\(<[^>]*>\\)?"
			       ;; :
			       "\\([ \t\n]*:[ \t\n]*"
			       ;; public|protected|private(6)
			       "\\(public\\|protected\\|private\\)?"
			       ;; inherit name(7)
			       "[ \t\n]*\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
			       ;; template(8)
			       "\\(<[^>]*>\\)?\\)?"
			       "[ \t]*\\(//[^\n]*\n\\)?[ \t\n]*"
			       ;; { or ; (10)
			       "[{]"
;;			       "\\(;\\|"
;;			       "{\\([^{]*{[^}]*}\\)*}[ \t\n]*;\\)"
			       ))

(defvar c++-protect-clause-regexp (concat
				   "^"
				   ;; public|protected|private(2) signals|slots(4)
				   "\\(\\(public\\|protected\\|private\\)"
				   "\\([ \t]*\\(signals\\|slots\\)\\)?"
				   "\\|"
				   ;; or signals|slots(5)
				   "\\(signals\\|slots\\)\\)"
				   ;; :
				   ":"))

(defvar c++-class-func-regexp (concat
			       "^[ \t]*"
			       "\\(template[ \t\n]*<[^>]*>\\)?"
			       ""
			       ))

(defun find-classes-in-buffer()
  (interactive)
  (let ((classes '())
	pos end
	(mpos (make-marker))
	(mend (make-marker)))
    (save-excursion
      (beginning-of-buffer)
      (while (search-forward-regexp
	      c++-class-decl-regexp
	      nil t)
	(setq pos (match-beginning 0))
	(setq end (match-end 0))
	(set-marker mpos pos)
	(set-marker mend end)
	(message (match-string-no-properties 10))
;	(if (string-equal (match-string-no-properties 10) "{")
	(setq classes (cons (list (match-string-no-properties 3) mpos mend) classes))
;	    )
	))
     classes))


(defun find-includes()
  (interactive)
  (let ((lst '()))
    (save-excursion
      (beginning-of-buffer)
      (while (search-forward-regexp (concat "^#include[ \t]+\\("
					    "\\(<[a-zA-Z0-9._\-]+>\\)\\|"
					    "\\(\"[a-zA-Z0-9._\-]+\"\\)"
					    "\\)")
				    nil t)
	(let ((res (match-string-no-properties 1))
	      (pos (match-beginning 0))
	      (end (match-end 0))
	      (mpos (make-marker))
	      (mend (make-marker)))
	  (set-marker mpos pos)
	  (set-marker mend end)
	  (setq lst (cons (list res mpos mend) lst)))))
    lst))

(defun include-exists( include )
  (interactive)
  (let ((incs buffer-include-list)
	inc
	(ok nil))
    (while (and incs (not ok))
      (setq inc (car incs))
      (if (string-equal (car inc) include)
	  (setq ok t))
      (setq incs (cdr incs)))
    ok))


; (global-set-key [(meta f11)] '(lambda()
; 			   (interactive)
; 			   (if (include-exists "<qstring.h>")
; 			       (message "yes")
; 			     (message "no"))))

(global-set-key [(meta f11)] '(lambda()
			   (interactive)
			   (let (buf)
			     (setq buf (generate-new-buffer "*classes*"))
			     (set-buffer buf)
			     (insert-string (concat ";; -*- Mode: Emacs-Lisp -*-\n"
						    ";; -*- lisp -*-\n"
						    ";; Project classes\n"
						    "\n"
						    ";; Automaticly insert these include files when found in c++ file\n"))
			     (insert-string (scan-directory-string "./" t "project-local-classes"))
			     (write-file "./.emacs-classes")
			     (kill-buffer buf))))

(global-set-key [(alt f11)] '(lambda()
			   (interactive)
			   (message (check-for-class "QListView"))))

(global-set-key [(alt f11)] '(lambda()
			   (interactive)
			   (message (find-classes-in-buffer))))

(global-set-key [f11] '(lambda ()
			 (interactive)
			 (make-variable-buffer-local 'buffer-include-list)
			 (setq buffer-include-list (find-includes))))


(defun check-for-file( dir file rec)
  (interactive)
  (if (file-exists-p (concat dir file))))
      


;; Checks for known classes and removes any unnecessary includes
(defun remove-include()
  "Removes #include on the top of the file if certain class names are not found in the file"
  (interactive)
  (if (string-equal mode-name "C++")
      (let ((includes project-include-classes)
	    (include)
	    (include-classes)
	    (include-class)
	    (include-file))
	(while includes
	  (setq include (car includes))
	  (setq include-classes (car include))
	  (setq include-file (car (cdr include)))
	  (save-excursion
	    (beginning-of-buffer)
	    (if (search-forward-regexp (concat "^#include[ \t]+"
					       include-file
					       "[ \t]*\n") nil t )
		(let ((start)
		      (end)
		      (class-exists nil))
		  (setq start (match-beginning 0))
		  (setq end (match-end 0))
		  (setq class-exists nil)
		  (while (and (not class-exists) include-classes)
		    (setq include-class (car include-classes))
		    (save-excursion
		      (beginning-of-buffer)
		      (while (search-forward-regexp (concat "\\<\\(" include-class "\\)\\>") nil t)
			(if (string-equal (match-string 1) include-class)
			    (let ()
			      (setq class-exists t)))))
		    (setq include-classes (cdr include-classes)))
		  (if (not class-exists)
		      (save-restriction
			(delete-region start end)))))
	    (setq includes (cdr includes)))))))

;; Make sure the include files are updated when saving
(add-hook 'write-file-hooks (lambda()
			      (interactive)
			      (if c++-auto-include-add
				  (insert-include))))
(add-hook 'write-file-hooks (lambda()
			      (interactive)
			      (if c++-auto-include-remove
				  (remove-include))))


;; Count words in buffer
(defun count-words-buffer ()
  "Count the number of words in current the buffer
print a message in the minibuffer with the result."
  (interactive)
  (save-excursion
    (let ((count 0))
      (goto-char (point-min))
      (while (< (point) (point-max))
	(forward-word 1)
	(setq count (1+ count)))
      (message "buffer contains %d words." count))))

;; Returns a buffer with the main project
;; Will try to load it from disk if not found in buffer list
(defun project-main ()
  "Finds the current project."
  (let (buffer)
    (let ((buffers (buffer-list))
	  file)
      (while buffers
	(setq file (buffer-file-name (car buffers)))
	(if file
	    (if (string-match project-regexp file)
		(setq buffer(car buffers))))
	(setq buffers (cdr buffers))))
    (if buffer
	()
      (let ((files (directory-files (expand-file-name ".") nil project-regexp t))
	    file
	    dir
	    (count 0))
	(setq dir files)
	(while files
	  (setq count (1+ count))
	  (setq files (cdr files)))
	(if (> count 0 )
	    (if (<= count 1)
		(if project-ask-load
		    (if (y-or-n-p (concat "Really load \"" (car dir) "\" from disk?"))
			(setq buffer (find-file (car dir))))
		  (setq buffer (find-file (car dir))))
	      (let ()
		(message "%d files found." count)
		(if (y-or-n-p (concat "Really load \"" (car dir) "\" from disk?"))
		    (setq buffer (find-file (car dir)))))))))
    buffer))

;; Returns a buffer with the main project
(defun project-main-in-buffers ()
  "Finds the current project."
  (let (buffer)
    (let ((buffers (buffer-list))
	  file)
      (while buffers
	(setq file (buffer-file-name (car buffers)))
	(if file
	    (if (string-match project-regexp file)
		(setq buffer(car buffers))))
	(setq buffers (cdr buffers))))
    buffer))


(defun project-file-list ( buffer tag )
  (if (stringp tag)
      (let ((lst nil))
	(save-excursion
	  (set-buffer buffer)
	  (beginning-of-buffer)
	  (if (search-forward-regexp (concat "^"
					     tag
					     "\\([ \t]*\=\\)[ \t]*\\(\\\\[ \t]*[\n]\\)?"
					     "\\(\\([ \t]*[a-zA-Z\.\-/]+\\([ \t]*\\\\[ \t]*[\n]\\)?\\)*\\)")
				     nil t)
	      (save-restriction
		(beginning-of-buffer)
		(narrow-to-region (match-beginning 3) (match-end 3))
		(while (search-forward-regexp (concat "[ \t]*\\([a-zA-Z\.\-/]+\\)"
						      "\\([ \t]*\\\\[ \t]*[\n]\\)?")
					      nil t)
		  (setq lst (cons (match-string-no-properties 1) lst))))))
	(nreverse lst))
    (error "Must supply a tag string" )))

(defun project-files ( project )
  (list (project-file-list project "SOURCES")
	(project-file-list project "HEADERS")))


(defun project-load-check ()
  (if (string-match project-regexp (buffer-name (current-buffer)))
      (project-update-menu)))

(add-hook 'find-file-hooks 'project-load-check)

(defun project-update-menu ()
  "Updates the project files menu with the files in the project"
  (interactive)
  (let* ((project (project-main))
	 (lst (project-files project))
	 (slst (car lst))
	 (hlst (car (cdr lst)))
	 bufdir)
    (setq bufdir (file-name-directory (buffer-file-name project)))
    (easy-menu-change '("files")
		      "Project Files"
		      (list (cons "Sources"
				  (mapcar '(lambda (entry)
					     (vector entry (list 'find-file (concat bufdir "/" entry)) t))
					  (car (project-files (project-main)))))
			    (cons "Headers"
				  (mapcar '(lambda (entry)
					     (vector entry (list 'find-file entry) t))
					  (car (cdr (project-files (project-main)))))))
		      "open-file")))

;; Loads all header and source files found in the project file.
(defun project-load-files ()
  "Loads all the project files."
  (interactive)
  (let ((project (project-main))
	lst slst hlst)
    (if project
	(save-excursion
	  (setq lst (project-files project))
	  (setq slst (car lst))
	  (setq hlst (car (cdr lst)))
	  (while slst
	    (find-file-noselect (car slst))
	    (setq slst (cdr slst)))
	  (while hlst
	    (find-file-noselect (car hlst))
	    (setq hlst (cdr hlst))))
      (message "Couldn't find any projects \(In right directory ?\)."))))

(defun project-execute ()
  "Executes the exe file in the current project."
  (interactive)
  (let (name)
    (save-excursion
      (set-buffer (project-main))
      (beginning-of-buffer)
      (save-excursion
	(if (search-forward-regexp "^TARGET[ \t]*\=[ \t]*\\([a-zA-Z\.\-_]+[ \t]*\\)[\n]" nil t)
	    (save-restriction
	      (beginning-of-buffer)
	      (shell-command (concat "./" (match-string 1) " &") (get-buffer "*compilation*")))
	  (message "No target found"))))))

(defun project-debug ()
  "Debugs the exe file in the current project."
  (interactive)
  (let (name)
    (save-excursion
      (set-buffer (project-main))
      (beginning-of-buffer)
      (save-excursion
	(if (search-forward-regexp "^TARGET[ \t]*\=[ \t]*\\([a-zA-Z\.\-_]+[ \t]*\\)[\n]" nil t)
	    (save-restriction
	      (beginning-of-buffer)
	      (shell-command (concat project-debugger " " (match-string 1) " &") (get-buffer "*compilation*"))))))))

; Compiles the current program with options
(defun project-compile(opts)
  "Compile current project with options"
  (interactive "MEnter compile options: ")
  (save-excursion
    (let ((project (project-main)))
      (if project
	  (let ((object-dir (project-object-dir project)))
	    (set-buffer project)
	    (if (not (string-match object-dir ""))
		(if (not (file-exists-p object-dir))
		    (make-directory object-dir))))))
    (compile (concat "make " opts))))

;; Inserts a file into the project
(defun project-insert-file (project file keyword)
  "Insert a FILE into the current PROJECT buffer after the given KEYWORD."
  (save-excursion
    (set-buffer project)
    (if (not (search-forward (file-relative-name file) nil t))
	(save-excursion
	  (beginning-of-buffer)
	  (if (search-forward-regexp (concat "^"
					     keyword
					     "\\([ \t]*\=\\)[ \t]*\\(\\\\[ \t]*[\n]\\)?"
					     "\\([ \t]*[a-zA-Z\.\-/]+\\([ \t]*\\\\[ \t]*[\n]\\)?\\)*")
				     nil t)
	      (save-restriction
		(insert-string " \\\n")
		(indent-relative)
		(insert-string (file-relative-name file))))))))

;; Removes a file from the project
(defun project-remove-file (project file keyword)
  "Removes a FILE from the current PROJECT buffer after the give KEYWORD."
  (save-excursion
    (set-buffer project)
    (beginning-of-buffer)
    (if (search-forward-regexp (concat "^"
				       keyword
				       "\\([ \t]*\=\\)\\(\\(\\([ \t]*\\\\[ \t]*[\n]\\)?"
				       "\\([ \t]*[a-zA-Z\.\-/]+\\)\\)*\\)")
			       nil t)
	(save-restriction
	  (narrow-to-region (match-beginning 2) (match-end 2))
	  (beginning-of-buffer)
	  (if (search-forward-regexp (concat "\\([ \t]*\\\\[ \t]*[\n]\\)?"
					     "\\([ \t]*"
					     file
					     "\\)")
				     nil t)
	      (replace-match "")
	    (message "Couldn't find file in project")))
      (message (concat "Couldn't find keyword: " keyword " in project.")))))

(defun project-replace-class-name (buffer classname oldname)
  "Replaces every occurence of project-normal-name-match,project-downcase-name-match and project-upcase-name-match with CLASSNAME,
in normal, downcase and upcase letters, in BUFFER."
  (save-excursion
    (set-buffer buffer)
    (beginning-of-buffer)
    (save-excursion
      (while (search-forward project-normal-name-match nil t)
	(save-restriction
	  (narrow-to-region (match-beginning 0) (match-end 0))
	  (replace-match classname))))
    (save-excursion
      (while (search-forward project-downcase-name-match nil t)
	(save-restriction
	  (narrow-to-region (match-beginning 0) (match-end 0))
	  (replace-match (downcase classname)))))
    (save-excursion
      (while (search-forward project-upcase-name-match nil t)
	(save-restriction
	  (narrow-to-region (match-beginning 0) (match-end 0))
	  (replace-match (upcase classname)))))
    (save-excursion
      (while (search-forward project-deriveclass-match nil t)
	(save-restriction
	  (narrow-to-region (match-beginning 0) (match-end 0))
	  (replace-match oldname))))
    (save-excursion
      (while (search-forward "\<real-name\>" nil t)
	(save-restriction
	  (narrow-to-region (match-beginning 0) (match-end 0))
	  (replace-match user-full-name))))
    (save-excursion
      (while (search-forward "\<login-name\>" nil t)
	(save-restriction
	  (narrow-to-region (match-beginning 0) (match-end 0))
	  (replace-match user-login-name))))
    (save-excursion
      (while (search-forward "\<mail-name\>" nil t)
	(save-restriction
	  (narrow-to-region (match-beginning 0) (match-end 0))
	  (replace-match project-mail-account))))))

;; Returns the project type, currently Qt or c++.
(defun project-type (project)
  "Returns type of project in use."
  (let (name)
    (save-excursion
      (set-buffer project)
      (beginning-of-buffer)
      (save-excursion
	(if (search-forward-regexp "^CONFIG[ \t]*\=[ \t]*\\(\\([a-zA-Z\.\-_]+[ \t]*\\)*\\)[\n]" nil t)
	    (save-restriction
	      (beginning-of-buffer)
	      (narrow-to-region (match-beginning 1) (match-end 1))
	      (if (search-forward "qt" nil t)
		  (setq name "Qt")
		(setq name "c++")))
	  (setq name "c++"))))
    name))


;; Returns a list of options
(defun project-config (project)
  "Returns project configuration."
  (interactive)
  (let (name)
    (save-excursion
      (set-buffer project)
      (beginning-of-buffer)
      (save-excursion
	(if (search-forward-regexp "^CONFIG[ \t]*\=[ \t]*\\(\\([a-zA-Z\.\-_]+[ \t]*\\)*\\)[\n]" nil t)
	    (save-restriction
	      (beginning-of-buffer)
	      (narrow-to-region (match-beginning 1) (match-end 1))
	      (while (search-forward-regexp "[a-zA-Z\.\-_]+" nil t)
		(let ()
		  (setq name (cons (match-string 0) name))))))
	(defvar project-config-opts name)))
    name))

;; Returns project object dir.
(defun project-object-dir (project)
  "Returns project object directory if exists."
  (interactive)
  (let (name)
    (save-excursion
      (set-buffer project)
      (beginning-of-buffer)
      (save-excursion
	(if (search-forward-regexp "^OBJECTS_DIR[ \t]*\=[ \t]*\\([a-zA-Z\.\-_]+[ \t]*\\)[\n]" nil t)
	    (setq name (match-string 1))
	  (setq name ""))))
    name))


;; Aks for project name and creates a new project
(defun project-new ()
  "Creates a new project if the no project exists"
  (interactive)
  (let ((project (project-main)))
    (if project
	(message "Project already exists.")
      (let (newproject
	    projectfile
	    projectbuf)
	(setq newproject (read-from-minibuffer (concat "Enter name of new project : ")))
	(setq projectfile (concat (downcase newproject)
				  ".pro"))
	(setq projectbuf (find-file projectfile))
	(project-replace-class-name projectbuf newproject "")
	(setq mainbuf (find-file-noselect "main.cpp"))
	(project-replace-class-name mainbuf "main" "")
	(save-buffer)
;;	(desktop-save "./")
;; Use revive instead
	(if option-package-available-revive
	    (save-current-configuration))
	(shell-command (concat "tmake -o Makefile " projectfile))
	(project-update-menu)))))

;; Returns the name of the project
(defun project-name (project)
  "Returns the name of the project."
  (let (name)
    (save-excursion
      (set-buffer project)
      (beginning-of-buffer)
      (if (search-forward-regexp "^PROJECT[ \t]*\=[ \t]*\\([a-zA-Z\.\-]+\\)[ \t]*[\n]" nil t)
	  (setq name (match-string 1))
	(setq name "Noname")))
    name))

;; Creates some customization files in the current directory
(defun project-create-custom-files ()
  "Add a class to the current project."
  (interactive)
  (let ((project (project-main))
	dir)
    (if project
	(setq dir (file-name-directory (buffer-file-name project)))
      (setq dir (expand-file-name ".")))
    (let* ((old-buffer (current-buffer))
	   (varfile ".emacs-vars")
	   (customfile ".emacs-custom")
	   (varbuffer (find-file-noselect varfile t t))
	   (custombuffer (find-file-noselect customfile t t)))
      (switch-to-buffer varbuffer)
      (if (> (buffer-size) 0)
	  (delete-region 1 (buffer-size)))
      (insert (concat ";; -*- Mode: Emacs-Lisp -*-\n;; -*- lisp -*-\n"
		      ";---------------------------------------------------------------------\n"
		      ";; This file was generated by emacs config Version "
		      (option-config-version) ", modify it to suit your needs\n\n"))
      (insert (concat ";; Set the default header extensions\n"
		      "(setq c++-default-header-ext \"" c++-default-header-ext "\")\n\n"))
      (insert (concat ";; Set the default source extensions\n"
		      "(setq c++-default-source-ext \"" c++-default-source-ext "\")\n\n"))
      (insert (concat ";; We use the local autoinsert files\n"
		      "(setq project-auto-insert-directory (concat (expand-file-name \".\") \"/.autoinsert/\"))\n"))
      (basic-save-buffer)
      (kill-buffer varbuffer)

      (switch-to-buffer custombuffer)
      (if (> (buffer-size) 0)
	  (delete-region 1 (buffer-size)))
      (insert (concat ";; -*- Mode: Emacs-Lisp -*-\n;; -*- lisp -*-\n"
		      ";---------------------------------------------------------------------\n"
		      ";; This file was generated by emacs config Version "
		      (option-config-version) ", modify it to suit your needs\n\n"))
      (insert (concat ";; Make sure the auto insert directory is properly updated\n"
		      "(setq auto-insert-directory (project-select-autoinsert))"))
      (basic-save-buffer)
      (kill-buffer custombuffer)

      (if (not (file-exists-p ".autoinsert"))
	  (make-directory ".autoinsert"))
      (setq auto-insert-files (directory-files "~/.autoinsert"))
      (mapcar '(lambda (x)
		 (if (not (or (string-equal "." x)
			       (string-equal ".." x)))
		     (copy-file (concat "~/.autoinsert/" x) (concat dir "/.autoinsert/" x))))
	      auto-insert-files)

      (switch-to-buffer old-buffer))))


;; Adds a class to the current project, creates the header and/or source file if non existing.
(defun class-add ()
  "Add a class to the current project."
  (interactive)
  (let ((project (project-main)))
    (if project
	(let (newclass
	      oldclass
	      oldname
	      headerfile
	      sourcefile
	      headerbuf
	      sourcebuf
	      proj_dir
	      real_headerfile
	      real_sourcefile
	      dest_dir)
	  (setq proj_dir (file-name-directory (buffer-file-name project)))
	  (setq cur_dir (file-name-directory (buffer-file-name (current-buffer))))
	  (setq newclass (read-from-minibuffer (concat "Enter class name to add to "
						       (project-type project)
						       " project \""
						       (project-name project)
						       "\": ")))
	  (setq oldname (read-from-minibuffer "Enter class to derive from(Enter for none): "))
	  (setq dest_dir (file-relative-name  proj_dir))
	  (if (string-equal dest_dir "./")
	      (setq dest_dir ""))
	  (setq dest_dir (read-from-minibuffer "Enter relative destination directory (Enter for current): " dest_dir))
	  (if (string-equal dest_dir "")
	      ()
	    (setq dest_dir (file-name-as-directory dest_dir)))
	  (make-directory dest_dir t)
	  (if (eq oldname nil)
	      (setq oldname ""))
	  (if (not (string-equal oldname ""))
	      (setq oldclass (concat " : public " oldname))
	    (setq oldclass ""))
	  (setq headerfile (concat dest_dir (downcase newclass)
				"." c++-default-header-ext))
	  (setq sourcefile (concat dest_dir (downcase newclass)
				"." c++-default-source-ext))
	  (setq headerbuf (find-file-noselect headerfile))
	  (setq sourcebuf (find-file-noselect sourcefile))
	  (setq real_headerfile (file-relative-name (buffer-file-name headerbuf) proj_dir))
	  (setq real_sourcefile (file-relative-name (buffer-file-name sourcebuf) proj_dir))
	  (project-replace-class-name headerbuf newclass oldclass)
	  (if (not (string-equal oldname ""))
	      (setq oldclass (concat "\n    : " oldname "(===)" ))
	    (setq oldclass ""))
	  (project-replace-class-name sourcebuf newclass oldclass)
	  (project-insert-params oldname headerbuf sourcebuf)
	  (project-insert-file project real_headerfile "HEADERS")
	  (project-insert-file project real_sourcefile "SOURCES")
	  (set-buffer project)
	  (shell-command (concat "tmake -o Makefile "
				 (file-name-nondirectory (buffer-file-name project))))
	  (project-update-menu)
	  (switch-to-buffer sourcebuf)
	  (switch-to-buffer headerbuf))
      (message "Couldn't find any projects \(In right directory ?\).")
      )))

;; Removes a file from the current project, and deletes them if is set.
;; TODO: Request source directory for files as in class-add
(defun class-remove ()
  "Removes a class from the current project."
  (interactive)
  (let ((project (project-main)))
    (if project
	(let (newclass
	      headerfile
	      sourcefile
	      headerbuf
	      sourcebuf)
	  (setq newclass (read-from-minibuffer (concat "Enter class name to remove from "
						       (project-type project)
						       " project \""
						       (project-name project)
						       "\": ")))
	  (setq headerfile (concat (downcase newclass)
				   "." c++-default-header-ext))
	  (setq sourcefile (concat (downcase newclass)
				   "." c++-default-source-ext))
	  (let ((projectdir (file-name-directory (buffer-file-name project)))
		(headerbuffer (get-buffer headerfile))
		(sourcebuffer (get-buffer sourcefile))
		realheaderfile realsourcefile;;Absolute file names
		relheaderfile relsourcefile);;Relative files names
	    (setq realheaderfile (buffer-file-name headerbuffer))
	    (setq realsourcefile (buffer-file-name sourcebuffer))
	    (setq relheaderfile (file-relative-name realheaderfile projectdir))
	    (setq relsourcefile (file-relative-name realsourcefile projectdir))
	    (if project-delete-redundant
		(let ()
		  (if headerbuffer
		      (let ()
			(set-buffer headerbuffer)
			(save-buffer)
			(kill-this-buffer)))
		  (if (file-exists-p realheaderfile)
		      (if project-delete-confirm
			  (if (y-or-n-p (concat "Delete file " relheaderfile " "))
			      (delete-file realheaderfile))
			(delete-file realheaderfile)))

		  (if sourcebuffer
		      (let ()
			(set-buffer sourcebuffer)
			(save-buffer)
			(kill-this-buffer)))
		  (if (file-exists-p realsourcefile)
		      (if project-delete-confirm
			  (if (y-or-n-p (concat "Delete file " relsourcefile " "))
			      (delete-file realsourcefile))
			(delete-file realsourcefile)))
		  (if (file-exists-p (concat (file-name-directory realsourcefile) "moc_" sourcefile))
		      (delete-file (concat (file-name-directory realsourcefile) "moc_" sourcefile)))))
	    (project-remove-file project relheaderfile "HEADERS")
	    (project-remove-file project relsourcefile "SOURCES")
	    (set-buffer project)
	    (save-buffer)
	    (shell-command (concat "tmake -o Makefile "
				   (buffer-file-name project)))
	    (project-update-menu)))
      (message "Couldn't find any projects \(In right directory ?\).")
      )))


(defconst project-c++-func-regexp (concat
			 "^"		; beginning of line is required
			 "\\(template[ \t]*<[^>]+>[ \t]*\\)?" ; there may be a "template <...>"
			 "\\([a-zA-Z0-9_:]+[ \t]+\\)?" ; type specs; there can be no
			 "\\([a-zA-Z0-9_:]+[ \t]+\\)?" ; more than 3 tokens, right?

			 "\\("		; last type spec including */&
			 "[a-zA-Z0-9_:]+"
			 "\\([ \t]*[*&]+[ \t]*\\|[ \t]+\\)" ; either pointer/ref sign or whitespace
			 "\\)?"		; if there is a last type spec
			 "\\("		; name; take that into the imenu entry
			 "[a-zA-Z0-9_:~]+" ; member function, ctor or dtor...
					; (may not contain * because then
				; "a::operator char*" would become "char*"!)
			 "\\|"
			 "\\([a-zA-Z0-9_:~]*::\\)?operator"
			 "[^a-zA-Z1-9_][^(]*" ; ...or operator
			 " \\)"
			 "[ \t]*([^)]*)[ \t\n]*[^ ;]" ; require something other than a ; after
			 ))


(defun c-project-menu (modestr)
  (let ((m
	 '(
	   ["New Project"      project-new (not (project-main-in-buffers))]
	   ["Load Project"     (set-buffer (project-main)) (not (project-main-in-buffers))]
	   ["Close Project"     (kill-buffer (project-main-in-buffers)) (project-main-in-buffers)]
	   ["Create Makefile"   (make-makefile) (project-main-in-buffers)]
	   ["Load All Sources"  project-load-files (project-main-in-buffers)]
	   "---"
	   ["Add Class"         class-add (project-main-in-buffers)]
	   ["Remove Class"      class-remove (project-main-in-buffers)]
	   "---"
	   ["Create Custom Files" project-create-custom-files t]
	   )))
    (cons modestr m)))

(defun config-menu (modestr)
  (let ((m
	 '(
	   ("Project"
	    ["Automatic Insert Include"
	     (progn (setq c++-auto-include-add (not c++-auto-include-add)) (option-config-invalidate))
	     :style toggle
	     :selected c++-auto-include-add]
	    ["Automatic Remove Include"
	     (progn (setq c++-auto-include-remove (not c++-auto-include-remove)) (option-config-invalidate))
	     :style toggle
	     :selected c++-auto-include-remove]
	    ["Ask Before Loading Project"
	     (progn (setq project-ask-load (not project-ask-load)) (option-config-invalidate))
	     :style toggle
	     :selected project-ask-load]
	    ["Use Project Auto Insertion"
	     (progn (setq project-use-auto-insert (not project-use-auto-insert)) (option-config-invalidate))
	     :style toggle
	     :selected project-use-auto-insert]
	    "---"
	    ["Delete Removed Classes"
	     (progn (setq project-delete-redundant (not project-delete-redundant)) (option-config-invalidate))
	     :style toggle
	     :selected project-delete-redundant]
	    ["Confirm Deletion of Removed Classes"
	     (progn (setq project-delete-confirm (not project-delete-confirm)) (option-config-invalidate))
	     :style toggle
	     :active project-delete-redundant
	     :selected project-delete-confirm]
	    )
	   ("General"
	    ["Smooth scrolling"
	     (option-smooth-scroll-toggle)
	     :style toggle
	     :selected option-smooth-scroll]
	    )
	   ("Packages"
	    ["ibuffer"
	     (if option-package-load-ibuffer
		 (option-unload-package option-package-ibuffer)
	       (option-load-package option-package-ibuffer))
	     :style toggle
	     :active option-package-available-ibuffer
	     :selected option-package-load-ibuffer]
;;	    ["CUA (Cut/Copy/Paste/Undo)"
;;	     (if option-package-load-CUA
;;		 (option-unload-package option-package-CUA)
;;	       (option-load-package option-package-CUA))
;;	     :style toggle
;;	     :active option-package-available-CUA
;;	     :selected option-package-load-CUA]
	    ["Blank"
	     (if option-package-load-blank-mode
		 (option-unload-package option-package-blank-mode)
	       (option-load-package option-package-blank-mode))
	     :style toggle
	     :active option-package-available-blank-mode
	     :selected option-package-load-blank-mode]
	    ["Mouse Wheel Support"
	     (if option-package-load-mwheel
		 (option-unload-package option-package-mwheel)
	       (option-load-package option-package-mwheel))
	     :style toggle
	     :active option-package-available-mwheel
	     :selected option-package-load-mwheel]
	    ["Blinking Cursor"
	     (if option-package-load-blinking-cursor
		 (option-unload-package option-package-blinking-cursor)
	       (option-load-package option-package-blinking-cursor))
	     :style toggle
	     :active option-package-available-blinking-cursor
	     :selected option-package-load-blinking-cursor]
	    ["Recent Files"
	     (if option-package-load-recentf
		 (option-unload-package option-package-recentf)
	       (option-load-package option-package-recentf))
	     :style toggle
	     :active option-package-available-recentf
	     :selected option-package-load-recentf]
	    ["Rectangle Mark"
	     (if option-package-load-rect-mark
		 (option-unload-package option-package-rect-mark)
	       (option-load-package option-package-rect-mark))
	     :style toggle
	     :active option-package-available-rect-mark
	     :selected option-package-load-rect-mark]
	    ["Revive"
	     (if option-package-load-revive
		 (option-unload-package option-package-revive)
	       (option-load-package option-package-revive))
	     :style toggle
	     :active option-package-available-revive
	     :selected option-package-load-revive]
	    ["Java Development Enviroment"
	     (if option-package-load-jde
		 (option-unload-package option-package-jde)
	       (option-load-package option-package-jde))
	     :style toggle
	     :active option-package-available-jde
	     :selected option-package-load-jde]
	    ["Save History"
	     (if option-package-load-save-history
		 (option-unload-package option-package-save-history)
	       (option-load-package option-package-save-history))
	     :style toggle
	     :active option-package-available-save-history
	     :selected option-package-load-save-history]
	    ["Speedbar"
	     (if option-package-load-speedbar
		 (option-unload-package option-package-speedbar)
	       (option-load-package option-package-speedbar))
	     :style toggle
	     :active option-package-available-speedbar
	     :selected option-package-load-speedbar]
	    ["Completion"
	     (if option-package-load-completion
		 (option-unload-package option-package-completion)
	       (option-load-package option-package-completion))
	     :style toggle
	     :active option-package-available-completion
	     :selected option-package-load-completion]
	    ["Auto Revert"
	     (if option-package-load-autorevert
		 (option-unload-package option-package-autorevert)
	       (option-load-package option-package-autorevert))
	     :style toggle
	     :active option-package-available-autorevert
	     :selected option-package-load-autorevert]
	    ["Highlight Line"
	     (if option-package-load-line-highlight
		 (option-unload-package option-package-line-highlight)
	       (option-load-package option-package-line-highlight))
	     :style toggle
	     :active option-package-available-line-highlight
	     :selected option-package-load-line-highlight]
	    )
; 	   ("Package options"
; 	    ["CUA (Cut/Copy/Paste/Undo)"
; 	     (option-smooth-scroll-toggle)
; 	     :style toggle
; 	     :active option-package-available-CUA
; 	     :selected option-smooth-scroll]
; 	    ["Mouse Wheel Support"
; 	     (progn (setq do-require-mwheel (not do-require-mwheel)) (option-config-invalidate))
; 	     :style toggle
; 	     :active option-package-available-mwheel
; 	     :selected do-require-mwheel]
; 	    ["Blinking Cursor"
; 	     (progn (if blinking-cursor-mode (blinking-cursor-mode -1) (blinking-cursor-mode 1)) (option-config-invalidate))
; 	     :style toggle
; 	     :active (require 'blinking-cursor nil t)
; 	     :selected blinking-cursor-mode]
; 	    ["Recent Files"
; 	     (progn (if recentf-mode (recentf-mode -1) (recentf-mode 1)) (option-config-invalidate))
; 	     :style toggle
; 	     :active (require 'recentf nil t)
; 	     :selected recentf-mode]
; 	    ["Save History"
; 	     (option-save-history-toggle)
; 	     :style toggle
; 	     :active (require 'save-history nil t)
; 	     :selected option-save-history-flag]
; 	    )
	   "---"
	   ("Session"
	    ["Save"
	     (save-current-configuration)
	     :active option-package-load-revive]
	    ["Restore"
	     (resume-try)
	     :active option-package-load-revive]
	    ["Wipe"
	     (wipe-try)
	     :active option-package-load-revive])
	   ["ibuffer"
	    (ibuffer)
	    :active option-package-load-ibuffer]
	   "---"
	   ["Save global"   (option-save-to-file) t]
	   ["Save"          (option-save-to-file-locally) t]
	   ["Revert global" (option-load-from-file) t]
	   ["Revert"        (option-load-from-file-locally) t]
	   )))
    (cons modestr m)))

(let ((option-name (cond ((emacs-type-is-regular) "Options")
			 (t "EOptions"))))
  (easy-menu-define config-symbol-menu lisp-interaction-mode-map "General options"
		    (config-menu option-name))
  (easy-menu-add (config-menu option-name))

  (easy-menu-define config-symbol-menu emacs-lisp-mode-map "General options"
		    (config-menu option-name))
  (easy-menu-add (config-menu option-name)))

(defun project-test ()
  (interactive)
  (if (search-forward-regexp project-c++-func-regexp nil t )
      (message (match-string 0))))


; Reads in an abbrev file if it exists
; C-x a i g to create an abbrev
(if (file-exists-p abbrev-file-name)
    (read-abbrev-file))

(defun project-tag-file()
  (let ((tag "TAGS")
	(project (project-main))
	prodir
	tagfile)
    (setq prodir (file-name-directory (buffer-file-name project)))
    (setq tagfile (concat prodir tag))
    tagfile))

(defun project-load-tags()
;  (interactive)
  (let ((tagfile (project-tag-file)))
    (if (file-exists-p tagfile)
	(visit-tags-table tagfile))))

(defvar project-tag-list)

(defvar project-qt-tag-dir '("$QTDIR/src/dialogs/*.h" "$QTDIR/src/kernel/*.h"
			     "$QTDIR/src/tools/*.h" "$QTDIR/src/widgets/*.h"))

(defvar project-standard-tag-dir '("/usr/include/*.h" "/usr/include/bits/*.h"
				   "/usr/include/g++-2/*" "/usr/include/sys/*.h"))

(defun project-expand-tag-list (lst)
  (interactive)
  (let (dirs item items)
    (setq items lst)
    (while items
      (setq item (car items))
      (if dirs
	  (setq dirs (concat dirs " " item))
	(setq dirs item))
      (setq items (cdr items)))
    dirs))

(defun project-config-has ( var )
  (interactive)
  (let ((items (project-config (project-main)))
	item
	(found nil))
    (while items
      (setq item (car items))
      (if (string-equal item var)
	  (setq found t))
      (setq items (cdr items)))
    found))

(defun project-generate-tags()
  (interactive)
  (let (files
	(lst nil))
    (if (project-config-has "qt")
	(setq lst (nconc lst project-qt-tag-dir)))
    (setq lst (nconc lst project-standard-tag-dir))
    (setq files (project-expand-tag-list lst))
    (shell-command (concat "etags " files))
    ))
  

(defun project-expand-symbol( arg )
  (interactive "P")
  (if (not (file-exists-p (project-tag-file)))
      (project-generate-tags))
;       (project-load-tags)
  (complete-symbol arg))

(defun project-hide-entry ()
  (interactive)
  (if (outline-on-heading-p)
      (let ()
	(show-entry)
	(forward-char))
    (hide-entry)))

;; Jumps to next buffer in the bufferlist, consecutive uses will browse trough all your buffers
(defun switch-to-next-buffer()
  "Jumps to next buffer in the buffer list, or the beginning of the list if at the end"
  (interactive)
  (let ((cur (current-buffer))
	(ok t))
    ;; Find current buffer in list
    (while (and cur ok)
      (setq cur (next-buffer cur (current-buffer)))
      (if cur
	  (if (buffer-allowed cur)
	      (setq ok nil))))
    (if (and cur (not ok))
	(switch-to-buffer cur t))))

;; Jumps to next buffer in the bufferlist, consecutive uses will browse trough all your buffers
(defun next-buffer(buf orig)
  "Jumps to next buffer in the buffer list, or the beginning of the list if at the end"
  (interactive)
  (let ((lst (buffer-list))
	nxt
	cur)
    ;; Find current buffer in list
    (while (and lst (not (eq buf (car lst))))
      (setq cur (car lst))
      (setq lst (cdr lst)))
    ;; Get next
    (setq nxt (car (cdr lst)))
    (if (eq nxt orig)
	nil)
    ;; If zero get first.
    (if nxt
	()
      (setq nxt (car (buffer-list))))
    nxt))

(defun buffer-allowed( buf )
  (interactive)
  (let ((incs buffer-include-regexp)
	inc
	(bname (buffer-name buf))
	(allow nil))
    (while (and incs (not allow))
      (setq inc (car incs))
      (if (string-match inc bname)
	  (setq allow t))
      (setq incs (cdr incs)))
    (if allow
	(let ((exs buffer-exclude-regexp)
	      ex)
	  (while (and exs allow)
	    (setq ex (car exs))
	    (if (string-match ex bname)
		(setq allow nil))
	    (setq exs (cdr exs)))
	  allow)
      allow)))

;; make backup files in ~/.backups/ rather than scattered around all
;; over the filesystem.
(defun make-backup-file-name (file-name)
  "Create the non-numeric backup file name for `file-name'."
  (require 'dired)
  (if (file-exists-p "~/.backups")
      (concat (expand-file-name "~/.backups/")
	      (dired-replace-in-string "/" "|" file-name))
    (concat file-name "~")))

;; disable backups for files in /tmp or in my Mail or News directories.     
(defun ecm-backup-enable-predicate (filename)
  (and (not (string= "/tmp/" (substring filename 0 5)))           
       (not (string-match "/Mail/" filename))           
       (not (string-match "/News/" filename))))        

(setq backup-enable-predicate 'ecm-backup-enable-predicate)


; This doesn't seem to work:
(setq auto-save-directory (expand-file-name "~/.autosaves/"))

; This gives error when trying ispell
;(setq ispell-dictionary "/usr/lib/ispell/norsk.hash")

;---------------------------------------------------------------------
; C++ mode modifications
;

;; Add project menu to the mode first started in emacs
(easy-menu-define project-menu lisp-interaction-mode-map "C++ Project Commands"
		  (c-project-menu "Project"))
(easy-menu-add (c-project-menu "Project"))


;; Define a new regexp for font-lock-mode
;; DONT'T MESS WITH IT
(if (emacs-type-is-regular)
    (defconst c++-new-font-lock-keywords
      '(
	("\\<[0-9]+\\.[0-9]+\\>" (0 font-lock-floatnumber-face))
	("^#[ 	]*error[ 	]+\\(.+\\)"
	 (1 font-lock-warning-face prepend))
	("^#[ 	]*\\(import\\|include\\)[ 	]*\\(<[^>\"\n]*>?\\)"
	 (2 font-lock-string-face))
	("^#[ 	]*define[ 	]+\\(\\sw+\\)("
	 (1 font-lock-function-name-face))
	("^#[ 	]*\\(elif\\|if\\)\\>"
	 ("\\<\\(defined\\)\\>[ 	]*(?\\(\\sw+\\)?" nil nil
	  (1 font-lock-builtin-face)
	  (2 font-lock-variable-name-face nil t)))
	("^#[ 	]*\\(\\sw+\\)\\>[ 	!]*\\(\\sw+\\)?"
	 (1 font-lock-builtin-face)
	 (2 font-lock-variable-name-face nil t))
	("\\<\\(public\\|private\\|protected\\)\\>[ \t]+\\(\\<\\(signals\\|slots\\)\\>\\)[ \t]*:"
	 (1 font-lock-type-face)
	 (2 font-lock-type-face)
	 )
	("\\<\\(class\\|public\\|private\\|protected\\|typename\\|signals\\|slots\\)\\>[ 	]*\\(\\(\\sw+\\)\\>\\([ 	]*<\\([^>\n]+\\)[ 	*&]*>\\)?\\([ 	]*::[ 	*~]*\\(\\sw+\\)\\)*\\)?"
	 (1 font-lock-type-face)
	 (3
	  (if
	      (match-beginning 6)
	      font-lock-type-face font-lock-function-name-face)
	  nil t)
	 (5 font-lock-function-name-face nil t)
	 (7 font-lock-function-name-face nil t))
	("^\\(\\sw+\\)\\>\\([ 	]*<\\([^>\n]+\\)[ 	*&]*>\\)?\\([ 	]*::[ 	*~]*\\(\\sw+\\)\\)*[ 	]*("
	 (1
	  (if
	      (or
	       (match-beginning 2)
	       (match-beginning 4))
	      font-lock-type-face font-lock-function-name-face))
	 (3 font-lock-function-name-face nil t)
	 (5 font-lock-function-name-face nil t))
	("\\<\\(auto\\|bool\\|c\\(har\\|o\\(mplex\\|nst\\)\\)\\|double\\|e\\(num\\|x\\(p\\(licit\\|ort\\)\\|tern\\)\\)\\|f\\(loat\\|riend\\)\\|in\\(line\\|t\\)\\|long\\|mutable\\|namespace\\|register\\|s\\(hort\\|igned\\|t\\(atic\\|ruct\\)\\)\\|t\\(emplate\\|ypedef\\)\\|u\\(n\\(ion\\|signed\\)\\|sing\\)\\|v\\(irtual\\|o\\(id\\|latile\\)\\)\\|JBF[a-zA-Z0-9]+\\|eZ[a-zA-Z0-9_]+\\|Q[A-Z][a-zA-Z_]*\\|Q[a-z][A-Z][a-zA-Z_]*\\|uint\\|ulong\\|string\\)\\>"
	 (0 font-lock-type-face))
	("\\<\\(operator\\)\\>[ 	]*\\(!=\\|%=\\|&[&=]\\|()\\|\\*=\\|\\+[+=]\\|-\\(>\\*\\|[=>-]\\)\\|/=\\|<\\(<=\\|[<=]\\)\\|==\\|>\\(>=\\|[=>]\\)\\|\\[\\]\\|\\^=\\||[=|]\\|[!%&*+,/<=>|~^-]\\)?"
	 (1 font-lock-keyword-face)
	 (2 font-lock-builtin-face nil t))
	("\\<\\(case\\|goto\\)\\>[ 	]*\\(-?\\sw+\\)?"
	 (1 font-lock-keyword-face)
	 (2 font-lock-constant-face nil t))
	(":"
	 ("^[ 	]*\\(\\sw+\\)[ 	]*:\\($\\|[^:]\\)"
	  (beginning-of-line)
	  (end-of-line)
	  (1 font-lock-constant-face)))
	("\\<\\(asm\\|break\\|c\\(atch\\|on\\(st_cast\\|tinue\\)\\)\\|d\\(elete\\|o\\|ynamic_cast\\)\\|else\\|for\\|if\\|new\\|re\\(interpret_cast\\|turn\\)\\|s\\(izeof\\|tatic_cast\\|witch\\)\\|t\\(h\\(is\\|row\\)\\|ry\\)\\|while\\)\\>"
	 (0 font-lock-keyword-face))
	("\\<\\(false\\|true\\)\\>"
	 (0 font-lock-constant-face))
	("\\<\\(auto\\|bool\\|c\\(har\\|o\\(mplex\\|nst\\)\\)\\|double\\|e\\(num\\|x\\(p\\(licit\\|ort\\)\\|tern\\)\\)\\|f\\(loat\\|riend\\)\\|in\\(line\\|t\\)\\|long\\|mutable\\|namespace\\|register\\|s\\(hort\\|igned\\|t\\(atic\\|ruct\\)\\)\\|t\\(emplate\\|ypedef\\)\\|u\\(n\\(ion\\|signed\\)\\|sing\\)\\|v\\(irtual\\|o\\(id\\|latile\\)\\)\\|JBF[a-zA-Z0-9_]*\\|eZ[a-zA-Z0-9_]*\\|Q[a-zA-Z_]*\\|uint\\|ulong\\|string\\)\\>\\([ 	]*<\\([^>\n]+\\)[ 	*&]*>\\)?\\([ 	]*::[ 	*~]*\\(\\sw+\\)\\)*\\([ 	*&]+\\(\\sw+\\)\\>\\([ 	]*<\\([^>\n]+\\)[ 	*&]*>\\)?\\([ 	]*::[ 	*~]*\\(\\sw+\\)\\)*\\)*"
	 (font-lock-match-c++-style-declaration-item-and-skip-to-next
	  (goto-char
	   (or
	    (match-beginning 20)
	    (match-end 1)))
	  (goto-char
	   (match-end 1))
	  (1
	   (cond
	    ((or
	      (match-beginning 2)
	      (match-beginning 4))
	     font-lock-type-face)
	    ((match-beginning 6)
	     font-lock-function-name-face)
	    (t font-lock-variable-name-face)))
	  (3 font-lock-function-name-face nil t)
	  (5
	   (if
	       (match-beginning 6)
	       font-lock-function-name-face font-lock-variable-name-face)
	   nil t)))
	("\\(}\\)[ 	*]*\\sw"
	 (font-lock-match-c++-style-declaration-item-and-skip-to-next
	  (goto-char
	   (match-end 1))
	  nil
	  (1
	   (if
	       (match-beginning 6)
	       font-lock-function-name-face font-lock-variable-name-face))))
	("^\\(\\(\\sw+\\)\\>\\([ 	]*<\\([^>\n]+\\)[ 	*&]*>\\)?\\([ 	]*::[ 	*~]*\\(\\sw+\\)\\)*[ 	*&]*\\)+"
	 (font-lock-match-c++-style-declaration-item-and-skip-to-next
	  (goto-char
	   (match-beginning 1))
	  (goto-char
	   (match-end 1))
	  (1
	   (cond
	    ((or
	      (match-beginning 2)
	      (match-beginning 4))
	     font-lock-type-face)
	    ((match-beginning 6)
	     font-lock-function-name-face)
	    (t font-lock-variable-name-face)))
	  (3 font-lock-function-name-face nil t)
	  (5
	   (if
	       (match-beginning 6)
	       font-lock-function-name-face font-lock-variable-name-face)
	   nil t)))
	("[{}()<>=;:+\\*\\/\\[]\\|\\]\\|\\-" (0 font-lock-keys-face))
	("\\<[0-9]+\\>" (0 font-lock-number-face))
	("\\<0x[0-9a-fA-F]+\\>" (0 font-lock-hexnumber-face))
					;     ((concat "\\<"
					; 	     (regexp-opt '("Q_OBJECT" "emit" "connect" "disconnect" "SIGNAL" "SLOT" "Q_EXPORT"))
					; 	     "\\>" )
					;      (0 font-lock-qt-face))
	("\\<\\(Q_\\(EXPORT\\|OBJECT\\|PROPERTY\\)\\|S\\(IGNAL\\|LOT\\)\\|connect\\|disconnect\\|emit\\)\\>"
	 (0 font-lock-qt-face))
	)))

; Auto-insert text when making new *.cpp, *.cc, *.h files.
(add-hook 'find-file-hooks 'auto-insert)


; If you create a file called Test.php, this function will replace:
;
;   @@@ with TEST
;   ||| with Test
;   !!! with test
;

(defun auto-update-php-file ()
  (let (classname)
    (setq classname (read-from-minibuffer "Enter name of PHP class (Enter for default): "
					(file-name-sans-extension
					 (file-name-nondirectory buffer-file-name))))
    (project-replace-class-name (current-buffer) classname "")))

; If you create a file called Test.hpp, this function will replace:
;
;   @@@ with TEST
;   ||| with Test
;   !!! with test
;
; The first one is useful for #ifdefs, the second one for the header
; description, for example.

(defun auto-update-header-file ()
  (if project-use-auto-insert
      (let ()
	(save-excursion
	  (while (search-forward project-upcase-name-match nil t)
	    (save-restriction
	      (narrow-to-region (match-beginning 0) (match-end 0))
	      (replace-match
	       (upcase
		(file-name-sans-extension
		 (file-name-nondirectory buffer-file-name)))))))
	(save-excursion
	  (while (search-forward project-normal-name-match nil t)
	    (save-restriction
	      (narrow-to-region (match-beginning 0) (match-end 0))
	      (replace-match
	       (file-name-sans-extension
		(file-name-nondirectory buffer-file-name))))))
	(save-excursion
	  (while (search-forward project-downcase-name-match nil t)
	    (save-restriction
	      (narrow-to-region (match-beginning 0) (match-end 0))
	      (replace-match
	       (downcase
		(file-name-sans-extension
		 (file-name-nondirectory buffer-file-name)))))))
	(save-excursion
	  (while (search-forward "\<real-name\>" nil t)
	    (save-restriction
	      (narrow-to-region (match-beginning 0) (match-end 0))
	      (replace-match user-full-name))))
	(save-excursion
	  (while (search-forward "\<login-name\>" nil t)
	    (save-restriction
	      (narrow-to-region (match-beginning 0) (match-end 0))
	      (replace-match user-login-name))))
	(save-excursion
	  (while (search-forward "\<mail-name\>" nil t)
	    (save-restriction
	      (narrow-to-region (match-beginning 0) (match-end 0))
	      (replace-match project-mail-account)))))))

; If you create a file called Test.pro, this function will replace:
;
;   @@@ with test
;   ||| with Test
;   !!! with test
;
; It will also create a directory for objects if a OBJECTS_DIR is present

(defun auto-update-project-file ()
  (if project-use-auto-insert
      (let ()
	(save-excursion
	  (while (search-forward project-upcase-name-match nil t)
	    (save-restriction
	      (narrow-to-region (match-beginning 0) (match-end 0))
	      (replace-match
	       (upcase
		(file-name-sans-extension
		 (file-name-nondirectory buffer-file-name)))))))
	(save-excursion
	  (while (search-forward project-normal-name-match nil t)
	  (save-restriction
	    (narrow-to-region (match-beginning 0) (match-end 0))
	    (replace-match
	     (file-name-sans-extension
	      (file-name-nondirectory buffer-file-name))))))
	(save-excursion
	  (while (search-forward project-downcase-name-match nil t)
	  (save-restriction
	    (narrow-to-region (match-beginning 0) (match-end 0))
	    (replace-match
	     (downcase
	      (file-name-sans-extension
	       (file-name-nondirectory buffer-file-name)))))))
	(save-excursion
	  (while (search-forward "\<real-name\>" nil t)
	  (save-restriction
	    (narrow-to-region (match-beginning 0) (match-end 0))
	    (replace-match user-full-name))))
	(save-excursion
	  (while (search-forward "\<login-name\>" nil t)
	  (save-restriction
	    (narrow-to-region (match-beginning 0) (match-end 0))
	    (replace-match user-login-name))))
	(save-excursion
	  (while (search-forward "\<mail-name\>" nil t)
	  (save-restriction
	    (narrow-to-region (match-beginning 0) (match-end 0))
	    (replace-match project-mail-account))))
	(save-excursion
	  (while (search-forward "OBJECTS_DIR" nil t)
	  (save-excursion
	    (search-forward "=" nil t)
	    (save-restriction
	      (if (search-forward-regexp "[ \t]*\\([a-zA-Z]+\\)[ \t]*$" nil t)
		  (if (not (file-exists-p (match-string 1 )))
		      (make-directory (match-string 1))))))))
	(save-excursion
	  (while (search-forward "MOC_DIR" nil t)
	  (save-excursion
	    (search-forward "=" nil t)
	    (save-restriction
	      (if (search-forward-regexp "[ \t]*\\([a-zA-Z]+\\)[ \t]*$" nil t)
		  (if (not (file-exists-p (match-string 1 )))
		      (make-directory (match-string 1))))))))
	(save-buffer)
	(shell-command (concat "tmake -o Makefile " (file-relative-name (buffer-file-name buffer-file-name) (pwd)))))))

;; Replaces the creation-tag with the current date/time and user
(defun insert-creation-date ()
  "Inserts the date of the creation if it finds the keyword."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (if (search-forward "\<creation-tag\>" nil t)
	(save-restriction
	  (narrow-to-region (match-beginning 0) (match-end 0))
	  (replace-match (concat "Created on: <"
				 (time-stamp-string)
				 ">")))))
  nil)

(add-hook 'write-file-hooks 'insert-creation-date)

; Scans a .hpp .h or .hh file when saved for the keyword Q_OBJECT
; if found then checks if the moc file exists for the the .cpp file
; if not runs tmake on the project file
(defun c++-moc-file ()
  "Runs tmake on the project if signals/slots has been added to the c++ header."
  (interactive)
  (if (string-match c++-header-ext-regexp (buffer-name))
      (save-excursion
	(beginning-of-buffer)
	(let ((filedir (file-relative-name (file-name-directory (buffer-file-name))))
	      (filenonext (file-name-sans-extension (file-relative-name (file-name-directory (buffer-file-name))))))
	  (if (search-forward-regexp "\\<Q_OBJECT\\>" nil t)
	      (progn
		(set-buffer (project-main))
		(save-buffer)
		(if (not (file-exists-p (concat filedir	"moc_" filenonext "." c++-default-source-ext)))
		    (shell-command (concat "tmake -o " filedir "Makefile " (file-name-nondirectory (buffer-file-name (project-main)))) )))
	    (save-restriction
	      (setq filename (concat filedir "moc_" filenonext "." c++-default-source-ext))
	      (if (file-exists-p filename)
		  (progn
		    (set-buffer (project-main))
		    (save-buffer)
		    (shell-command (concat "tmake -o " filedir "Makefile " (file-name-nondirectory (buffer-file-name (project-main)))))
		    (delete-file filename))))))))
  nil)

; (add-hook 'after-save-hook 'c++-moc-file)

;; Inserts a Q_OBJECT in a c++ header file if slots of signals are used
(defun buffer-insert-qobject()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (if (search-forward-regexp (regexp-opt '("slots" "signals") t) nil t)
	(let ()
	  (beginning-of-buffer)
	  (if (search-forward-regexp (concat "^\\(template[ \t]*<[^>]+>[ \t]*\\)?class[ \t]+\\([a-zA-Z0-9_]+\\)[ \t\n]*"
					     "\\([:][ \t\n]*\\(public\\|protected\\|private\\)?[ \t\n]*\\<[a-zA-Z0-9_]+\\>\\)?"
					     "[ \t\n]*{")
				     nil t)
	      (if (not (looking-at "[ \t\n]*\\(Q_OBJECT\\)"))
		  (insert-string "\n\tQ_OBJECT"))
	    (ding))))))

;(add-hook 'write-file-hooks '(lambda ()
;			       (interactive)
;			       (if (string-match c++-header-ext-regexp (buffer-name))
;				   (buffer-insert-qobject))))

;; Adds index to the menu for lisp and c/c++ modes.
(defun project-add-index ()
  (if (and (emacs-type-is-regular)
	   (or (string-match mode-name "Emacs-Lisp")
	       (string-match mode-name "C++")
	       (string-match mode-name "C")))
      (imenu-add-menubar-index)))

;; Creates the index after the file has been loaded.
(add-hook 'find-file-hooks 'project-add-index)

;; Automaticly rescan the index
;(setq imenu-auto-rescan t)


;; Add Time-stamp <> or Time-stamp " " anywhere in the top 8 lines of a
;; file to insert save date and time and user:

(add-hook 'write-file-hooks 'time-stamp)

;---------------------------------------------------------------------
;I'd like to enable ispell check for text-mode too...

(setq default-major-mode 'indented-text-mode)
(add-hook 'indented-text-mode-hook 'turn-on-auto-fill)


; (add-hook 'server-visit-hook '(lambda ()
; 				(interactive)
; 				(yes-or-no-p "Hello there")))
(add-hook 'server-switch-hook 'make-frame-command)
; (add-hook 'server-done-hook '(lambda ()
; 			       (interactive)
; 			       (if (is-buffer-a-client)
; 				   (delete-frame))))

;Faces
; (set-face-foreground 'region "black")
; (set-face-background 'highlight "CadetBlue")
; (set-face-background 'secondary-selection "MediumSeaGreen")


;; New faces
(defvar font-lock-number-face 'font-lock-number-face)
(defvar font-lock-hexnumber-face 'font-lock-hexnumber-face)
(defvar font-lock-floatnumber-face 'font-lock-floatnumber-face)
(defvar font-lock-keys-face 'font-lock-keys-face)
(defvar font-lock-qt-face 'font-lock-qt-face)

(defface font-lock-number-face
  '(
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:background "hotpink"))
    (((class color) (background dark)) (:foreground "black" :background "hotpink"))
    (t (:italic t)))
  "blah"
  :group 'font-lock-highlighting-faces)

(defface font-lock-hexnumber-face
  '(
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:background "darkblue"))
    (((class color) (background dark)) (:foreground "black" :background "darkblue"))
    (t (:italic t)))
  "blah"
  :group 'font-lock-highlighting-faces)

(defface font-lock-floatnumber-face
  '(
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:background "darkgreen"))
    (((class color) (background dark)) (:foreground "black" :background "darkgreen"))
    (t (:italic t)))
  "blah"
  :group 'font-lock-highlighting-faces)

(defface font-lock-keys-face
  '(
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:background "yellow"))
    (((class color) (background dark)) (:foreground "black" :background "yellow"))
    (t (:italic t)))
  "blah"
  :group 'font-lock-highlighting-faces)

(defface font-lock-qt-face
  '(
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:background "brown"))
    (((class color) (background dark)) (:foreground "green" :background "brown"))
    (t (:italic t)))
  "blah"
  :group 'font-lock-highlighting-faces)

;; First try to load default colors
(if (file-exists-p "~/.myshellsetup/.emacs-d-colors")
    (load-file "~/.myshellsetup/.emacs-d-colors"))

;; Then override with local colors
(if (file-exists-p ".emacs-colors")
    (load-file ".emacs-colors"))

;; Set the new size
;(set-frame-width (car (car (cdr (current-frame-configuration)))) default-frame-width)
;(set-frame-height (car (car (cdr (current-frame-configuration)))) default-frame-height)

; Start with the same buffers, major modes and buffer positions:
; You must do a M-x desktop-save the first time it's used. Emacs
; must be started in the same current directory.

(load "desktop")
(desktop-load-default)
(desktop-read)

;; Options Menu Settings
;; =====================
(cond
 ((and (string-match "XEmacs" emacs-version)
       (boundp 'emacs-major-version)
       (or (and
            (= emacs-major-version 19)
            (>= emacs-minor-version 14))
           (= emacs-major-version 20))
       (fboundp 'load-options-file))
  (load-options-file "/root/.xemacs-options")))
;; ============================
;; End of Options Menu Settings

;; Load the extras file
(if (file-exists-p ".emacs-extras")
    (load-file ".emacs-extras"))

;; returns t if the current buffer is an emacs-client file
(defun is-buffer-a-client ()
  (interactive)
  (let ((cls server-clients)
	cl
	bufs
	buf
	(ok nil))
    (while cls
      (setq cl (car cls))
      (setq bufs (cdr cl))
      (while bufs
	(setq buf (car bufs))
	(if (eq buf (current-buffer))
	    (setq ok t))
	(setq bufs (cdr bufs)))
      (setq cls (cdr cls)))
    ok))

;; This starts emacs as a server.
;;(server-start)

;; Use gnu serv instead if available
(if (if (emacs-type-is-regular)
	(require 'gnuserv nil t)
      (require 'gnuserv))
    (gnuserv-start)
  (server-start))

;;
;; Rebind mouse-2 events to mouse-1 in various places:
;; Completion list
(add-hook 'completion-list-mode-hook
  '(lambda() (define-key completion-list-mode-map [down-mouse-1] 
	       'mouse-choose-completion)))
;; TexInfo
(add-hook 'Info-mode-hook
  '(lambda() (define-key Info-mode-map [down-mouse-1] 
	       'Info-mouse-follow-nearest-node)))
;; Buffer Menu
(add-hook 'buffer-menu-mode-hook
  '(lambda() (define-key Buffer-menu-mode-map [down-mouse-1] 
	       'Buffer-menu-mouse-select)))

;; Add a new menu element
;;(define-key-after (lookup-key global-map [menu-bar edit])
;;  [startup] '("Toggle Truncate Lines" . (lambda () (interactive)
;;					(setq-default truncate-lines (not truncate-lines)))) [calendar])

;; If non-nil each line of text is exactly one screen line, else wrap text.
(setq-default truncate-lines nil)

;;(require 'speedbar)

;;(define-key-after (lookup-key global-map [menu-bar tools])
;;  [speedbar] '("Speedbar" . speedbar-frame-mode) [calendar])

(setq imenu-always-use-completion-buffer-p t)


(defun change-var-in-file( var file val )
  "Changes the variable named var in the given file with the given val and saves it"
  (let (buf)
    (save-excursion
      (setq buf (find-file-noselect file))
      (set-buffer buf)
      (beginning-of-buffer)
      (if (search-forward-regexp (concat "^(defvar[ \t]+"
					 var
					 "[ \t]+\\(t\\|nil\\))")
				 nil t)
	  (save-restriction
	    (narrow-to-region (match-beginning 1) (match-end 1))
	    (replace-match val t nil nil 1)
	    (save-buffer))))))

;; Adds support for mouse wheel
; (if ask-for-mwheel
;     (if (not (featurep 'mwheel))
;  	(let ((ask nil))
;  	  (if (y-or-n-p "Do you want mouse wheel support? ")
;  	      (setq ask t))
; 	  (if ask
; 	      (require 'mwheel))
; 	  (if (not (y-or-n-p "Do you want this question next time? "))
; 	      (let ()
; 		(setq ask-for-mwheel nil)
; 		(change-var-in-file "ask-for-mwheel" "~/.myshellsetup/.emacs-d-custom" "nil")))
; 	  (let (val)
; 	    (if ask
; 		(setq val "t")
; 	      (setq val "nil"))
; 	    (change-var-in-file "do-require-mwheel" "~/.myshellsetup/.emacs-d-custom" val))))
;   (if do-require-mwheel
;       (require 'mwheel nil t)))

;; Set the wanted color style
(change-color-style default-color-style)

;; (c-set-style "ezsystems")

;; HTML/SGML related stuff

;; DocBook IDE mode
(autoload 'docbook-mode "docbookide" "Major mode for DocBook documents." t)

;; Turn on font lock when in DocBook mode
(add-hook 'docbook-mode-hook
	  'turn-on-font-lock)

;; You might want to make this the default for .sgml or .xml documents,
;; or you might want to rely on -*- DocBook -*- on the first line,
;; or perhaps buffer variables. It's up to you...
(setq auto-mode-alist
      (append
       (list
	'("\\.sgm" . docbook-mode)
	'("\\.sgml" . docbook-mode)
;;	'("\\.xml" . docbook-mode)
;;	'("\\.html" . html-mode)
	'("\\.ptl" . html-mode))
       auto-mode-alist))

(setq auto-mode-alist
        (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\)\\'" . nxml-mode)
              auto-mode-alist))


(setq make-backup-files nil)

(require 'cua-base)
(cua-mode t)

;;
;; ADO-Mode for Stata Files
;;
;; Tell emacs where the ado-mode is
;;  Change /Universal/Custom/emacs to the place which actually holds the ado-mode
;;  folder. Leave the /lisp at the end of the path!
(setq load-path (cons "~/.myshellsetup/.site-lisp/ado-mode/lisp" load-path))

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
(setq ado-site-template-dir "~/.myshellsetup/.site-lisp/ado-mode/templates/") 

;; Tell emacs to auto-load the ado-mode, so that it is available 
(load-library "ado-mode") 

;;   this gets used when making help files. 
(setq ado-claim-name "Joshua Krall") 

;; Copy the ado-signature-template to a location of your choosing, 
;;   change the information, and store it in the file like .ado-signature 
;; Once again, this is definitely for individuals. 
(setq ado-signature-file "~/.myshellsetup/.site-lisp/ado-mode/.ado-signature") 

;;; Optional but Highly Recommended Additions 

;; Tell the ado-mode the directory which will hold new, untested Stata programs. 
;; It should be a directory on your adopath within Stata, so that Stata finds
;;  your new ados.
(setq ado-new-dir "/Programming/Stata") 

;; A directory to store good value labels. This could be shared across a group
;;; This should also be in your adopath within Stata
(setq ado-label-dir "/Programming/Stata/labels/") 

;;; Optional Additions for the Person Who Wants to Alter the ado-mode 
;;;    Needed if you ever want to edit the source for the ado-mode (ado-mode.el), 
;;;   and have it byte-compile properly. 
;;;

;; Autoload the make-regexp and make-regexps functions for easier changes to the auto-highlighting 
(autoload 'make-regexp "make-regexp" 
"Return a regexp to match a string item in STRINGS.") 
(autoload 'make-regexps "make-regexp" 
"Return a regexp to REGEXPS.") 




