;; ado-mode.el --- ado mode, and its idiosyncratic commands.

;; Copyright (C) 1999,..., 2006 Bill Rising

;; Maintainer: Bill Rising, brising@louisville.edu
;; Keywords: ado-mode, highlighting
;; Version: 0.91.2 of March 24, 2006

;; This file is NOT part of GNU Emacs.

;; This ado-mode is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This ado-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this ado-mode; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;; This package provides the ado mode *NOT* documented in the
;; Emacs user's manual.
;; Much of this code originated with the C-mode, but has changed
;; quite a bit since.
;; To add your own additions to this, you will also need the
;; make-regexp package, a copy of which is located with this file
;; in Bill's Current Pile of Stuff
;;  ftp://erdos.math.louisville.edu/pub/brising
;;
;;; Code:

;;; required files
(require 'font-lock)
(require 'ado-font)
(require 'ado-cus)

;;; putting in the proper extensions for using the ado-mode
(if (assoc "\\.ado$" auto-mode-alist) nil
  (setq auto-mode-alist (append auto-mode-alist
				(list '("\\.ado\\'" . ado-mode)
				      '("\\.lbl\\'" . ado-mode)
				      '("\\.do\\'"  . ado-mode)	;could be a problem
				      '("\\.hlp\\'" . ado-mode) ; will be a problem for those programming in MS WinWhatever
				      '("\\.dlg\\'" . ado-mode)
				      '("\\.smcl\\'" . ado-mode) 
				      '("\\.ADO\\'" . ado-mode) ; for MS-DOG files
				      '("\\.LBL\\'" . ado-mode)
				      '("\\.DLG\\'" . ado-mode)
				      '("\\.DO\\'"  . ado-mode)
				      '("\\.HLP\\'" . ado-mode)
				      '("\\.SMCL\\'" . ado-mode) 
				      ))))

(defvar ado-font-lock-keywords nil)
(defvar ado-font-lock-syntactic-keywords nil)
(defvar ado-extension nil)

;; abbrev table
(defvar ado-mode-abbrev-table nil
  "Abbrev table used while in ado mode.")
(define-abbrev-table 'ado-mode-abbrev-table ())

;; syntax table
(defvar ado-mode-syntax-table nil
  "Syntax table used while in ado mode.")
(if ado-mode-syntax-table
    ()
  (setq ado-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "." ado-mode-syntax-table) ;nullify escape meaning
  (modify-syntax-entry ?\$ "." ado-mode-syntax-table)
;; commented out, because ' can be used too many places, now.
;  (modify-syntax-entry ?` "(\'" ado-mode-syntax-table)
;  (modify-syntax-entry ?\' ")`" ado-mode-syntax-table)
  (modify-syntax-entry ?/ ". 124b" ado-mode-syntax-table)
  (modify-syntax-entry ?* ". 23n" ado-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" ado-mode-syntax-table)
  (modify-syntax-entry ?+ "_" ado-mode-syntax-table)
  (modify-syntax-entry ?- "_" ado-mode-syntax-table)
  (modify-syntax-entry ?= "_" ado-mode-syntax-table)
  (modify-syntax-entry ?% "." ado-mode-syntax-table)
  (modify-syntax-entry ?< "." ado-mode-syntax-table)
  (modify-syntax-entry ?> "." ado-mode-syntax-table)
  (modify-syntax-entry ?& "." ado-mode-syntax-table)
  (modify-syntax-entry ?| "." ado-mode-syntax-table)
  (modify-syntax-entry ?~ "." ado-mode-syntax-table)
  ;; added this, because underscores really are parts of words
  (modify-syntax-entry ?_ "w" ado-mode-syntax-table)
  ;; an attempt to fix embedded quote highlighting problems
  ;; fails because string starts and ends can be only single characters
;  (modify-syntax-entry ?` "| 1b" ado-mode-syntax-table)
;  (modify-syntax-entry ?' "| 4b" ado-mode-syntax-table)
;  (modify-syntax-entry ?\" "| 23b" ado-mode-syntax-table)
  )

;;; keymap definitions
(defvar ado-mode-map (make-sparse-keymap)
  "Keymap for Ado mode." )
(define-key ado-mode-map "\t"       'ado-indent-line)
(define-key ado-mode-map "\M-\C-m"  'ado-split-line)
(define-key ado-mode-map "\M-a"     'ado-beginning-of-command)
(define-key ado-mode-map "\M-e"     'ado-end-of-command)
(define-key ado-mode-map "\C-c\C-f" 'ado-foreach-loop)
(define-key ado-mode-map "\C-c\C-n" 'ado-new-program)
(define-key ado-mode-map "\C-c\C-i" 'ado-insert-new-program)
(define-key ado-mode-map "\C-c\C-l" 'ado-new-label)
(define-key ado-mode-map "\C-c\C-d" 'ado-help-file)
(define-key ado-mode-map "\C-c;"    'comment-region)
(define-key ado-mode-map "\C-c:"    'uncomment-region)
(define-key ado-mode-map "\C-x\C-s" 'ado-save-program)
(define-key ado-mode-map "{"        'electric-ado-brace)
(define-key ado-mode-map "}"        'electric-ado-closing-brace)
(define-key ado-mode-map ";"        'electric-ado-semi)

;;; menu bar definitions!
(define-key ado-mode-map [menu-bar] (make-sparse-keymap))
(define-key ado-mode-map [menu-bar ado]
  (cons "Ado-mode" (make-sparse-keymap "Ado-mode")))
(define-key ado-mode-map [menu-bar ado options]
  (cons "Options" (make-sparse-keymap "options")))
(define-key ado-mode-map [menu-bar ado indent-buffer]
  '("Indent Buffer" . ado-indent-buffer))
(define-key ado-mode-map [menu-bar ado uncomment-region]
  '("Uncomment Region" . uncomment-region))
(define-key ado-mode-map [menu-bar ado comment-region]
  '("Comment Out Region" . comment-region))
(define-key ado-mode-map [menu-bar ado ado-foreach-loop]
  '("Foreach loop" . ado-foreach-loop))
(define-key ado-mode-map [menu-bar ado new]
  (cons "New program" (make-sparse-keymap "new")))
;; place for customizations
(define-key ado-mode-map [menu-bar files save-buffer]
  '("Save buffer" . ado-save-program))

;; submenu New
(define-key ado-mode-map [menu-bar ado new ado-help-file]
  '("Help file" . ado-help-file))
(define-key ado-mode-map [menu-bar ado new ado-new-label]
  '("Label file" . ado-new-label))
(define-key ado-mode-map [menu-bar ado new ado-insert-new-program]
  '("Insert new subprogram" . ado-insert-new-program))
(define-key ado-mode-map [menu-bar ado new ado-new-program]
  '("Generic new program" . ado-new-program))

;; submenu Options
;;; this submenu follows
(define-key ado-mode-map [menu-bar ado options special-indentation]
  (cons "Special Indentation" (make-sparse-keymap "special-indentation")))

(define-key ado-mode-map [menu-bar ado options ado-confirm-overwrite-toggle]
  '(menu-item "Confirm File Overwrite"
	      (lambda () (interactive) (ado-toggle-flag 'ado-confirm-overwrite-flag))
	      :button (:toggle . ado-confirm-overwrite-flag)))

(define-key ado-mode-map [menu-bar ado options ado-comment-column-change]
  '(menu-item "Set Comment Column..." 
	      (lambda () (interactive) ado-change-number 'ado-comment-column 'ask)))

(define-key ado-mode-map [menu-bar ado options ado-continued-statement-indent-spaces-change]
  '(menu-item "Set Continuation Indentation..." ado-continued-statement-indent-spaces-change))

(define-key ado-mode-map [menu-bar ado options ado-tab-width-change]
  '(menu-item "Set Tab Width..." ado-tab-width-change))

(define-key ado-mode-map [menu-bar ado options ado-fontify-new-toggle]
  '(menu-item "Fontify New Ado Files" 
	      (lambda () (interactive) (ado-toggle-flag 'ado-fontify-new-flag))
	      :button (:toggle . ado-fontify-new-flag)))

(define-key ado-mode-map [menu-bar ado options ado-auto-newline-toggle]
  '(menu-item "Automatic New Line" 
	      (lambda () (interactive) (ado-toggle-flag 'ado-auto-newline-flag))
	      :button (:toggle . ado-auto-newline-flag)))

(define-key ado-mode-map [menu-bar ado options ado-closing-brace-alone-toggle]
  '(menu-item "Closing Brace Alone" 
	      (lambda () (interactive) (ado-toggle-flag 'ado-closing-brace-alone-flag))
	      :button (:toggle . ado-closing-brace-alone-flag)))

(define-key ado-mode-map [menu-bar ado options ado-close-under-line-toggle]
  '(menu-item "Close Under Line"
	      (lambda () (interactive) (ado-toggle-flag 'ado-close-under-line-flag))
	      :button (:toggle . ado-close-under-line-flag)))

(define-key ado-mode-map [menu-bar ado options ado-use-modern-split-toggle]
  '(menu-item "Use Modern Line-split"
	      (lambda () (interactive) (ado-toggle-flag 'ado-use-modern-split-flag))
	      :button (:toggle . ado-use-modern-split-flag)))

(define-key ado-mode-map [menu-bar ado options ado-do-indent-toggle]
  '(menu-item "Indent Do Files"
	      (lambda () (interactive) (ado-toggle-flag 'ado-do-indent-flag))
	      :button (:toggle . ado-do-indent-flag)))

;; needs its own toggling function, because keymaps must be changed.
(define-key ado-mode-map [menu-bar ado options ado-return-also-indents-toggle]
  '(menu-item "Return also Indents" ado-return-toggle
	      :button (:toggle . ado-return-also-indents-flag)))

(define-key ado-mode-map [menu-bar ado options ado-smart-indent-toggle]
  '(menu-item "Smart Indent"
	      (lambda () (interactive) (ado-toggle-flag 'ado-smart-indent-flag))
	      :button (:toggle . ado-smart-indent-flag)
;	      :help "This is some help, ain't it?"
	      ))

;(define-key ado-mode-map [menu-bar ado options div1]
;  '(menu-item "--shadow-etched-in"))
;(define-key ado-mode-map [menu-bar ado options title]
;  '(menu-item "Check or Uncheck"))
;  '("Toggle smart indent" . ado-smart-indent-toggle))

;; subsubmenu Options/Special Indent
(define-key ado-mode-map [menu-bar ado options special-indentation ado-change-comment-indent]
  '(menu-item "Change comment indent column..." 
	      (lambda () (interactive) (ado-change-number 'ado-comment-indent-column 'ask))
	      :enable ado-delimit-indent-flag))

(define-key ado-mode-map [menu-bar ado options special-indentation ado-comment-indent-flag-toggle]
  '(menu-item "Comment column indentation" 
	      (lambda () (interactive) (ado-toggle-flag 'ado-comment-indent-flag))
 	      :button (:toggle . ado-comment-indent-flag)))

(define-key ado-mode-map [menu-bar ado options special-indentation ado-change-delimit-indent]
  '(menu-item "Change #delimit column..." 
	      (lambda () (interactive) (ado-change-number 'ado-delimit-indent-column 'ask))
	      :enable ado-delimit-indent-flag))

(define-key ado-mode-map [menu-bar ado options special-indentation ado-delimit-indent-flag-toggle]
  '(menu-item "#delimit indented differently" 
	      (lambda () (interactive) (ado-toggle-flag 'ado-delimit-indent-flag))
 	      :button (:toggle . ado-delimit-indent-flag)))

(define-key ado-mode-map [menu-bar ado options special-indentation ado-change-debugging-indent]
  '(menu-item "Change debugging column..." 
	      (lambda () (interactive) (ado-change-number 'ado-debugging-indent-column 'ask))
	      :enable ado-debugging-indent-flag))

(define-key ado-mode-map [menu-bar ado options special-indentation ado-debugging-indent-flag-toggle]
  '(menu-item "debugging indented differently" 
	      (lambda () (interactive) (ado-toggle-flag 'ado-debugging-indent-flag))
 	      :button (:toggle . ado-debugging-indent-flag)))

;; initial mode defintion function
(defun ado-mode ()
  "Major mode for editing ado, do, hlp, dlg, and smcl files for
use in the Stata statistical package. It indents blocks of
code properly, highlights command names, many keywords, some
more complicated command structures, strings, Stata macro names 
and the like.

If you downloaded the template folder (directory) which came with this
distribution, you can use this mode to create ado files (programs)
 and help files. To change the templates, edit the
files in the template directory which have .blp extensions. (blp stands
for boilerplate)

The mode comes with a menu (the Ado-mode menu) which shows most all of the
variables which are worth changing locally in a buffer. Global customization
can be done via '\\[customize-group] ado-mode' using emacs customization
routines. More suggestions can be found at 
http://faculty.bellarmine.edu/~wrising/Stata/ado-mode_install.html

Here is a short list of the common commands which come with the mode:
Things for dealing with files:
- \\[ado-new-program] will make a new buffer ready for a new ado file.
- \\[ado-save-program] will save the current buffer and give it a good
    time stamp. Ensures that the file name matches the name of the
    command (ado program) or class being defined.
- \\[ado-help-file] will start a new help file, ready for editing.
Things for changing style:
Most of these would be most easily done using emacs' ability to customize
its enviroment using \\[customize-group ado-mode]. Other little things
 are
- \\[ado-tab-width-change] will change the tab-width for the current buffer.
- \\[ado-toggle-flag] which asks for the name of a flag to toggle. Even
    easier: use the Options... submenu of the Ado-mode menu..
Moving about indenting
- \\[ado-indent-buffer] will re-indent the whole buffer.
Things for special Stata manipulations
- \\[ado-beginning-of-command] will move the point back to the beginning
    of the current command. If in the whitespace between two commands, it will
    move to the start of the next command.
- \\[ado-split-line] will split a long line in two using either the /* */ or ///
    style comments, depending on the value of ado-use-modern-split-flag (which
    defaults to on, implying the use of ///).
- \\[ado-foreach-loop] will insert a foreach loop, asking in the minibuffer
    for the particulars.
- \\[ado-insert-new-program] puts a new subprogram at the bottom of the current
    buffer for use within the current ado file.
- \\[comment-region] will comment out an entire region.
- \\[uncomment-region] will uncomment the region. Be careful if some of the region
    is not actually commented!

Here are some rather esoteric command which I've not yet documented well,
because they correspond to my personal ways of working on large projects in 
Stata:
- \\[ado-project-program] will create a program which runs subprograms,
    which is quite nice for large projects.
- \\[ado-stage-program] will create a new ado file made to act as a 
    single stage in a larger project. Warning: if you use this, you
    will need to get the slog package from Bill Rising's packages.
- \\[ado-new-label] will make a new label file useful for storing commonly
    used value labels.

Most all of the commands are accessible from the ado-mode menu.

If you also use ESS (Emacs Speaks Statistics), but you would rather
use this ado-mode to code Stata, include the following in your .emacs
file:

 (setq auto-mode-alist 
      (append (list '(\"\\\\.ado\\\\'\" . ado-mode)
		    '(\"\\\\.do\\\\'\"  . ado-mode)
		    )
	      auto-mode-alist
	      ))

This will make ado-mode load when you open an ado or do file."
;; standard variables for any mode
  (interactive)
  (kill-all-local-variables)
  (use-local-map ado-mode-map)
  (define-abbrev-table 'ado-mode-abbrev-table ())
  (setq local-abbrev-table ado-mode-abbrev-table)
  (set-syntax-table ado-mode-syntax-table)
  (make-local-variable 'ado-return-also-indents-flag)
  (ado-set-return ado-return-also-indents-flag)
  ;; indentation and paragraph definitions
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'ado-indent-line)
;  (make-local-variable 'indent-region-function)
;  (setq indent-region-function 'ado-indent-function)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  ;; comment definitions
  (make-local-variable 'comment-start)
  (setq comment-start "/* ")
  (make-local-variable 'comment-end)
  (setq comment-end " */")
  (make-local-variable 'comment-column)
  (setq comment-column ado-comment-column)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+ *")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'ado-comment-indent)
  (make-local-variable 'comment-multi-line)
  (setq comment-multi-line nil)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  ;; make local copies of all the style stuff
  (make-local-variable 'ado-smart-indent-flag)
  (make-local-variable 'ado-comment-column)
  (make-local-variable 'ado-tab-width)
  (setq tab-width ado-tab-width)
  (make-local-variable 'ado-confirm-overwrite-flag)
  (make-local-variable 'ado-use-modern-split-flag)
  (make-local-variable 'ado-delimit-indent-flag)
  (make-local-variable 'ado-delimit-indent-column)
  (make-local-variable 'ado-comment-indent-flag)
  (make-local-variable 'ado-comment-indent-column)
  (make-local-variable 'ado-debugging-indent-flag)
  (make-local-variable 'ado-debugging-indent-column)
  (make-local-variable 'ado-fontify-new-flag)
  (make-local-variable 'ado-close-under-line-flag)
  (make-local-variable 'ado-continued-statement-indent-spaces)
  (make-local-variable 'ado-auto-newline-flag)
  (make-local-variable 'ado-closing-brace-alone-flag)
  ;; delete auto-save-file when file is saved for real
  (make-local-variable 'delete-auto-save-files)
  (setq delete-auto-save-files t)
  (use-local-map ado-mode-map)
  (setq mode-name "Ado")
  (setq major-mode 'ado-mode)
  ;; make sure function ends with lf
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'font-lock-defaults)
  (ado-set-font-lock-keywords)
  (setq font-lock-defaults '(ado-font-lock-keywords))
  (make-local-variable 'font-lock-syntactic-keywords)
  (ado-set-font-lock-syntactic-keywords)
  ;; make local copy of the extension, and try to guess the extension
  (make-local-variable 'ado-extension)
  (ado-set-ado-extension)
  ;; turn off indenting in hlp and do files (by default)
  (if ado-smart-indent-flag
      (if (or 
	   (string= ado-extension "hlp")
	   (string= ado-extension "dlg"))	;don't have good indentation for dialogs yet
	  (setq ado-smart-indent-flag nil)
	(if (string= ado-extension "do")
	    (setq ado-smart-indent-flag ado-do-indent-flag)
	  )
	)
    )
  ;; not a good idea --- since it is a bit heavy handed for custom indentations
  ;; (if ado-smart-indent-flag
  ;;    (ado-indent-buffer))
  (run-hooks 'ado-mode-hook))

; ado-set-return == t -> swap ret and C-j
(defun ado-set-return (state)
  (if state
      (progn
	(define-key ado-mode-map "\C-m" 'ado-newline) 
	(define-key ado-mode-map "\C-j" 'newline) 
	)
    (define-key ado-mode-map "\C-j" 'ado-newline) 
    (define-key ado-mode-map "\C-m" 'newline) 
    ))

;;;; all the style-toggles for local resets rather than global

(defun ado-return-toggle ()
  (interactive)
  (setq ado-return-also-indents-flag (not ado-return-also-indents-flag))
  (ado-set-return ado-return-also-indents-flag))

(defun ado-toggle-flag (flag-name)
  (interactive "vWhat flag would you like to toggle? ")
  (set flag-name (not (eval flag-name))))

;;;; all the style value changers for local changes
;; a function which makes all the prompts and messages look the same.
(defun ado-change-number (variable newvalue)
  "For changing options which have numerical values somewhat nicely. Does
not work if fed an expression."
  (interactive "vWhat variable would you like to change? 
i")
  (if (or (null newvalue)
	  (eq newvalue 'ask))
      (progn 
	(setq newvalue (read-from-minibuffer (concat "Change " (symbol-name variable) " to ")  (number-to-string (eval variable) )))
	(if (or
	     (string= newvalue "")
	     (= (setq newvalue (string-to-number newvalue)) (eval variable)))
	    (progn
	      (message (concat "value of " (symbol-name variable) " left unchanged."))
	      nil)
	  (set variable (eval newvalue))
	  (message (format (concat "value of " (symbol-name variable) " set to %d.") (eval variable)))
	  t))
    (set variable (eval newvalue))
    t
    ))

(defun ado-tab-width-change (&optional new-tab-width)
"Changes the tab-width for the current buffer, and then optionally re-indents the file."
  (interactive)
  (if (not (ado-change-number 'tab-width new-tab-width))
      ()
    (if (y-or-n-p "Reindent buffer now? ")
	(progn
	  (save-excursion)
	  (ado-indent-buffer))
      )))

(defun ado-continued-statement-indent-spaces-change (&optional spaces)
"Changes the tab-width for the current buffer, and then optionally re-indents the file."
  (interactive)
  (if (not (ado-change-number 'ado-continued-statement-indent-spaces spaces))
      ()
    (if (y-or-n-p "Reindent buffer now? ")
	(progn
	  (save-excursion)
	  (ado-indent-buffer))
      )))

;;; scunged from the c-mode indentation
(defun ado-comment-indent ()
  (if (looking-at "^/\\*")
      0				;Existing comment at bol stays there.
    (let ((opoint (point)))
      (save-excursion
	(beginning-of-line)
	(cond ((looking-at "[ \t]*}[ \t]*\\($\\|/\\*\\)")
	       ;; A comment following a solitary close-brace
	       ;; should have only two spaces.
	       (search-forward "}")
	       (+ 2 (current-column)))
	      ((or (looking-at "^#[ \t]*endif[ \t]*")
		   (looking-at "^#[ \t]*else[ \t]*"))
	       7)			;2 spaces after #endif
	      ((progn
		 (goto-char opoint)
		 (skip-chars-backward " \t")
		 (and (= comment-column 0) (bolp)))
	       ;; If comment-column is 0, and nothing but space
	       ;; before the comment, align it at 0 rather than 1.
	       0)
	      (t
	       (max (1+ (current-column))	;Else indent at comment column
		    comment-column)))))))	; except leave at least one spaces.
  
;; useful things which are better than keyboard macros
(defun ado-parse-loop ()
  (interactive)
  (error "This is out of date! Use a foreach loop (\\[ado-foreach-loop]), instead"))

(defun ado-foreach-loop (&optional macname listtype)
"Inserts a foreach loop, after asking for the type of loop to insert."
  (interactive)
  (if (not macname)
      (setq macname (read-from-minibuffer "What local macro should hold the tokens? ")))
  (if (not listtype)
      (setq listtype (read-from-minibuffer "What type of list? (leave blank if not special) ")))
  (insert (concat "foreach " macname))
  (if (equal listtype "")
      (insert " in \"\"")
    (insert (concat " of " listtype "  ")))
  (ado-insert-with-lfd " {")
  (newline-and-indent)
  (insert "}")
  (search-backward "{")
  (backward-char 2))

(defun ado-new-program (&optional stayput name purpose)
"Makes a new buffer by inserting the file ado.blp from the template
directory. Inserts the proper name for the new command and the ado file
itself. Asks if the file should be saved in the `new' directory. If the
answer is no, the file will be saved in the current working directory.
Bound to \\[ado-new-program]" 
  (interactive)
  (if (not name)
      (setq name (read-from-minibuffer "What is the name of the program? ")))
  (if (not purpose)
      (setq purpose (read-from-minibuffer "What does it do? ")))
  (switch-to-buffer
   (generate-new-buffer
    (generate-new-buffer-name (concat name ".ado"))))
  (ado-insert-boilerplate "ado.blp")
  (if (and ado-new-dir (not stayput))
      (if (y-or-n-p "Put in 'new' directory? ")
	  (cd ado-new-dir)))
  (ado-mode)
  (goto-char (point-min))
  (end-of-line)
  (insert (ado-nice-current-date))
  (search-forward "*!")
  (end-of-line)
  (insert purpose)
  (search-forward "program define")
  (end-of-line)
  (insert name)
  (re-search-forward "\t")
  (if ado-fontify-new-flag (turn-on-font-lock))
  (ado-save-program))

(defun ado-marker-program ()
  (interactive)
  (let
      ((long-name (read-from-minibuffer "What is the name of the condition? "))
       (short-name (read-from-minibuffer "What is the default name of the marker? "))
       program-name)
    (setq program-name
	  (concat "_mk" (substring short-name 0 (min 5 (length short-name)))))
    (ado-new-program nil program-name
		       (concat "Generates default marker " short-name " for the condition " long-name))
    (ado-insert-boilerplate "markit.blp")
    (re-search-forward "\"\"")
    (forward-char -1)
    (insert long-name)
    (re-search-forward "()")
    (forward-char -1)
    (insert short-name)))

(defun ado-project-program ()
  (interactive)
  (ado-new-program)
  (ado-insert-boilerplate "proj1.blp")
  (goto-char (point-max))
  (forward-line -2)
  (ado-insert-slog-block)
  (ado-insert-boilerplate "proj2.blp")
  (goto-char (point-min))
  (search-forward "stages \"")
  (recenter))

(defun ado-stage-program ()
  (interactive)
  (ado-new-program t)
  (ado-insert-slog-block "replace")
  (ado-insert-boilerplate "stage.blp")
  ;(search-forward "tmp" nil t)
  )

(defun ado-lookup-program ()
  (interactive)
  (ado-new-program)
  (ado-insert-boilerplate "lookup.blp")
  (goto-char (point-min))
  (search-forward "using \""))

(defun ado-checker-setup ()
  (interactive)
  (let ((var-name (read-from-minibuffer "What is the name of the variable? "))
	err-name)
    (ado-new-program var-name nil (concat "Error checker for " var-name))
    var-name))

(defun ado-checker-err-name (var-name)
  (concat "err" (substring var-name 0 (min 5 (length var-name)))))

(defun ado-ckmiss-program (&optional name)
  (interactive)
  (let ((var-name (ado-checker-setup))
	err-name)
    (setq err-name (ado-checker-err-name var-name))
    (ado-insert-boilerplate "ckmiss.blp")
    (while (search-forward "foovar" nil t)
      (replace-match var-name nil t))
    (while (search-forward "fooerr" nil t)
      (replace-match err-name nil t))
    ))

(defun ado-ckdunno-program (&optional name)
  (interactive)
  (let ((var-name (ado-checker-setup)))
    (ado-insert-boilerplate "ckdunno.blp")
    (while (search-forward "foovar" nil t)
    (replace-match var-name nil t))
    ))

(defun ado-checker-program ()
  (interactive)
  (let ((var-name (ado-checker-setup))
	err-name)
    (setq err-name (ado-checker-err-name var-name))
    (ado-insert-boilerplate "check.blp")
    (while (search-forward "foobar" nil t)
      (replace-match var-name nil t))
    (goto-char (point-min))
    (while (search-forward "errfoobar" nil t)
      (replace-match err-name nil t))
    (goto-char (point-min))
    (search-forward "=")
    ))
  
(defun ado-insert-new-program (&optional name purpose)
  "Inserts a subprogram at the bottom of the current buffer. There is
something broken in that the insertion point is left in the wrong spot..."
  (interactive)
  (if (not name)
      (setq name (read-from-minibuffer "What is the name of the program? ")))
  (goto-char (point-max))
  (ado-insert-boilerplate "smallado.blp")
  (search-forward "program define")
  (end-of-line)
  (insert name)
  (search-forward "	"))

(defun ado-new-label (&optional name)
  "Grab the boilerplate for a label and name the buffer"
  (interactive)
  (if (not name)
      (setq name (read-from-minibuffer "What is the name of the label? ")))
  (switch-to-buffer
   (generate-new-buffer
    (generate-new-buffer-name (concat name ".lbl"))))
  (ado-insert-boilerplate "lbl.blp")
  (if (and ado-label-dir (y-or-n-p "Put in local 'lbl' directory? "))
      (cd ado-local-label-dir)
    (pwd))				;put in to avoid troubles later!
  (ado-mode)
  (goto-char (point-min))
  (re-search-forward "def ")
  (insert name)
  (forward-char 1))

(defun ado-write-file-as-buffer-name ()
  "Takes care of the problem in emacs where a buffer can have its name 
changed, but will write itself under it's regular filename."
  (interactive)
  (let (this-buffer)
    (setq this-buffer (buffer-name))
    (if (string-match "*" this-buffer)
	(save-buffer)
      (write-file (substring this-buffer 0 (string-match "<" this-buffer)) ado-confirm-overwrite-flag)
      ))
  )

(defun ado-save-program (&optional filename)
  "Saves file, doing its best to make a good file name and timestamp. The 
command works differently depending on the type of file:

  .ado files : looks for the first 'program define' statement, and
    attaches the extension .ado. This means that there is no worry
    about the program name and file name being different. *Note* that
    the 'program define' statement may be abbreviated according, but
    must be at the start of a line.

  .class files: looks for a line starting with 'class', and attaches
    the extension .class.

  .dlg files: since Stata's standard way to store name for dialog
    (dlg) files doesn't lend itself to an easy way to pick it out from
    the file, make a new standard! Assume that .dlg files look at the
    top just like program files, but they start file with *! and then
    the name, and then the version number and then the timestamp.

  .do files : looks for the first instance of .do, and pulls the name
    from there. Note that the .do filename can have no spaces!

If none of these tags are found, then the program tries to save the
file as it's buffer name (without the <#> garbage). 

Timestamps are updated only if there is a '*! version xxx' statement
in ado files, or a {title:Last Updated} in hlp files." 
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; looking for a version number, so that the date stamp can be updated
;; leaves the new date and time, even if the file is not saved... dunno what
;; to do to fix this, since I cannot find error trapping for errors which do
;; not use 'signal to identify the errors (write-file does not use a signal)
    (if (or 
	 (re-search-forward "^\*![ \t]+version[0-9\. ]*" (point-max) t)
	 (re-search-forward "Last Updated\}: " (point-max) t)
	 )
	(progn
	  (kill-line)
	  (insert (ado-nice-current-date))
	  )
      )
    (goto-char (point-min))
    ;; if file name specified, this is just a write-file
    (if filename
	(write-file filename ado-confirm-overwrite-flag)
      ;; check to see if this file has a buffer .do
      (if (string= "do" (downcase (concat "" (file-name-extension (concat (buffer-file-name) ""))))) 
	  (save-buffer)
	(setq filename (ado-make-ado-name)) ; will be fooled by illegal program defines
	(if (string= (concat default-directory filename) (buffer-file-name))
	    (save-buffer)
	  (write-file filename ado-confirm-overwrite-flag)
	  ))
      )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  the following gets confused by .do files with subroutines ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun ado-set-ado-extension ()
;;   (interactive)
;;   ;; first look at the extension of the buffer
;;   (save-excursion
;;     (goto-char (point-min))
;;     (let (
;; 	  (from-buffer 
;; 	   (if buffer-file-name
;; 	       (downcase (file-name-extension buffer-file-name))
;; 	     nil))
;; 	  (from-file 
;; 	   (cond
;; 	    ((re-search-forward "^class[ \t]+" nil t) "class")
;; 	    ((re-search-forward 
;; 	      "^pr\\(o\\|\\og\\|\\ogr\\|\\ogra\\|\\ogram\\)[ \t]+\\(de\\(f\\|fi\\|fin\\|fine\\)[ \t]+\\)*" nil t) "ado")
;; 	    ((re-search-forward "^\\*![ \t]+" nil t) "dlg")
;; 	    )
;; 	 ))
;;       (if (and from-file from-buffer (not (string= from-file from-buffer)))
;; 	  (setq ado-extension 
;; 		(read-from-minibuffer "I'm confused. What should the file extension be? "))
;; 	(setq ado-extension (or from-file from-buffer)))
;;       )))

(defun ado-set-ado-extension ()
  (interactive)
  ;; when in doubt, trust the buffer
  (save-excursion
    (goto-char (point-min))
    (setq ado-extension
	  (if buffer-file-name
	      (downcase (file-name-extension buffer-file-name))
	   (cond
	    ((re-search-forward "^class[ \t]+" nil t) "class")
	    ((re-search-forward 
	      "^pr\\(o\\|\\og\\|\\ogr\\|\\ogra\\|\\ogram\\)[ \t]+\\(de\\(f\\|fi\\|fin\\|fine\\)[ \t]+\\)*" nil t) "ado")
	    ((re-search-forward "^\\*![ \t]+" nil t) "dlg")
	    )
	   ))))

(defun ado-make-ado-name ()
  (interactive)
  (let (name-start name-end name-extension)
    (save-excursion
      (goto-char (point-min))
;;  dlg choice below must be last, since regular programs start this way too.
      (if 
	  (or (setq name-extension "class" name-start (re-search-forward "^class[ \t]+" nil t))
	      (setq name-extension "ado"
		    name-start (re-search-forward 
			"^pr\\(o\\|\\og\\|\\ogr\\|\\ogra\\|\\ogram\\)[ \t]+\\(de\\(f\\|fi\\|fin\\|fine\\)[ \t]+\\)*" nil t))
	      (setq name-extension "dlg" name-start (re-search-forward "^\\*![ \t]+" nil t))
	      )
	  (progn
	     ; could have illegal name below - but needed a hack to allow underscores
	    (re-search-forward "[a-zA-Z_]+[a-zA-Z0-9_]*\\b")
	    (setq name-end (point))
	    (concat (buffer-substring-no-properties name-start name-end) "." name-extension))
	(if (re-search-forward "\\.do\\b" nil t)
	    (progn
	      (search-backward ".do")
	      (setq name-end (point))
	      (re-search-backward " ") ;cannot handle spaces in do-file file names
	      (setq name-start (+ (point) 1))
	      (concat (buffer-substring-no-properties name-start name-end) ".do"))
	  )
      ))))


(defun ado-find-local-name ()
  "Returns the name of the defining program in which the point is sitting. If the
program define statement cannot be found, returns nil. Returns a mess if the
program name is missing (need to fix this). Currently broken --- need to assess need."
  (let (name-start name-end)
    (save-excursion
      (if (re-search-backward "^pr\\(o\\|\\og\\|\\ogr\\|\\ogra\\|\\ogram\\)[ \t]+" 0 t) ;goes to most recent definition
	  (progn
	    (setq name-start (re-search-forward 
			"^pr\\(o\\|\\og\\|\\ogr\\|\\ogra\\|\\ogram\\)[ \t]+\\(de\\(f\\|fi\\|fin\\|fine\\)[ \t]+\\)*" nil t)
		  name-end (re-search-forward "[a-zA-Z_]+[a-zA-Z0-9_]*\\b"))
	    (buffer-substring-no-properties name-start name-end))
	))))

(defun ado-show-local-name ()
  (interactive)
  (message (concat "The local program is " (ado-find-local-name))))

;; (defun ado-insert-boilerplate (file-name &optional raw)
;;   (if (not ado-site-template-dir)
;;       (error "%s" "Use \\[set-variable] to set ado-site-template-dir to the directory holding the ado templates!"))
;;   (insert-file-contents (concat ado-site-template-dir file-name))
;;   (if (not raw)
;;       (ado-indent-region)
;;     ))

(defun ado-insert-file-and-indent (file)
  "Interactively insert (and properly indent) an ado file.
Can also be called from within another program for inserting
and indenting"
  (interactive "fGive the file to insert: ")
  (let ((beg (point)))
    (insert-file-contents file)
    (ado-indent-region beg (point))
    ))

(defun ado-insert-boilerplate (file-name &optional raw)
  (if (not ado-site-template-dir)
      (error "%s" "Use \\[set-variable] to set ado-site-template-dir to the directory holding the ado templates!"))
  (if raw
      (insert-file-contents (concat ado-site-template-dir file-name))
    (ado-insert-file-and-indent (concat ado-site-template-dir file-name))))


(defun ado-insert-slog-block (&optional replace-flag)
  (interactive)
  (let ((the-name (ado-find-local-name)))
    (ado-insert-boilerplate "slog.blp")
    (re-search-forward "\"\"")
    (forward-char -1)
    (if the-name
	(insert the-name)
      (error "Could not determine the name of the defining program"))
    (if replace-flag
	(progn
	  (re-search-forward "append")
	  (forward-word -1)
	  (kill-word 1)
	  (insert "replace")))
    (re-search-forward "{")
    (newline-and-indent)
    ))

;;; depth and indentation commands
(defun ado-line-starts-with-end-comment ()
  (interactive)
  (save-excursion
  (beginning-of-line)
  (and (search-forward "*/" (point-at-eol) t)
       (not (search-backward "/*" (point-at-bol) t)))))

(defun ado-out-of-nested-comment (&optional top from-level)
  (interactive)
  (let ((ppsexp (parse-partial-sexp 1 (point))) this-level )
    (if (numberp (setq this-level (nth 4 ppsexp)))
	(if (or (not from-level) (and (<= from-level this-level)))
	    (if (search-backward "/*" 1 t)
		(if top (ado-out-of-nested-comment t)
;		  (forward-char -1)
		  (ado-out-of-nested-comment nil this-level)
		  ))
;	  (if (not top) (forward-char 1))
	  )
      (if top
	  (if (search-backward "*/" (point-at-bol) t)
	      (ado-out-of-nested-comment top)
	    )))))

(defun ado-show-depth ()
  (interactive)
  (let ((depth (ado-find-depth)))
    (message (concat "The depth was " (number-to-string (car depth))
		     (if (nth 1 depth) " with continuation" " without continuation")))))

;; changed left-indenting of 'version' to left-indent iff spelled out
;;   to avoid the indenting, use an abbreviation
(defun ado-find-depth ()
  (let (depth start ppsexp in-continuation (oddend (ado-line-starts-with-end-comment)))
    (save-excursion
      ;; look for line starting with a comment
      (setq in-continuation (ado-beginning-of-command))
      (setq ppsexp (parse-partial-sexp 1 (point)))
      (setq depth (car ppsexp))
      (setq start (point))
      ;; list of things which should be left aligned
      ;;  if there are unbalanced parameters, this provides feedback via odd alignment
;      (unless (and (not (or (nth 4 ppsexp) (nth 7 ppsexp)))
;	       (or
      (unless (or oddend
	       (and (not ado-close-under-line-flag) (looking-at "}"))
	       (looking-at "*!")
	       (looking-at "p\\(r\\|ro\\|rog\\|rogr\\|rogra\\|rogram\\)[ \t]+")
	       (looking-at "version")
	       (looking-at "end")
	       (looking-at "pause"))
	(setq depth (1+ depth))))
    (save-excursion
      (beginning-of-line)
      (setq ppsexp (parse-partial-sexp start (point)))
      (if (numberp (nth 4 ppsexp))
	  (list (+ depth (nth 4 ppsexp)) in-continuation)
	(list depth in-continuation)))
      ))

(defun ado-indent-region (&optional start end)
  (interactive)
  (let (endmark)
    (if (and (null start) (null end))
	(progn
	  (setq start (min (point) (mark)))
	  (setq end (max (point) (mark))))
      )
    (save-excursion
      (goto-char start)
      ;; Advance to first nonblank line.
      (beginning-of-line)
      (setq endmark (copy-marker end))
      (while (and (bolp) (not (eobp)) (< (point) endmark))
	(skip-chars-forward " \t\n")
	(ado-indent-line)
	(forward-line 1)
      ))))

(defun ado-indent-buffer ()
  (interactive)
  (save-excursion
    (ado-indent-region (point-min) (point-max))))

(defun ado-indent-line ()
  "A smart indenter for ado files. Many of the parameters can
be customized using '\\[customize-group] ado-mode'."
  (interactive)
  (if ado-smart-indent-flag
      (let (indent depth beg shift-amt endmark
	    (pos (- (point-max) (point)))
	    (watch-for-semi (ado-delimit-is-semi)))
	(beginning-of-line)
	(setq beg (point))
	(cond ((and ado-delimit-indent-flag (looking-at "[ \t]*#d\\(e\\|el\\|eli\\|elim\\|elimi\\|elimit\\)"))	;#delimits belong at delimit indent
	       (setq indent ado-delimit-indent-column))
	      ((and ado-comment-indent-flag
		    (or (looking-at "^\\*") (looking-at "^*")))	;comments at start of line belong at comment indent
	       (setq indent ado-comment-indent-column))
	      ((and ado-debugging-indent-flag
		    (or (looking-at "^[ \t]*pause")	
			(looking-at "^[ \t]*set t\\(r\\|ra\\|rac\\|race\\)[ \t]+")))
	       (setq indent ado-debugging-indent-column)) ; debugging at proper column (usually 0)
	      (t (setq indent (* tab-width (car (setq depth (ado-find-depth)))))   ; regular indentation
		 (if (nth 1 depth)
		     (setq indent (+ indent ado-continued-statement-indent-spaces)))
		 ))						  ; end of conditional statement
	(skip-chars-forward " \t")
	(setq shift-amt (- indent (current-column)))
	(if (zerop shift-amt)
	    (if (> (- (point-max) pos) (point))
		(goto-char (- (point-max) pos)))
	  (delete-region beg (point))
	  (indent-to indent)
	  ;; If initial point was within line's indentation,
	  ;; position after the indentation.  Else stay at same point in text.
	  (if (> (- (point-max) pos) (point))
	      (goto-char (- (point-max) pos))))
	shift-amt)))

(defun ado-delimit-is-semi ()
  "Returns t if semicolons delimit commands, otherwise returns nil."
  (save-excursion
    ;; if #delimit command is on same line, the delimiter is always cr
    (let ((line-start (point-at-bol)))
      (if (re-search-backward "#d\\(e\\|el\\|eli\\|elim\\elimi\\elimit\\)" 1 t)
	  (let ((ppsexp (parse-partial-sexp 1 (point))))
	    (if (or 
		 (nth 3 ppsexp)		; inside a string
		 (nth 4 ppsexp)		; inside a non-nestable comment
		 (nth 7 ppsexp))		; inside a type-b comment
		(ado-delimit-is-semi)
	      (if (>= (point) line-start)
		  nil
		(forward-sexp)
		(skip-chars-forward " \t")
		(looking-at ";"))))))))

(defun ado-show-delimiter ()
  "Returns the value of the delimiter in ado-functions as a message."
  (interactive)
  (message (if (ado-delimit-is-semi)
	       "The delimiter is ;"
	     "The delimiter is cr"
	     )))

(defun ado-beginning-of-command ()
  "Moves the cursor to the start of the command in which the insertion
point is sitting. This will jump back to the start of a command if the
insertion point is within the command, and jump forward to the start
of a command if the delimiter is in whitespace preceding a command.

When the delimiter is cr, blank lines count as empty commands.

When the delimiter is ; blank lines count as whitespace in commands.
This last statement is wrong and needs some work.

Returns t if inside of a continued function, nil otherwise."
  (interactive)
  (let ((in-continuation nil) (start-line (line-number-at-pos)))
    (ado-out-of-nested-comment t)
    ;; first look backwards for either delimiter or delimit command
    (unless (search-backward "#delimit" (point-at-bol) t)
      (if (ado-delimit-is-semi)
	  (progn
	    (search-backward ";" 1 t)
	    (if (and
		 (< (line-number-at-pos) start-line)
		 (save-excursion
		   (skip-chars-forward " \t\n;")
		   (< (line-number-at-pos) start-line)))
		(setq in-continuation t)))
	(search-backward "\n" 1 t))
      (unless (bobp)
	(if (re-search-backward "\\(\\s-\\|^\\)///+.*$" (point-at-bol) t)
	  (progn
	    (forward-char -1)
	    (ado-beginning-of-command)
	    (setq in-continuation t))
	  (forward-char 1))))
    (skip-chars-forward " \t\n")
    in-continuation
    ))

(defun ado-end-of-command ()
  "Move to the end of the current command."
  (interactive)
  (let (ppsexp)
    (if (ado-delimit-is-semi)
	(if (not (search-forward ";" nil t))
	    (error "No end of command") 
	  (setq ppsexp (parse-partial-sexp 1 (point)))
	  (if (or (nth 4 ppsexp) (nth 3 ppsexp) (nth 7 ppsexp))
	      (ado-end-of-command)
	    (backward-char)))
      (if (re-search-forward "/\\*" (point-at-eol) t)
	  (progn
	    (backward-char 2)
	    (skip-syntax-backward "-"))
	;; look for continuation
	(if (search-forward "///" (point-at-eol) t)
	    (progn
	      (end-of-line)
	      (if (not (< (point) (point-max)))
		  (error "No end of command - moved as far as possible")
		(forward-char)
		(ado-end-of-command)))
	  (if (not (search-forward "//" (point-at-eol) t))
	      (end-of-line)
	    (backward-char 2)
	    (skip-syntax-backward "-")))
       ))))


;; stolen from c-mode, and changed slightly, since Stata does not allow
;; braces on separate lines
(defun electric-ado-closing-brace (arg)
  "Insert closing character, possibly on new line, and correct line's indentation."
  (interactive "P")
  (if (and (not arg)
	   (eolp)
;	   ado-auto-newline-flag
	   ado-closing-brace-alone-flag)
      (if (save-excursion
	    (skip-chars-backward " \t")
	    (not (bolp)))
	(newline-and-indent)))
  (electric-ado-brace arg))

(defun electric-ado-brace (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos)
    (if (and (not arg)
	     (eolp)
	     ado-auto-newline-flag)
	(progn
	  (insert last-command-char)
	  (ado-indent-line)
	  (if ado-auto-newline-flag
	      (progn
		(newline)
		;; (newline) may have done auto-fill
		(setq insertpos (- (point) 2))
		(ado-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1+ insertpos)))
	    (delete-char -1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun ado-newline ()
  "Justifies current line before doing a newline-and-indent"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (ado-indent-line))
  (newline-and-indent))

(defun ado-split-line ()
  "Splits line at point, putting a start comment (/*) at the end of the current line, and an end comment (*/) at the beginning of the next line"
  (interactive)
  (if ado-use-modern-split-flag
      (progn
	(insert " ///")
	(newline-and-indent))
    (insert "/*")
    (newline-and-indent)
    (insert "*/"))  
  )


(defun electric-ado-semi (arg)
  "Puts in extra newline if the semi-colon is the end-of-line delimiter"
  (interactive "P")
  (if (ado-delimit-is-semi)
      (electric-ado-brace arg)
    (self-insert-command (prefix-numeric-value arg))))

;; here comes all the stuff for auto-highlighting!
;;
(defun ado-set-font-lock-keywords ()
;  (make-local-variable 'ado-font-lock-keywords)
  (interactive)
  (setq
   ado-font-lock-keywords
   (list
    ;; an attempt to get nested quotes to work
    (eval-when-compile
      (make-regexps
       '(("`\".*\"'") ado-string-face t)
       ))
      
    ;; special highlighting
    ;; program definitions
    (eval-when-compile
      (make-regexps
       '(("^\\*!.*") ado-builtin-harmful-face)
       ))
    ;; does not force program define for keywords; could look ragged if ado-subcommand-face has a background
    (eval-when-compile
     (make-regexps
      "^"
      '((
	 "pr" "pro" "prog" "progr" "progra" "program"
	 ) ado-builtin-harmful-face)
      '((
	 "\\([ \t]+\\(\\(d\\(e\\(f\\|fi\\|fin\\|fine\\)\\|rop\\)\\)\\|\\(l\\|li\\|lis\\|list\\)\\)\\)?"
	 ) ado-subcommand-face t)
      "[ \t]+"
      '(("[_a-zA-Z.]+[_a-zA-Z0-9]*") ado-builtin-harmful-face t)
      ))
    ;; the program dir thingy
    (eval-when-compile
     (make-regexps
      "\\b"
      '((
	 "pr" "pro" "prog" "progr" "progra" "program"
	 ) ado-builtin-harmless-face)
      "[ \t]+"
      '((
	 "dir"
	 ) ado-subcommand-face t)
      ))
    (eval-when-compile
      (make-regexps
       "\\b"
      '(("version") ado-builtin-harmless-face)
      "[ \t]+"
      '(("1[.]0" "2[.]0" "2[.]1" "3[.]0" "3[.]1" "4\\([.]0\\)?" "5\\([.]0\\)?" "6\\([.]0\\)?" "7" "8\\([.]\\(1\\|2\\)\\)?" "9\\([.]\\(0\\|1\\)\\)?") ado-subcommand-face)
      "\\b"
      ))
    (eval-when-compile
      (make-regexps
       "^[ \t]*"
       '(("pause") ado-builtin-harmful-face)
       "[ /t]+"
       '(("off" "on") ado-subcommand-face t)
       "\\b"
       ))
     (eval-when-compile
       (make-regexps
        "^[ \t]*"
        '(("end" "pause"
 	  ) ado-builtin-harmful-face)
	"\\b"
        ))
    ;; delimit command
    (eval-when-compile
      (make-regexps
       "^[ \t]*"
       '((
	  "#d" "#de" "#del" "#deli" "#delim" "#delimi" "#delimit" ) ado-builtin-harmless-face)
       "\\s-*"
       '(("\\(cr\\|;\\)\\s-*$") ado-subcommand-face nil)
       ))
    ;;
    ;; obsolete stuff which appears as OK as subcommands
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("sco" "scor" "score") ado-obsolete-face)
       "\\b"
       ))
    ;;
    ;; the cluster commands
    ;; comment region 1
    (eval-when-compile
      (make-regexps
       '(("\\bcluster") ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "dend" "dendr" "dendro" "dendrog" "dendrogr" "dendrogra" "dendrogram"
	  "dir"
	  "k" "km" "kme" "kmea" "kmean" "kmeans" 
	  "kmed" "kmedi" "kmedia" "kmedian" "kmedians" 
	  "list"
	  "note" "notes"
	  "parsedist" "parsedista" "parsedistan" "parsedistanc" "parsedistance" 
	  "query"
	  "stop"
	  "tr" "tre" "tree"
	  ) ado-subcommand-face)
       "\\b"
       ))
    ;; 
    ;; data altering cluster commands
    ;; comment region 2
    (eval-when-compile
      (make-regexps
       '(("\\bcluster") ado-builtin-harmful-face)
       "[ \t]+"
       '((
	  "del" "dele" "delet" "delete" 
	  "drop"
	  "gen" "gene" "gener" "genera" "generat" "generate" 
	  "measures"
	  "rename" "renamevar"
	  "set"
	  "use"
	  ) ado-subcommand-face)
       "\\b"
       ))
    ;; putting together common cluster and clustermat commands
    ;; comment region whatever
    (eval-when-compile
      (make-regexps
       '(("\\bcluster\\(mat\\)?") ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "a" "av" "ave" "aver" "avera" "averag" "average" "averagel" "averageli" 
	  "averagelin" "averagelink" "averagelinka" "averagelinkag" "averagelinkage" 
	  "c" "cent" "centr" "centro" "centroi" "centroid" "centroidl" "centroidli" 
	  "centroidlin" "centroidlink" "centroidlinka" "centroidlinkag" "centroidlinkage" 
	  "co" "com" "comp" "compl" "comple" "complet" "complete" "completel" "completeli" 
	  "completelin" "completelink" "completelinka" "completelinkag" "completelinkage" 
	  "med" "medi" "media" "median" "medianl" "medianli" "medianlin" 
	  "medianlink" "medianlinka" "medianlinkag" "medianlinkage" 
	  "s" "si" "sin" "sing" "singl" "single" "singlel" "singleli" "singlelin" 
	  "singlelink" "singlelinka" "singlelinkag" "singlelinkage"
	  "ward" "wards" "wardsl" "wardsli" "wardslin" "wardslink" "wardslinka" "wardslinkag" "wardslinkage"
	  "wav" "wave" "waver" "wavera" "waverag" "waverage" "waveragel" 
	  "waverageli" "waveragelin" "waveragelink" "waveragelinka" "waveragelinkag" "waveragelinkage" 
	  ) ado-subcommand-face)
       "\\b"
       ))

;;     ;; set command and its variations
    (eval-when-compile
      (make-regexps
       '(("^[ \t]*set") ado-builtin-harmless-face)
       "[ \t]+"
       '(("a" "ad" "ado" "ados" "adosi" "adosiz" "adosize" 
	  "conren\\(?:[ \t]+\\(?:clear\\|sf\\|bf\\|it\\|res\\|resu\\|resul\\|result\\|txt\\|text\\|inp\\|inpu\\|input\\|li\\|lin\\|link\\|hi\\|hil\\|hili\\|hilit\\|hilite\\)\\)?"
	  "copycolor[ \t]+\\(?:auto\\|autom\\|automa\\|automat\\|automati\\|automatic\\|asis\\|gs[123]\\)"
	  "dp[ \t]+\\(?:com\\|comm\\|comma\\|per\\|peri\\|perio\\|period\\)"
	  "eolc\\(?:h\\|ha\\|har\\)[ \t]+\\(?:mac\\|unix\\)"
	  "httpproxyhost" "httpproxyport" "httpproxypw" "httpproxyuser"
	  "l" "le" "lev" "leve" "level"
	  "li" "lin" "line" 
	  "lineg" "linega" "linegap" 
	  "lines" "linesi" "linesiz" "linesize" 
	  "log\\(?:t\\|ty\\|typ\\|type\\)[ \t]+\\(?:t\\|te\\|tex\\|text\\|s\\|sm\\|smc\\|smcl\\)"
	  "macgp\\(?:h\\|he\\|hen\\|heng\\|hengi\\|hengin\\|hengine\\)[ \t]+\\(?:qu\\(?:artz\\|ickdraw\\)\\)"
	  "mat" "mats" "matsi" "matsiz" "matsize"
	  "maxdb" "maxiter" "maxvar"
	  "mem" "memo" "memor" "memory"
	  "ob" "obs"
	  "p" "pa" "pag" "page" "pages" "pagesi" "pagesiz" "pagesize" 
	  "printcolor[ \t]+\\(?:auto\\|autom\\|automa\\|automat\\|automati\\|automatic\\|asis\\|gs[123]\\)"
	  "reventr" "reventri" "reventrie" "reventries" 
	  "revwi\\(?:n\\|nd\\|ndo\\|ndow\\)[ \t]+\\(?:no\\)?float"
	  "scheme" "scrollbufsize"
	  "search\\(?:d\\|de\\|def\\|defa\\|defau\\|defaul\\|default\\)[ \t]+\\(?:all\\|local\\|net\\)"
	  "se" "see" "seed"
	  "smoothsize"
	  "timeout1"
	  "timeout2"
	  "traced" "tracede" "tracedep" "tracedept" "tracedepth" 
	  "traceh" "tracehi" "tracehil" "tracehili" "tracehilit" "tracehilite" 
	  "t" "ty" "typ" "type" 
	  "update_interval"
	  "varlab" "varlabe" "varlabel" 
	  "varlabelpos"
	  "varwi\\(?:n\\|nd\\|ndo\\|ndow\\)[ \t]+\\(?:no\\)?float"
	  )
	 ado-subcommand-face t)
       "\\b"
       ))
    ;; set command : on/off settable items
    (eval-when-compile
      (make-regexps
       '(("^[ \t]*set") ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "checksum" "fastscroll"
	  "dockable"
	  "dockingg" "dockinggu" "dockinggui" "dockingguid" "dockingguide" "dockingguides"
	  "g" "gr" "gra" "grap" "graph" "graphi" "graphic" "graphics"
	  "httpproxy" 
	  "httpproxya" "httpproxyau" "httpproxyaut" "httpproxyauth" 
	  "icmap"
	  "locksplit" "locksplitt" "locksplitte" "locksplitter" "locksplitters" 
	  "mo" "mor" "more" 
	  "piccom" "piccomm" "piccomme" "piccommen" "piccomment" "piccomments" 
	  "persistfv" "persistvtopic"
	  "r" "rm" "rms" "rmsg" 
	  "smalldlg" "smoothfonts"
	  "tr" "tra" "trac" "trace"
	  "tracee" "traceex" "traceexp" "traceexpa" "traceexpan" "traceexpand" 
	  "tracei" "tracein" "traceind" "traceinde" "traceinden" "traceindent" 
	  "tracen" "tracenu" "tracenum" "tracenumb" "tracenumbe" "tracenumber" 
	  "traces" "tracese" "tracesep" 
	  "update_prompt" "update_query"
	  "varabbrev"
	  "vir" "virt" "virtu" "virtua" "virtual"
	  "xptheme"
	  )
	 ado-subcommand-face t)
       "[ \t]+"
       '(("off" "on") ado-subcommand-face t)
       ))
       ;;
       ;; set command (with endless options!)
     (eval-when-compile
       (make-regexps
        '(("^[ \t]*set") ado-builtin-harmless-face)
        "[ \t]+"
        '((
 	  "ou" "out" "outp" "outpu" "output"
 	  )
 	 ado-subcommand-face)
        "[ \t]+"
        '((
 	  "e" "er" "err" "erro" "error"
 	  "i" "in" "inf" "info" "infor" "inform" 
 	  "p" "pr" "pro" "proc" 
 	  ) ado-subcommand-face)
	"\\b"
        ))
    ;;
    ;; the set subcommands which appear to be obsolete (which don't show in the Stata Reference)
     (eval-when-compile
       (make-regexps
        '(("^[ \t]*set") ado-builtin-harmless-face)
        "[ \t]+"
        '(("ANSI" 
 	  "b" "be" "bee" "beep" "contents" 
 	  "d" "di" "dis" "disp" "displ" "displa" "display"
 	  "help"
 	  "IBM" 
 	  "lo" "log"
 	  "maxobs" 
	  "printcolor[ \t]+grayscale"
 	  "seed0" "shell"
 	  "te" "tex" "text" "texts" "textsi" "textsiz" "textsize"
 	  "video"
 	  )
 	 ado-obsolete-face t)
        "\\b"
        ))

     ;; the args command
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("args") ado-builtin-harmful-face)
	"[ \t]+"
        '((
 	  "\\([a-zA-Z_][a-zA-Z_0-9]*[ \t]*\\)+"
 	  ) ado-variable-name-face)
        "\\b"
        ))


     ;; char with sub commands
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("char") ado-builtin-harmless-face)
        "[ \t]+"
        '((
 	  "l" "li" "lis" "list" 
 	  ) ado-subcommand-face)
        "\\b"
        ))
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("char") ado-builtin-harmful-face)
        "[ \t]+"
        '((
	   "ren" "rena" "renam" "rename"
 	  ) ado-subcommand-face)
        "\\b"
        ))


    ;; the constraint commands
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "cons" "const" "constr" "constra" "constrai" "constrain" "constraint" 
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "d"
	  "de" "def" "defi" "defin" "define" 
	  "di" "dir"
	  "drop"
	  "free" "get"
	  "l" "li" "lis" "list"
	  ) 
	 ado-subcommand-face)
       "\\b"
       ))

    ;; the confirm commands - could be a mess!
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "conf" "confi" "confir" "confirm"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "e" "ex" "exi" "exis" "exist" "existe" "existen" "existenc" "existence"
	  "f" "fi" "fil" "file" 
	  "mat" "matr" "matri" "matrix" 
	  "n" "name" "names" "nu" "num" "numb" "numbe" "number" 
	  "sca" "scal" "scala" "scalar" 
	  "v" "va" "var" "vari" "varia" "variab" "variabl" "variable"
	  ) ado-subcommand-face)
       "\\b"
       ))
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "conf" "confi" "confir" "confirm"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "integer"
	  ) ado-subcommand-face)
       "[ \t]+"
       '((
	  "n" "nu" "num" "numb" "numbe" "number"
	  ) ado-subcommand-face)
       "\\b"
       ))
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "conf" "confi" "confir" "confirm"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "n" "ne" "new"
	  ) ado-subcommand-face)
       "[ \t]+"
       '((
	  "f" "fi" "fil" "file"
	  "v" "va" "var" "vari" "varia" "variab" "variabl" "variable"
	  ) ado-subcommand-face)
       "\\b"
       ))
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "conf" "confi" "confir" "confirm"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "byte" "double" "float" "int" "long"
	  "numeric"
	  "str" "stri" "strin" "string"
	  ) ado-subcommand-face)
       "[ \t]+"
       '((
	  "v" "va" "var" "vari" "varia" "variab" "variabl" "variable" 
	  ) ado-subcommand-face)
       "\\b"
       ))
    ;;; the str# won't quite look right, but that's the breaks for using
    ;;; a tool like this...
    ;;; won't allow big long strings which are allowed in wicked huge stata
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "conf" "confi" "confir" "confirm"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "str[1-9]+[0-9]*[ \t]+"
	  ) ado-subcommand-face)
       '((
	  "v" "va" "var" "vari" "varia" "variab" "variabl" "variable" 
	  ) ado-subcommand-face)
       "\\b"
       ))
    ;; the duplicates commands
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("duplicates") ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "b" "br" "bro" "brow" "brows" "browse" 
	  "drop"
	  "e" "ex" "exa" "exam" "examp" "exampl" "example" "examples" 
	  "l" "li" "lis" "list" 
	  "r" "re" "rep" "repo" "repor" "report" 
	  "t" "ta" "tag" 
	  ) ado-subcommand-face)
       "\\b"
       ))
    ;; the estimates commands
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "est" "esti" "estim" "estima" "estimat" "estimate" "estimates"
	  ) ado-builtin-harmless-face t)
       "[ \t]+"
       '((
	  "ch" "cha" "chan" "chang" "change" 
	  "clear"
	  "d" "di" "dir" 
	  "drop"
	  "f" "fo" "for" 
	  "q" "qu" "que" "quer" "query" 
	  "r" "re" 
	  "rep" "repl" "repla" "replay" 
	  "res" "rest" "resto" "restor" "restore" 
	  "st" "sta" "stat" "stats" 
	  "sto" "stor" "store" 
	  "t" "ta" "tab" "tabl" "table" 
	  ) 
	 ado-subcommand-face t)
       "\\b"
       ))
    ;; the _estimates commands
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "_est" "_esti" "_estim" "_estima" "_estimat" "_estimate" "_estimates"
	  ) ado-builtin-harmless-face t)
       "[ \t]+"
       '((
	  "clear"
	  "dir" 
	  "drop"
	  "h" "ho" "hol" "hold"
	  "u" "un" "unh" "unho" "unhol" "unhold"
	  ) 
	 ado-subcommand-face t)
       "\\b"
       ))

    ;; the estat commands --- moved to after the obsolete commands

    ;; the etest commands
    ;;  FIX when at regression post estimation
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("etest") ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "hett" "hette" "hettes" "hettest" 
	  ) ado-subcommand-face)
       "\\b"
       ))
    ;; the file commands

    (eval-when-compile
       (make-regexps
        "\\b"
        '(("file") ado-builtin-harmless-face)
        "[ \t]+"
        '((
 	  "close" "open" 
	  "q" "qu" "que" "quer" "query" 
	  "seek" 
	  "set"
 	  ) ado-subcommand-face)
        "\\b"
        ))

    (eval-when-compile
       (make-regexps
        "\\b"
        '(("file") ado-builtin-harmful-face)
        "[ \t]+"
        '((
	   "r" "re" "rea" "read" 
	   "sersetread" "sersetwrite"
	   "w" "wr" "wri" "writ" "write" 
 	  ) ado-subcommand-face)
        "\\b"
        ))

    ;; the gph commands
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "gph"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "arc"
	  "box"
	  "clear" "close"
	  "font"
	  "line"
	  "open"
	  "pen" "point"
	  "text"
	  "vline" "vpoint" "vpoly" "vtext"
	  ) 
	 ado-subcommand-face)
       "\\b"
       ))
    ;;
    ;; the gprefs commands
    ;;   (in multiple pieces)
    ;; comment region 5
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
 	  "gprefs"
 	  ) ado-builtin-harmless-face)
        "[ \t]+"
        '((
 	  "q" "qu" "que" "quer" "query" 
 	  ) 
 	 ado-subcommand-face)
	"[ \t]"
        '((
 	  "window"
 	  ) ado-subcommand-face)
        "\\b"
        ))
     ;;
     ;; the gprefs set window scheme commands
     ;; comment region 6
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
 	  "gprefs"
 	  ) ado-builtin-harmless-face)
        "[ \t]+"
        '(("set") ado-subcommand-face)
        "[ \t]+"
        '(("window") ado-subcommand-face)
        "[ \t]+"
        '(("scheme") ado-subcommand-face)
        "[ \t]+"
        '((
 	  "black" "blackb" "blackbg" 
 	  "custom1" "custom2" "custom3"
 	  "mono" "monoc" "monoch" "monochr" "monochro" "monochrom" "monochrome" 
 	  "white" "whiteb" "whitebg" 
 	  ) 
 	 ado-subcommand-face)
        "\\b"
        ))
     ;;
     ;; the other gprefs set window 
     ;; comment region 7
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
 	  "gprefs"
 	  ) ado-builtin-harmless-face)
        "[ \t]+"
        '(("set") ado-subcommand-face)
        "[ \t]+"
        '(("window") ado-subcommand-face)
        "[ \t]+"
        '((
 	  "displaythick[ \t]+off" "displaythick[ \t]+on"
 	  "update"
 	  "usegphsize[ \t]+off" "usegphsize[ \t]+on"
 	  "xsize"
 	  "ysize"
 	  ) 
 	 ado-subcommand-face)
        "\\b"
        ))
     ;;
     ;; the gprefs set scheme commands 
     ;; comment region 8
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
 	  "gprefs"
 	  ) ado-builtin-harmless-face)
        "[ \t]+"
        '(("set") ado-subcommand-face)
        "[ \t]+"
        '((
 	  "custom1" "custom2" "custom3"
 	  ) ado-subcommand-face)
        "[ \t]+"
        '((
 	  "background_color"
 	  "pen1_color" "pen2_color" "pen3_color" "pen4_color" "pen5_color" "pen6_color" "pen7_color" "pen8_color" "pen9_color"
 	  "pen1_thick" "pen2_thick" "pen3_thick" "pen4_thick" "pen5_thick" "pen6_thick" "pen7_thick" "pen8_thick" "pen9_thick"
 	  "symmag_all"
 	  ) 
 	 ado-subcommand-face)
        "_*\\b"
        ))
     ;;
     ;; shoulda never started this - the gprefs query scheme layout all by its lonesome
     ;; comment region 9
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
 	  "gprefs"
 	  ) ado-builtin-harmless-face)
        "[ \t]+"
        '(("query") ado-subcommand-face)
        "[ \t]+"
        '((
 	  "custom1" "custom2" "custom3"
 	  ) ado-subcommand-face)
        "\\b"
        ))
     ;;; worst than smcl ---- it's graph!
     ;;;  -> will need multiple copies of the subcommands for the () and || and plain versions
     ;;;     argh, what a pain in the rear.
      (eval-when-compile
        (make-regexps
         "\\b"
         '(("gr" "gra" "grap" "graph") ado-builtin-harmless-face t)
         "[ \t]+"
         '((
	   "bar" "box"
	   "combine"
	   "des" "desc" "descr" "descri" "describ" "describe" 
	   "di" "dir" "dis" "disp" "displ" "displa" "display"
	   "dot"
	   "export"
	   "hbar" "hbox"
	   "matrix"
	   "pie" "print"
	   "q" "qu" "que" "quer" "query" 
	   "save" "set"
	   "tw" "two" "twow" "twowa" "twoway"
  	  ) ado-subcommand-face t)
         "\\b"
         ))
      ;; the initial graph commands which are destructive
      (eval-when-compile
        (make-regexps
         "\\b"
         '(("gr" "gra" "grap" "graph") ado-builtin-harmful-face t)
         "[ \t]+"
         '((
	   "drop\\([ \t]+_all\\)?"
	   "rename"
	   "use"
	   ) ado-subcommand-face t)
         "\\b"
         ))

      ;; the graph twoway stuff
      (eval-when-compile
        (make-regexps
         "\\b"
         '(("\\(\\(gr\\|gra\\|grap\\|graph\\)[ \t]+\\)?") ado-builtin-harmless-face)
         '(("tw" "two" "twow" "twowa" "twoway" ) ado-builtin-harmless-face t)
	 "[ \t]+"
	 '((
	    "area"
	    "bar"
	    "con" "conn" "conne" "connec" "connect" "connecte" "connected" 
	    "dot" "dropline"
	    "fpfit" "fpfitci" "function"
	    "hist" "histo" "histog" "histogr" "histogra" "histogram" 
	    "kdensity"
	    "line"
	    "lfit" "lfitci"
	    "lowess"
	    "mband" "mspline"
	    "pcarrow" "pcbarrow" "pcbarrowi" "pccapsym" "pci" "pcscatter" "pcspike"
	    "qfit" "qfitci"
	    "rarea" "rbar" "rcap" "rcapsym" 
	    "rcon" "rconn" "rconne" "rconnec" "rconnect" "rconnecte" "rconnected" 
	    "rl" "rli" "rlin" "rline" 
	    "rsc" "rsca" "rscat" "rscatt" "rscatte" "rscatter" 
	    "rspike"
	    "sc" "sca" "scat" "scatt" "scatte" "scatter" 
	    "scatteri" "spike"
	   ) ado-subcommand-face t)
         "\\b"
         ))
      ;; even more aggravating: things for which both graph and twoway are optional
       (eval-when-compile
         (make-regexps
          "\\b"
          '(("\\(\\(gr\\|gra\\|grap\\|graph\\)[ \t]+\\)?") ado-builtin-harmless-face)
          '(("\\(\\(tw\\|two\\|twow\\|twowa\\|twoway\\)[ \t]+\\)?" ) ado-builtin-harmless-face t)
 	 '((
 	    "tsline" "tsrline" ) ado-builtin-harmless-face t)
 	 "\\b"
 	 ))
	 
     ;; icd9, icd9p commands
     (eval-when-compile
       (make-regexps
	"\\b"
	'(("\\(icd\\(9\\|9p\\)\\)"
	   ) ado-builtin-harmless-face)
	"[ \t]+"
	'((
	   "check"
	   "l" "lo" "loo" "look" "looku" "lookup"
	   "q" "qu" "que" "quer" "query" 
	   "sea" "sear" "searc" "search" 
	  ) ado-subcommand-face)
	"\\b"
	))
     ;; icd9s with generate
     (eval-when-compile
       (make-regexps
	"\\b"
	'(("\\(icd\\(9\\|9p\\)\\)"
	   ) ado-builtin-harmless-face)
	"[ \t]+"
	'((
	   "clean"
	   "gen" "gene" "gener" "genera" "generat" "generate" 
	  ) ado-builtin-harmful-face)
	"\\b"
	))
     ;;
     ;; some of the matrix commands
     ;; with no matrix arguments - harmless
     ;; with one following argument but no subcommand
     (eval-when-compile
       (make-regexps
       "\\b"
       '(("mat" "matname" "mat_put_rr" "matr" "matri" "matrix"
	  ) ado-builtin-harmful-face)
       "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-matrix-name-face)
       "\\b"
       ))
     (eval-when-compile
       (make-regexps
       "\\b"
       '(("mat" "matr" "matri" "matrix"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "d" "di" "dir" "dispCns" 
	  "post"
	  ) 
	 ado-subcommand-face t)
       "\\b"
       ))
     ;; with no matrix arguments - harmful
     (eval-when-compile
       (make-regexps
       "\\b"
       '(("mat" "matr" "matri" "matrix"
	  ) ado-builtin-harmless-face t)
       "[ \t]+"
       '((
 	  "sco" "scor" "score" 
	  ) 
	 ado-subcommand-face t)
       "\\b"
       ))
     ;; with an indeterminant number of matrix arguments - harmful
     (eval-when-compile
       (make-regexps
       "\\b"
       '(("mat" "matr" "matri" "matrix"
	  ) ado-builtin-harmful-face)
       "[ \t]+"
       '((
	  "drop\\([\ t]+_all\\)?"
	  "makeCns"
	  ) 
	 ado-subcommand-face t)
       "\\b"
       ))
     ;; with one following argument
     (eval-when-compile
       (make-regexps
       "\\b"
       '(("mat" "matr" "matri" "matrix"
	  ) ado-builtin-harmful-face)
       "[ \t]+"
       '((
	  "ac" "acc" "accu" "accum" 
 	  "cole" "coleq" 
 	  "coln" "colna" "colnam" "cloname" "colnames"
 	  "def" "defi" "defin" "define"
	  "dis" "diss" "dissi" "dissim" "dissimi" "dissimil" "dissimila" "dissimilar" 
	  "dissimilari" "dissimilarit" "dissimilarity" 
 	  "glsa" "glsac" "glsacc" "glsaccu" "glsaccum"
 	  "in" "inp" "inpu" "input" 
;; 	  "mlou" "mlout"
 	  "opaccum"
 	  "rowe" "roweq" 
 	  "rown" "rowna" "rownam" "rowname" "rownames"
;; 	  "sub" "subs" "subst" "substi" "substit" "substitu" "substitut" "substitute" 
 	  "veca" "vecac" "vecacc" "vecaccu" "vecaccum"
	  )  ado-subcommand-face t)
       "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-matrix-name-face t)
       "\\b"
       ))
     ;; with one following arguments -- but harmless (good grief!)
     (eval-when-compile
       (make-regexps
       "\\b"
       '(("mat" "matr" "matri" "matrix"
	  ) ado-builtin-harmful-face)
       "[ \t]+"
       '((
	  "l" "li" "lis" "list" 
	  )  ado-subcommand-face)
       "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-matrix-name-face t)
       "\\b"
       ))
     ;; with two following arguments
     (eval-when-compile
       (make-regexps
       "\\b"
       '(("mat" "matr" "matri" "matrix"
	  ) ado-builtin-harmful-face)
       "[ \t]+"
       '((
 	  "eigenval" "eigenvalu" "eigenvalue" "eigenvalues" 
	  "ren" "rena" "renam" "rename" 
 	  "syme" "symei" "symeig" "symeige" "symeigen"
	  )  ado-subcommand-face t)
       "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-matrix-name-face t)
       "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-matrix-name-face t)
       "\\b"
       ))
     ;; with three(!) following arguments
     (eval-when-compile
       (make-regexps
       "\\b"
       '(("mat" "matr" "matri" "matrix"
	  ) ado-builtin-harmful-face)
       "[ \t]+"
       '(("svd" 
	  )  ado-subcommand-face t)
       "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-matrix-name-face t)
       "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-matrix-name-face t)
       "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-matrix-name-face t)
       "\\b"
       ))
     ;; with three(!) following arguments but no friggin matrix command
     (eval-when-compile
       (make-regexps
       "\\b"
       '(("matcproc"
	  ) ado-builtin-harmful-face)
       "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-matrix-name-face t)
       "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-matrix-name-face t)
       "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-matrix-name-face t)
       "\\b"
       ))
 
     ;; the ml commands
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("ml"
	   ) ado-builtin-harmless-face)
        "[ \t]+"
        '((
 	  "check" "clear" "count"
 	  "di" "dis" "disp" "displ" "displa" "display"
	  "foot" "footn" "footno" "footnot" "footnote"
 	  "gr" "gra" "grap" "graph"
 	  "init"
 	  "max" "maxi" "maxim" "maximi" "maximiz" "maximize"
 	  "me" "met" "meth" "metho" "method"
 	  "mod" "mode" "model"
	  "p" "pl" "plo" "plot" 
 	  "q" "qu" "que" "quer" "query"
 	  "rep" "repo" "repor" "report"
 	  "sea" "sear" "searc" "search"
 	  "trace")
 	 ado-subcommand-face)
        "\\b"
        ))
     ;; obsolete ml commands
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("ml") ado-builtin-harmless-face)
        "[ \t]+"
        '(("b" "be" "beg" "begi" "begin" 
 	  "de" "dep" "depn" "depna" "depnam" "depname" "depnames" 
 	  "f" "fu" "fun" "func" "funct" "functi" "functio" "function"
 	  "ml" "mlo" "mlou" "mlout"
 	  "pl" "plo" "plot"
 	  "po" "pos" "post" 
 	  "sa" "sam" "samp" "sampl" "sample")
 	 ado-obsolete-face)
        "\\b"
        ))
    ;; the net commands
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("net") ado-builtin-harmless-face)
       "[ \t]"
       '((
	  "cd"
	  "d" "de" "des" "desc" "descr" "descri" "describ" "describe"
	  "from" "get" 
	  "ins" "inst" "insta" "instal" "install" 
	  "link"
	  "q" "qu" "que" "quer" "query"
	  "search" "sj" "stb")
	 ado-subcommand-face)
       "\\b"
       ))
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("net") ado-builtin-harmless-face)
       "[ \t]+"
       '(("set") ado-builtin-harmless-face)
       "[ \t]+"
       '(("ado" "other") ado-subcommand-face)
       "\\b"
       ))
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("ado") ado-builtin-harmless-face)
       "[ \t]+"
       '(("d" "de" "des" "desc" "descr" "descri" "describ" "describe"
	  "dir"
	  "uninstall")
	 ado-subcommand-face)
       "\\b"
       ))
    ;; odbc commands 
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("odbc") ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "des" "desc" "descr" "descri" "describ" "describe"
	  "li" "lis" "list"
	  "q" "qu" "que" "quer" "query"
	 ) ado-subcommand-face)
       "\\b"
       ))
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("odbc") ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "exe" "exec" 
	  "in" "ins" "inse" "inser" "insert" 
	  "lo" "loa" "load" 
	  "sql" "sqlf" "sqlfi" "sqlfil" "sqlfile" 
	 ) ado-builtin-harmful-face)
       "\\b"
       ))
    ;; palette commands     (eval-when-compile
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("palette") ado-builtin-harmless-face)
       "[ \t]+"
       '((
 	  "color"
	  "line" "linep" "linepa" "linepal" "linepale" "linepalet" "linepalett" "linepalette" 
	  "symbol" "symbolp" "symbolpa" "symbolpal" "symbolpale" "symbolpalet" "symbolpalett" "symbolpalette" 
 	  ) ado-subcommand-face)
       "\\b"
       ))

    ;; postutil commands - both of them
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("postutil") ado-builtin-harmless-face)
       "[ \t]+"
       '(("dir") ado-subcommand-face t)
       ))
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("postutil") ado-builtin-harmful-face)
       "[ \t]+"
       '(("clear") ado-subcommand-face t)
       ))
    ;; query commands 
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "q" "qu" "que" "quer" "query"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "eff" "effi" "effic" "effici" "efficie" "efficien" "efficienc" "efficiency" 
	  "graph" "graphi" "graphic" "graphics" 
	  "inter" "interf" "interfa" "interfac" "interface"
	  "mata"
	  "mem" "memo" "memor" "memory" 
	  "net" "netw" "netwo" "networ" "network" 
	  "out" "outp" "outpu" "output" 
	  "oth" "othe" "other" 
	  "trace"
	  "up" "upd" "upda" "updat" "update" 
	 ) ado-subcommand-face)
       "\\b"
       ))
    
    ;; the reshape commands
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("reshape") ado-builtin-harmful-face)
       "[ \t]+"
       '((
	  "clear"
	  "error"
	  "i" "j"
	  "long"
	  "wide"
	  "xi" "xij")
	 ado-subcommand-face)
       "\\b"
       ))
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("reshape") ado-builtin-harmful-face)
       '(("\\([ \t]\\(q\\|qu\\|que\\|quer\\|query\\)\\)?"
	  ) ado-subcommand-face)
       "\\b"
       ))

    ;; the _return commands (not the return commands)
    (eval-when-compile
       (make-regexps
        "\\b"
        '(("_ret" "_retu" "_retur" "_return") ado-builtin-harmless-face)
        "[ \t]+"
        '((
	   "dir" "drop"
 	  "hold"
	  "res" "rest" "resto" "restor" "restore" 
 	  ) ado-subcommand-face t)
        "\\b"
        ))

    ;; the return commands
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("ret" "retu" "retur" "return") ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "add" "clear"
	  "li" "lis" "list" 
	  "loc" "loca" "local" 
	  "mat" "matr" "matri" "matrix" 
	  "sca" "scal" "scala" "scalar" 
	  ) ado-subcommand-face t)
       "\\b"
       ))
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("eret" "eretu" "eretur" "ereturn") ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "clear"
	  "di" "dis" "disp" "displ" "displa" "display" 
	  "li" "lis" "list" 
	  "loc" "loca" "local" 
	  "mat" "matr" "matri" "matrix" 
	  "post" "repost"
	  "sca" "scal" "scala" "scalar" 
	  ) ado-subcommand-face t)
       "\\b"
       ))
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("sret" "sretu" "sretur" "sreturn") ado-builtin-harmless-face)
       "[ \t]+"
       '(("clear" 
	  "li" "lis" "list" 
	  "loc" "loca" "local" 
	  ) ado-subcommand-face)
       "\\b"
       ))
    ;; scc commands
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("ssc") ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "copy"
	  "d" "de" "des" "desc" "descr" "descri" "describ" "describe" 
	  "inst" "insta" "instal" "install" 
	  "type" "uninstall"
	  "what" "whats" "whatsn" "whatsne" "whatsnew" 
	 ) ado-subcommand-face)
       "\\b"
       ))
    ;; the serset commands
    (eval-when-compile
       (make-regexps
        "\\b"
        '(("serset") ado-builtin-harmful-face)
        "[ \t]+"
        '((
	   "clear"
	   "cr" "cre" "crea" "creat" "create" "create_cspline" "create_xmedians"
	   "drop"
	   "reset_id"
	   "set" "sort"
	   "use"
	   ) ado-subcommand-face)
        "\\b"
        ))
    (eval-when-compile
       (make-regexps
        "\\b"
        '(("serset") ado-builtin-harmless-face)
        "[ \t]+"
        '((
	   "dir"
	   "su" "sum" "summ" "summa" "summar" "summari" "summariz" "summarize" 
	   ) ado-subcommand-face)
        "\\b"
        ))

    ;; the sts commands
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("sts") ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "g"
	  "gen" "gene" "gener" "genera" "generat" "generate"
	  "gr" "gra" "grap" "graph"
	  "l" "li" "lis" "list"
	  "t" "te" "tes" "test"
	  )
	 ado-subcommand-face)
       "\\b"
       ))
    ;; the sw commands
    ;; the sw commands are all now obsolete, because of syntax change
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("sw") ado-builtin-harmless-face)
       "[ \t]+"
       '(( 
	 "clogit" "cloglog" "cnreg" "cox" 
	 "ereg" 
	 "gamma" "glm" "gompertz" 
	 "hetprob" 
	 "llogistic" "lnormal" "logistic" "logit" "nbreg" "ologit" "oprobit"
	 "poisson" "probit" "qreg" "reg" "regr" "regre" "regres" "regress"
	 "scobit" "stcox" "streg" "tobit" "weibull"
	  )
	 ado-obsolete-face)
       "\\b"
       ))
    ;; the sysdir commands
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("sysdir") ado-builtin-harmless-face)
        "[ \t]+"
        '((
 	  "l" "li" "lis" "list"
	  "set"
 	  ) ado-subcommand-face t)
        "\\b"
        ))
    ;; the personal commands
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("personal") ado-builtin-harmless-face)
        "[ \t]+"
        '((
 	  "dir"
 	  ) ado-subcommand-face t)
        "\\b"
        ))
    ;; tsunab and unab commands
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("tsunab" "unab") ado-builtin-harmful-face)
        "[ \t]+"
	'(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-variable-name-face)
	"[ \t]*:[ \t]*"
        ))

    ;; the tssmooth commands
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("tssmooth") ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "d" "de" "dex" "dexp" "dexpo" "dexpon" "dexpone" "dexponen" "dexponent" "dexponenti" "dexponentia" "dexponential" 
	  "e" "ex" "exp" "expo" "expon" "expone" "exponen" "exponent" "exponenti" "exponentia" "exponential" 
	  "h" "hw" "hwi" "hwin" "hwint" "hwinte" "hwinter" "hwinters" 
	  "ma" "nl"
	  "s" "sh" "shw" "shwi" "shwin" "shwint" "shwinte" "shwinter" "shwinters" 
	  )
	 ado-subcommand-face)
       "\\b"
       ))
    ;;
    ;; the translator commands
    ;; comment region 12
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("translator") ado-builtin-harmless-face)
        "[ \t]+"
        '((
 	  "q" "qu" "que" "quer" "query" 
 	  "reset"
 	  "set"
 	  ) ado-subcommand-face)
        "\\b"
        ))
       ;;
       ;; the transmap commands
       ;; comment region 13
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("transmap") ado-builtin-harmless-face)
        "[ \t]+"
        '((
 	  "def" "defi" "defin" "define" 
 	  "q" "qu" "que" "quer" "query" 
 	  ) ado-subcommand-face)
        "\\b"
        ))
     ;; 
     ;; the update commands
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("update") ado-builtin-harmful-face)
        "[ \t]+"
        '((
 	  "ado" "all"
 	  "executable"
 	  "from"
	  "swap"
 	  ) ado-subcommand-face)
        "\\b"
        ))
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("update") ado-builtin-harmless-face)
        "[ \t]+"
        '((
 	  "q" "qu" "que" "quer" "query" 
 	  ) ado-subcommand-face)
        "\\b"
        ))
     ;; the fcast commands which leave data alone
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("fcast") ado-builtin-harmless-face)
        "[ \t]+"
        '((
 	  "g" "gr" "gra" "grap" "graph" 
 	  ) ado-subcommand-face)
        "\\b"
        ))
     ;; fcast commands which alter data
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("fcast") ado-builtin-harmful-face)
        "[ \t]+"
        '((
 	  "c" "co" "com" "comp" "compu" "comput" "compute" 
 	  ) ado-subcommand-face)
        "\\b"
        ))
     ;; the obsolete varfcast commands with sub commands
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("varfcast") ado-obsolete-face)
        "[ \t]+"
        '((
	   "c"
	   "cl" "cle" "clea" "clear" 
	   "co" "com" "comp" "compu" "comput" "compute" 
	   "g" "gr" "gra" "grap" "graph" 
 	  ) ado-subcommand-face)
        "\\b"
        ))
     ;; the irf functions which leave data alone
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("irf") ado-builtin-harmless-face)
        "[ \t]+"
        '((
 	  "cg" "cgr" "cgra" "cgrap" "cgraph" 
	  "ct" "cta" "ctab" "ctabl" "ctable" 
	  "d" "de" "des" "desc" "descr" "descri" "describ" "describe" 
	  "di" "dir"
	  "g" "gr" "gra" "grap" "graph" 
	  "og" "ogr" "ogra" "ograp" "ograph" 
	  "t" "ta" "tab" "tabl" "table" 
 	  ) ado-subcommand-face)
        "\\b"
        ))
     ;; 
     ;; the irf commands which alter data
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("irf") ado-builtin-harmful-face)
        "[ \t]+"
        '((
 	  "a" "ad" "add" 
	  "cr" "cre" "crea" "creat" "create" 
	  "drop"
	  "ren" "rena" "renam" "rename" 
	  "set"
 	  ) ado-subcommand-face)
        "\\b"
        ))
     ;; the irf functions which are obsolete
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("irf") ado-builtin-harmless-face)
        "[ \t]+"
        '((
	  "di" "dir"
	  "erase"
 	  ) ado-obsolete-face)
        "\\b"
        ))

     ;; obsolete varirf functions
     ;; the varirf functions which leave data alone
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("varirf") ado-obsolete-face)
        "[ \t]+"
        '((
 	  "a" "ad" "add" 
 	  "cg" "cgr" "cgra" "cgrap" "cgraph" 
	  "cr" "cre" "crea" "creat" "create" 
	  "ct" "cta" "ctab" "ctabl" "ctable" 
	  "d" "de" "des" "desc" "descr" "descri" "describ" "describe" 
	  "di" "dir"
	  "drop" "erase"
	  "g" "gr" "gra" "grap" "graph" 
	  "og" "ogr" "ogra" "ograp" "ograph" 
	  "ren" "rena" "renam" "rename" 
	  "set"
	  "t" "ta" "tab" "tabl" "table" 
 	  ) ado-subcommand-face)
        "\\b"
        ))

     ;;
     ;; the view commands
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("view") ado-builtin-harmless-face)
        "[ \t]+"
        '((
 	  "ado" "ado_d"
 	  "browse"
	  "file"
 	  "help" "help_d"
 	  "net" "net_d" "news"
 	  "search" "search_d"
 	  "view_d"
 	  "update" "update_d"
 	  ) ado-subcommand-face)
        "\\b"
        ))
     ;; the webuse commands
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("webuse") ado-builtin-harmless-face)
        "[ \t]+"
        '((
	   "query" "set"
 	  ) ado-subcommand-face)
        "\\b"
        ))
    ;; the window commands
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "win" "wind" "windo" "window"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "d"
	  "di" "dia" "dial" "dialo" "dialog"
	  "dir" "drop"
	  "fo" "fop" "fope" "fopen"
	  "fs" "fsa" "fsav" "fsave"
	  "l" "list"
	  "push"
	  ) ado-subcommand-face)
       "\\b"
       ))
    ;; the window controls
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "win" "wind" "windo" "window"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "c" "co" "con" "cont" "contr" "contro" "control"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "button" "check" "clear"
	  "edit"
	  "mcombo" "msimple"
	  "radbegin"
	  "radend"
	  "radio"
	  "scombo"
	  "ssimple"
	  "static"
	  ) ado-subcommand-face)
       "\\b"
       ))
    ;; the window manage commands
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "win" "wind" "windo" "window"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "man" "mana" "manag" "manage"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "associate"
	  "close[ \t]+\\(graph\\|viewer\\)"
	  "maintitle\\([ \t]+reset\\)?"
	  "minimize" 
	  "prefs[ \t]+load" 
	  "prefs[ \t]+save" 
	  "prefs[ \t]+default"
	  "print[ \t]+graph"
	  "print[ \t]+viewer"
	  "rename[ \t]+graph"
	  "restore"
	  "update[ \t]+variable"
	  ) 
	 ado-subcommand-face)
       "\\b"
       ))
    ;; the window manage forward commands [sheesh]
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "win" "wind" "windo" "window"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "man" "mana" "manag" "manage"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '(("forward") ado-subcommand-face)
       "[ \t]+"
       '(("command" "doeditor" "graph" "help" "results" "review" "variables" "viewer"
	  ) 
	 ado-subcommand-face)
       "\\b"
       ))
    ;; the window manage close/print commands [sheesh]
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "win" "wind" "windo" "window"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "man" "mana" "manag" "manage"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '(("forward") ado-subcommand-face)
       "[ \t]+"
       '(("command" "doeditor" "graph" "help" "results" "review" "variables" "viewer"
	  ) 
	 ado-subcommand-face)
       "\\b"
       ))
    ;; the window menu commands
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "win" "wind" "windo" "window"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "m" "me" "men" "menu"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "add_recentfiles"
	  "append[ \t]+item"
	  "append[ \t]+separator"
	  "append[ \t]+submenu"
	  "clear"
	  "refresh"
	  ) 
	 ado-subcommand-face)
       "\\b"
       ))
    ;; the window stopbox commands
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "win" "wind" "windo" "window"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "stop" "stopb" "stopbo" "stopbox"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "note"
	  "rusure"
	  "stop"
	  ) 
	 ado-subcommand-face)
       "\\b"
       ))
    ;; the xwindow commands
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "xwin" "xwind" "xwindo" "xwindow"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "de" "def" "defi" "defin" "define" 
	  "di" "dir"
	  "drop"
	  "l" "li" "lis" "list"
	  ) 
	 ado-subcommand-face)
       "\\b"
       ))

;; all the endless Stata keywords (not in a good order)
;; first those keywords which must start line
;; note that these will look like text if preceded by a comment
;; (but comments shouldn't be before the command, anyway, 'cuz it makes things hard to read)

    (eval-when-compile
      (make-regexps
       "^[ \t]*"
       '((
	  "by"
	  "cap" "capt" "captu" "captur" "capture"
	  "char" "err" "erro" "error" "e" "ex" "exi" "exit" 
	  "for"
	  "set"
	  ) ado-builtin-harmless-face)
       "\\b"
       ))
    ;; here are some keywords which appear in the middle of lines
    ;; note that the really short abbreviations could make a mess of things
    ;;
    ;; These are split to allow compiling!
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "_coef_table"
	  "_huber" "_qreg" "_rmcoll" "_rmdcoll" "_robust"
	  "#r" "#re" "#rev" "#revi" "#revie" "#review" 
	  "about" "ac" "acprplot" "adjust" 
	  "ado" "adopath" "alpha" "ameans" 
	  "an" "ano" "anov" "anova" 
	  "arch" "areg" "arima" 
	  "as" 
	  "asmprobit"
	  "ass" "asse" "asser" "assert" 
	  "avplot" "avplots"
	  "b" "be" "bee" "beep"
	  "binreg" "biprobit" "biplot" "bitest" "bitesti" "blogit"
	  "bootstrap" "boxcox" "bprobit" "br" "break" "brier" 
	  "bro" "brow" "brows" "browse" 
	  "bsqreg" "bstat"
	  "ca" "cabiplot" 
	  "canon" "caprojection" "cat" "cc" "cci" "cchart" "centile" "cf" 
	  "ch" "che"
	  "checksum" 
	  "chel" "chelp"
	  "ci" "cii" 
	  "clog" "clogi" "clogit" "clogitp" "cloglog"
	  "close" "cluster" "cmdlog" "cmdtool" 
	  "cnr" "cnre" "cnreg" "cnsreg" "codebook" "compare" "continue"
	  "copy" "copyright" 
	  "cor" "corc" "corr" "corre" "correl" "correla" "correlat" "correlate"
	  "corrgram"
	  "cou" "coun" "count" 
	  "cox"	"cprplot" "_crcswxx" "cs" "csi" 
	  "ct" "ctset" 
	  "cumsp" "cumul" "cusum")
	 ado-builtin-harmless-face)
       "\\b"
       ))
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "d" 
	  "db"
	  "de" "des" "desc" "descr" "descri" "describ" "describe"
	  "dfbeta" "dfgls" "dfuller" "di"
	  "dir" "dis" "disp" "disp_res" "disp_s" 
	  "displ" "displa" "display"
	  "do" 
	  "doed" "doedi" "doedit" 
	  "dotplot"
	  "dprobit" "ds" "dstdize" 
	  "eivreg" "eq" "ereg"
	  "fac" "fact" "facto" "factor" "factormat"
	  "findfile" "findit" "fit"
	  "fl" "fli" "flis" "flist"
	  "for" "fpredict" 
	  "fracplot" "fracpoly" "frontier" "fsl"
	  ) ado-builtin-harmless-face)
       "\\b"
       ))
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "gamma" "gammahet" "gladder" "gllamm" "glm" "glmpred" "glogit" "gnbreg" "gompertz"
	  "gphdot" "gphpen" "gprobit" "gr7" "graph7" "greigen" "grmeanby"
	  "h"
	  "hadimvo" "hausman" "heckman" "heckprob" 
	  "he" "hel" "help" 
	  "hetprob" "hexdump" "hilite"
	  "hist" "histo" "histog" "histogr" "histogra" "histogram" 
	  "hlu" "hotel" "hotelling"
	  "iqreg" "istdize" "iis" 
	  "ins" "insp" "inspe" "inspec" "inspect"
	  "intreg" "ir" "iri" "isid" "ivprobit" "ivreg" "ivtobit"
	  "jackknife"
	  "kap" "kappa" "kapwgt" "kdensity" "ksm" "ksmirnov" "ktau"
	  "kwallis"
	  ) ado-builtin-harmless-face)
       "\\b"
       ))
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "l"
	  "labelbook" "ladder"
	  "levelsof"
	  "li" "line"
	  "lincom" "linktest" 
	  "lis" "list"
	  "lo" "loadingplot" "log"
	  "logi" "logistic" "logit" 
	  "loneway" "lookfor" "lowess" 
	  "lpredict" "lroc" "lrtest" "ls" "lsens" "ltable" "lv" "lvr2plot"
	  "man" "mano" "manov" "manova" "manovatest" "matlist"
	  "mcc" "mcci" 
	  "mds" "mdsconfig" "mdslong" "mdsmat" "mdsshepard"
	  "mean" "median" "memory" "mfx" "mhodds"
	  "mlog" "mlogi" "mlogit"
	  "mor" "more"
	  "mprobit" "mvreg" "mx_param"
	  "n" "nbreg" "net" "newey" "news"
	  "nl" "nlcom" "nlogit" 
	  "no" "noi" "nois" "noisi" "noisil" "noisily"
	  "note" "notes"
	  "numlabel"
	  "nptrend" "numlist"
	  "olog" "ologi" "ologit"
	  "ologitp" 
	  "on" "one" "onew" "onewa" "oneway"
	  "oprob" "oprobi" "oprobit"
	  "oprobitp")
	 ado-builtin-harmless-face)
       "\\b"
       ))
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("pac" "pca" "pcamat" "pchart" "pchi" "pcorr" "pergram" "permute" "personal"
	  "pkcross" "pkequiv" "pkexamine" "pksumm"
	  "pl" "plo" "plot"
	  "pnorm" "poisson" "pperron"
	  "prais" "print"
	  "prob" "probi" "probit"
	  "procoverlay" "procrustes" "proportion"
	  "prtest" "prtesti"
	  "pwcorr" "pwd"
	  "q" "qchi" "qnorm" "qqplot" "qreg" "qladder" "quadchk" "quantile" 
	  "qu" "que" "quer" "query"
	  "qui" "quie" "quiet" "quietl" "quietly"
	  "ranksum" "ratio" "rchart" "regdw" "regph" 
	  "reg" "reg3" "regr" "regre" "regres" "regress"
	  "robvar"
	  "roccomp" "rocfit" "rocgold" "rocplot" "roctab"
	  "rologit" "rolling"
	  "rot" "rota" "rotat" "rotate"
	  "rotatemat"
	  "rreg"
	  "ru" "run" "runtest" "rvfplot" "rvpplot"
	  ) ado-builtin-harmless-face)
       "\\b"
       ))
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "sampsi" "sconfirm" 
	  "sc" "sca" "scat" "scatt" "scatte" "scatter" 
	  "scobit" "scoreplot" "screeplot"
	  "sdtest" "sdtesti" "search" "serrbar" "serset"
	  "set_defaults"
	  "sfrancia" 
	  "sh" "she" "shewhart" "shel" "shell" 
	  "signrank" "signtest"
	  "sktest" "sleep" "slog" "slogit" "spearman" "spikeplot" "sqreg"
	  "ssc"
	  "st" "st_is" "st_show" "st_ct" "stci"
	  "stcox" "stcoxkm" "stcurv" "stcurve" "stdes"
	  "stem" "stepwise"
	  "stereg" "stir" "stmc" "stmh" "stphplot" "stptime" 
	  "strate" "streg" "streset"
	  "sts" "stse" "stset" "stsum" "stvary" "stweib"
	  "su" "suest" "sum" "summ" "summa" "summar" "summari" "summariz" "summarize"
	  "sureg" "sunflower" "svar"
	  "svydes" "svyset"
	  "sw" "swilk" "symmetry" "symmi" "symplot" "syntax" "sysdir" 
	  ) ado-builtin-harmless-face)
       "\\b"
       ))
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "ta" "tab" 
	  "tab1" "tab2" 
	  "tabdisp"
	  "tabi" 
	  "table" "tabodds" "tabstat"
	  "tabu" "tabul" "tabula" "tabulat" "tabulate"
	  "te" "tes" "test"
	  "testnl" "testparm" "tetrachoric" "tis" 
	  "tob" "tobi" "tobit"
	  "token" "tokeni" "tokeniz" "tokenize" 
	  "total" "touch" 
	  "translator" "transmap" "treatreg" "truncreg"
	  "tsreport" "tsset" "tssmooth" "tsunab" "ttest" "ttesti"
	  "ty" "typ" "type"
	  "unab" "unabcmd" "update" "using"
	  "var" "varbasic" "vargranger"  
	  "varlmar" "varnorm" "varsoc" "varstable" "varwle" 
	  "vec" "veclmar" "vecnorm" "vecrank" "vecstable"
	  "verinst" "view" "viewsource" "vwls"
	  "weibull" "which" "who" "wntestb" "wntestq" 
	  "xchart" "xcorr"
	  "xsh" "xshe" "xshel" "xshell" 
	  "xtabond" "xtclog" "xtcloglog" "xtdes" "xtfrontier"
	  "xtgee" "xtgls" "xthtaylor" "xtintreg" "xtivreg"
	  "xtline" "xtlogit" "xtmixed" "xtnbreg" "xtpcse" "xtpois" "xtpoisson" "xtprobit"
	  "xtrc" "xtreg" "xtregar" "xtsum" "xttab" "xttest0" "xttobit" "xttrans"
	   "zinb" "zip" "ztnb" "ztb"
	  ) ado-builtin-harmless-face)
       "\\b"
       ))

    ;; things to use with the svy ... : prefix
    ;; can be fooled by svy brr, nothing but garbage:
    ;; another problem: don't know how to make the hilighted stuff in the middle
    ;;  optional
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("svy") ado-builtin-harmless-face)
       "[ \t]*\\(,\\)?.*?:[ \t]*"
       '((
	  "gnbreg" 
	  "heckman" "heckprob" 
	  "intreg" "ivreg" 
	  "logistic" "logit"
	  "mean" "mlogit" 
	  "nbreg" "ologit" "oprobit"
	  "poisson" "probit" "proportion" 
	  "ratio" 
	  "reg" "regr" "regre" "regres" "regress" 
	  "tab" "tabu" "tabul" "tabula" "tabulat" "tabulate" 
	  "total"
	  ) ado-builtin-harmless-face)
       "\\b"
       ))
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("svy") ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "brr"
	  "jack" "jackk" "jackkn" "jackkni" "jackknif" "jackknife" 
	  "linear" "lineari" "lineariz" "linearize" "linearized" 
	  ) ado-subcommand-face t)
       ".*?\\(,\\)?.*?:[ \t]*"
       '((
	  "gnbreg" 
	  "heckman" "heckprob" 
	  "intreg" "ivreg" 
	  "logistic" "logit"
	  "mean" "mlogit" 
	  "nbreg" "ologit" "oprobit"
	  "poisson" "probit" "proportion" 
	  "ratio" 
	  "reg" "regr" "regre" "regres" "regress" 
	  "tab" "tabu" "tabul" "tabula" "tabulat" "tabulate" 
	  "total"
	  ) ado-builtin-harmless-face)
       "\\b"
       ))

    ;; haver subcommands
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("haver") ado-builtin-harmless-face)
       "[ \t]+"
       '(("des" "desc" "descr" "descri" "describ" "describe") ado-builtin-harmless-face)))
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("haver") ado-builtin-harmful-face)
       "[ \t]+"
       '(("use") ado-builtin-harmful-face)))

    ;; Conditional statements 
    ;; if might not work right ('cuz it is also a keyword)
    (eval-when-compile
      (make-regexps
       "\\w+[ \t]+"
       '(("if"
	  ) ado-builtin-harmless-face t)
       "\\b"
       ))

    (eval-when-compile
      (make-regexps
       "^[ \t]*"
       '(("if" "while"
	  ) ado-builtin-harmless-face t)
       "[ \t]+.*?[ \t]+.+?"
       ))
    ;; else statement (which may just have a {)
    (eval-when-compile
      (make-regexps
       "^[ \t]*"
       '(("else"
	  ) ado-builtin-harmless-face)
       "[ \t]+?.+?"
       ))

    ;; short version of list --- which can get fooled if used as a var
    (eval-when-compile
      (make-regexps
       '(("^[ \t]*l\\b" 
	  ) ado-builtin-harmless-face)
       ))

;; all the Stata options
    ;; commonly used options
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("byte" "int" "long" "str" "str[1-7]?[0-9]?" "str80" "float" "double"
	  "width" "maxobs" "maxvar"
	  ) ado-subcommand-face)
       "\\b"
       ))
    ;; special local variables (used in parsing)
    ;; now obsolete, no?
    (eval-when-compile
      (make-regexps
       "^[ \t]*\\(local\\)+[ \t]+"
       '(("varlist" "exp" "weight" "if" "in" "using" "options"
	  ) ado-subcommand-face nil t t)
       "\\b"
       ))

    ;; things used with display
    ;; since these are often split across lines, and Stata commands are hard
    ;; to delimit, this will highlight even if out of context

    ;;
    ;; plain display
    ;;
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("di" "dis" "disp" "displ" "displa" "display") ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "_asis"
	  "_c" "_co" "_con" "_cont" "_conti" "_contin" "_continu" "_continue"
	  "_n" "_ne" "_new" "_newl" "_newli" "_newlin" "_newline" 
	  "_quote"
	  ) 
	 ado-subcommand-face)
       "\\b"
       ))
    ;;
    ;; request (for a local macro)
    ;;
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("di" "dis" "disp" "displ" "displa" "display") ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "_r" "_re" "_req" "_requ" "_reque" "_reques" "_request"
	  ) 
	 ado-subcommand-face)
       "([ \t]*"
       '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-variable-name-face)
       "[ \t]*)"
       ))
    ;;
    ;; things which take numbers
    ;; some left as is, because they are used in dictionaries...
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("di" "dis" "disp" "displ" "displa" "display") ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "_char"
	  "_col" "_colu" "_colum" "_column"
	  "_d" "_du" "_dup"
	  "_n" "_ne" "_new" "_newl" "_newli" "_newlin" "_newline"
	  "_s" "_sk" "_ski" "_skip"
	  ) 
	 ado-subcommand-face)
       "([ \t]*"
       '(("[1-9]+[0-9]*") ado-constant-face)
       "[ \t]*)"
       ))
    ;;
    ;; other display subcommands
     (eval-when-compile
       (make-regexps
       '(("di" "dis" "disp" "displ" "displa" "display") ado-builtin-harmless-face)
       "[ \t]+"
	'(("as") ado-builtin-harmless-face)
	"[ \t]+"
        '((
 	  "err" "erro" "error" 
 	  "inp" "inpu" "input"
 	  "res" "resu" "resul" "result" 
 	  "text" "txt"
 	  ) ado-subcommand-face)
        "\\b"
        ))
    ;; trust Stata corp to use different prepositions...
     (eval-when-compile
       (make-regexps
        "\\b"
	'(("di" "dis" "disp" "displ" "displa" "display" ) ado-builtin-harmless-face)
	"[ \t]+"
	'(("in") ado-builtin-harmless-face)
	"[ \t]+"
        '((
	   "smcl"
	   ) ado-subcommand-face)
        "\\b"
        ))
     ;;
     ;; obsolete coloring
     (eval-when-compile
       (make-regexps
        "\\b"
       '(("di" "dis" "disp" "displ" "displa" "display") ado-builtin-harmless-face)
       "[ \t]+"
       '(("in") ado-obsolete-face)
       "[ \t]+"
        '((
 	  "b" "bl" "blu" "blue" 
 	  "g" "gr" "gre" "gree" "green"
 	  "r" "re" "red"
 	  "w" "wh" "whi" "whit" "white"
 	  "y" "ye" "yel" "yell" "yello" "yellow"
 	  ) ado-obsolete-face)
        "\\b"
        ))
     ;;
     ;; foreach ... in
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("foreach") ado-builtin-harmless-face)
        "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-variable-name-face)
        '(("[ \t]+in[ \t]+") ado-subcommand-face t)
        ))
       ;; 
       ;; foreach ... of
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("foreach") ado-builtin-harmless-face)
        "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-variable-name-face t)
        '(("[ \t]+of[ \t]+") ado-subcommand-face t)
        '((
 	  "glo" "glob" "globa" "global"
 	  "loc" "loca" "local" 
 	  "new" "newl" "newli" "newlis" "newlist" 
	  "num" "numl" "numli" "numlis" "numlist" 
	  "var" "varl" "varli" "varlis" "varlist" 
 	  ) ado-subcommand-face t)
        "\\b"
        ))
       ;; forvalues ... = ??
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("forv" "forva" "forval" "forvalu" "forvalue" "forvalues") ado-builtin-harmless-face)
        "[ \t]+"
        '(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-variable-name-face t)
        "[ \t]*=[ \t]*"
        ))

     ;; gettoken
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("gettoken") ado-builtin-harmless-face)
        '(("\\([ \t]+[a-zA-Z]+[a-zA-Z0-9_]*\\)\\{1,2\\}") ado-variable-name-face t)
        "[ \t]*:[ \t]*"
	'(("[a-zA-Z]+[a-zA-Z0-9_]*") ado-variable-name-face t)
        ))
     

    
    ;; labels
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("la" "lab" "labe" "label"
	  ) ado-builtin-harmless-face t)
       "[ \t]"
       '((
	  "da" "dat" "data"
	  "de" "def" "defi" "defin" "define"
	  "di" "dir" 
	  "drop" 
	  "lang" "langu" "langua" "languag" "language" 
	  "l" "li" "lis" "list"
	  "save"
	  "val" "valu" "value" "values"
	  "var" "vari" "varia" "variab" "variabl" "variable"
	  ) ado-subcommand-face t) ; was nil t t
       "\\b"
       ))
    ;; mfp arguments
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("mfp") ado-builtin-harmless-face t)
       "[ \t]"
       '(( 
	  "clogit" "glm" "logistic" "logit" "poisson"
	  "probit" "qreg" "regress" "stcox" "streg" "xtgee"
	  ) ado-subcommand-face t) 
       "\\b"
       ))
    ;; mfx
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("mfx") ado-builtin-harmless-face t)
       "[ \t]+"
       '(("c" "co" "com" "comp" "compu" "comput" "compute" 
	  "r" "re" "rep" "repl" "repla" "replay" 
	  ) ado-subcommand-face t) 
       "\\b"
       ))
    
;; all Stata data-altering stuff
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "_pctile" "_predict"
	  "aorder" 
	  "ap" "app" "appe" "appen" "append" 
	  "bcskew0" "bs" "bsample" "bstrap"
  	  "bys" "byso" "bysor" "bysort" 
	  "cd" "chdir" "clear" "clonevar" "collapse" "compress" 
	  "contract" "convert" "corr2data" "cross" "cttost" 
	  "dec" "deco" "decod" "decode" "destring"
	  "discard" "drawnorm" "drop" "dydx"
	  "ed" "edi" "edit" "egen" 
	  "en" "enc" "enco" "encod" "encode"
	  "erase"
	  "expand" "expandcl"
	  "fdades" "fdadesc" "fdadescr" "fdadescri" "fdadescrib" "fdadescribe" 
	  "fdasav" "fdasave" 
	  "fdause"
	  "filef" "filefi" "filefil" "filefilt" "filefilte" "filefilter" 
	  "fillin"
	  "form" "forma" "format"
	  "fracgen" "fracpred"
	  "g" "ge" "gen" "gene" "gener" "genera" "generat" "generate"
	  "gsort"
	  "impute" 
	  "infile" "infix" 
	  "inp" "inpu" "input"
	  "insheet" "integ" "ipolate" 
	  "joinby"
	  "keep" 
	  "lnskew0"
	  ) ado-builtin-harmful-face)
       "\\b"
       ))
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "makecns"
	  "mark" "markin" "markout" "marksample"
	  "matrix"
	  "mer" "merg" "merge"
	  "mkdir" "mkmat" "mkspline"
	  "mleval" "mlmatsum" "mlsum""mlvecsum"
	  "modify" "mov" "move"
	  "mvdecode" "mvencode" "nlpred" "nobreak" 
	  "order" "orthog" "orthpoly"
	  "ou" "out" "outf" "outfi" "outfil" "outfile"
	  "outs" "outsh" "outshe" "outshee" "outsheet"
	  "pctile" 
	  "pkcollapse" "pkshape"
	  "post" "postclose" "postfile" 
	  "predict" "predictnl"
	  "preserve" "range"
	  "recast" "recode" 
	  "ren" "rena" "renam" "rename"
	  "renpfix" "replace" "restore" "rm" "rmdir"
	  "sappend" 
	  "sa" "sav" "save" "saveold"
	  "sample" "sdrop"
	  "separate"
	  "simul" "simulate" "sinfile" "smerge" 
	  "smooth" "snapspan" 
	  "so" "sor" "sort" "sortpreserve"
	  "split"
	  "ssave" "ssort" "stack" "statsby"
	  "stbase" "stfill" "stgen" "stjoin" "stsplit" "sttocc" "sttoct"
	  "suse" "svmat" "svymarkout" "sysuse"
	  "translate"
	  "tsappend" "tsfill" "tsrevar"
	  "u" "us" "use" "uselabel"
	  "webuse"
	  "xi" "xi:" 
	  "xmlsav" "xmlsave" 
	  "xtile" "xpose" 
	  "xtdata" "xtpred"
	  ) ado-builtin-harmful-face)
       "\\b"
       ))

    ;; assignment of macros
    ;;  local macros have different naming conventions (boo)
    (eval-when-compile
      (make-regexps
       "[ \t]*"
       '((
	  "gl" "glo" "glob" "globa" "global" 
	  "sca" "scal" "scala" "scalar" 
	  ) ado-builtin-harmful-face)
       "[ \t]+\\(--\\|++\\|`\\)?"
       '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
	  ) ado-variable-name-face t)
       ))
    (eval-when-compile
      (make-regexps
       "[ \t]*"
       '((
	  "lo" "loc" "loca" "local" 
	  ) ado-builtin-harmful-face)
       "[ \t]+\\(--\\|++\\|`\\)?"
       '(("[a-zA-Z_0-9]+"
	  ) ado-variable-name-face t)
       "\\b"
       ))
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("sca" "scal" "scala" "scalar" ) ado-builtin-harmful-face)
       '(("\\([ \t]+\\(def\\|defi\\|defin\\|define\\)\\)?" ) ado-subcommand-face)
       "[ \t]+"
       '(("[a-zA-Z_][a-zA-Z0-9_]*") ado-variable-name-face t)
       ))
    (eval-when-compile
      (make-regexps
       "[ \t]*"
       '((
	  "gl" "glo" "glob" "globa" "global" 
	  "ma" "mac" "macr" "macro"
	  "sca" "scal" "scala" "scalar" 
	  ) ado-builtin-harmful-face)
       "[ \t]+"
       '(("drop") ado-subcommand-face t)
       "[ \t]+"
       '(("\\([a-zA-Z_]+[a-zA-Z_0-9]*\\b\\)?"
	  ) ado-variable-name-face t)
       ))
    (eval-when-compile
      (make-regexps
       "[ \t]*"
       '((
	  "lo" "loc" "loca" "local" 
	  ) ado-builtin-harmful-face)
       "[ \t]+"
       '(("drop") ado-subcommand-face t)
       "[ \t]+"
       '(("\\([a-zA-Z_0-9]+\\b\\)?"
	  ) ado-variable-name-face t)
       ))
    
    ;;
    ;; an attempt to include the extended macro names (which will probably fail)
    ;; single word extended macro names
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
	   "gl" "glo" "glob" "globa" "global" 
	   "loc" "loca" "local" 
 	  ) ado-builtin-harmful-face t)
	"[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
 	  ) ado-variable-name-face t)
        "[ \t]*:[ \t]*"
        '((
 	  "char" "cole" "coleq" 
	  "colf" "colfu" "colful" "colfull" "colfulln" "colfullna" "colfullnam" "colfullname" "colfullnames" 
	  "coln" "colna" "colnam" "colname" "colnames" 
	  "constraint"
 	  "dirsep" 
 	  "di" "dir" "dis" "disp" "displ" "displa" "display" 
 	  "env" "envi" "envir" "enviro" "environ" "environm" "environme" "environmen" "environment" 
 	  "f" "fo" "for" "form" "forma" "format" 
 	  "lab" "labe" "label"
	  "list"
 	  "permname" "piece" "properties" "pwd"
	  "rowe" "roweq" 
	  "rowf" "rowfu" "rowful" "rowfull" "rowfulln" "rowfullna" "rowfullnam" "rowfullname" "rowfullnames" 
	  "rown" "rowna" "rownam" "rowname" "rownames" 
 	  "sort" "sorte" "sorted" "sortedb" "sortedby" 
 	  "tempf" "tempfi" "tempfil" "tempfile" "tempv" "tempva" "tempvar" 
	  "tsnorm" 
	  "ty" "typ" "type"
 	  "word\\([ \t]+count\\)?"
 	  ) ado-subcommand-face t)
	"\\b"
        ))
     ;; things with parens in them (sheesh)
     ;; not included above, incase someone uses a font which has a background color
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
	   "gl" "glo" "glob" "globa" "global" 
	   "loc" "loca" "local" 
 	  ) ado-builtin-harmless-face t)
	"[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
 	  ) ado-variable-name-face t)
        "[ \t]*:[ \t]*"
        '(("e" "r") ado-subcommand-face t)
	"([ \t]*"
	'(("functions" "macros" "matrices" "scalars"
 	  ) ado-subcommand-face t)
	"[ \t]*)"
        ))
     ;; damn s(macros)
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
	   "gl" "glo" "glob" "globa" "global" 
	   "loc" "loca" "local" 
	   ) ado-builtin-harmless-face t)
	"[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
	   ) ado-variable-name-face t)
        "[ \t]*:[ \t]*"
        '(("s") ado-subcommand-face t)
	"([ \t]*"
	'(("macros") ado-subcommand-face t)
	"[ \t]*)"
        ))
     ;; twin word macros length and subinstr
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
	   "gl" "glo" "glob" "globa" "global" 
	   "loc" "loca" "local" 
	   ) ado-builtin-harmless-face t)
	"[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
	   ) ado-variable-name-face t)
        "[ \t]*:[ \t]*"
        '(("length" "subinstr") ado-subcommand-face t)
	"[ \t]+"
	'((
	   "gl" "glo" "glob" "globa" "global" 
	   "loc" "loca" "local" ) ado-subcommand-face t)
	"\\b"
        ))
     ;; serset macros
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
	   "gl" "glo" "glob" "globa" "global" 
	   "loc" "loca" "local" 
	   ) ado-builtin-harmful-face t)
	"[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
	   ) ado-variable-name-face t)
        "[ \t]*:[ \t]*"
        '(("serset") ado-subcommand-face t)
	"[ \t]+"
	'((
	   "N"
	   "format" "k" "id" "max" "min" "type" "varnames" "varnum"
	   ) ado-subcommand-face t)
	"\\b"
        ))
     ;;
     ;; sheesh, now there are combined abbreviations!
     ;;
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
	   "gl" "glo" "glob" "globa" "global" 
	   "loc" "loca" "local" 
 	  ) ado-builtin-harmful-face t)
	"[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
 	  ) ado-variable-name-face t)
        "[ \t]*:[ \t]*"
        '((
 	  "data"
 	  "val" "valu" "value" 
 	  "var" "vari" "varia" "variab" "variabl" "variable" 
 	  ) ado-subcommand-face t)
        "[ \t]+"
        '((
 	  "l" "la" "lab" "labe" "label"
 	  ) ado-subcommand-face t)
        "\\b"
        ))
     ;; macro list commands start here
     ;; single word commands
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
	   "gl" "glo" "glob" "globa" "global" 
	   "loc" "loca" "local" 
	   ) ado-builtin-harmful-face t)
	"[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
	   ) ado-variable-name-face t)
        "[ \t]*:[ \t]*"
	'(("list") ado-subcommand-face t)
	"[ \t]+"
        '((
	   "clean"
	   "dups"
	   "retok" "retoke" "retoken" "retokeni" "retokeniz" "retokenize"
	   "sizeof"
	   "sort"
	   "uniq"
	   ) ado-subcommand-face t)
	"[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
	   ) ado-variable-name-face t)
	"\\b"
        ))
     ;; operator-like word commands
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
	   "gl" "glo" "glob" "globa" "global" 
	   "loc" "loca" "local" 
	   ) ado-builtin-harmless-face t)
	"[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
	   ) ado-variable-name-face t)
        "[ \t]*:[ \t]*"
	'(("list") ado-subcommand-face t)
	"[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
	   ) ado-variable-name-face t)
	"[ \t]*\\([|&-]\\|==\\|===\\|in\\)[ \t]*"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
	   ) ado-variable-name-face t)
	"\\b"
        ))
     ;; friggin' posof subcommand
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
	   "gl" "glo" "glob" "globa" "global" 
	   "loc" "loca" "local" 
	   ) ado-builtin-harmless-face t)
	"[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
	   ) ado-variable-name-face t)
        "[ \t]*:[ \t]*"
	'(("list") ado-subcommand-face t)
	"[ \t]+"
	'(("posof" ) ado-subcommand-face t)
	"[ \t]+\".*?\"[ \t]+"
	'(("in") ado-subcommand-face t)
	"[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
	   ) ado-variable-name-face t)
	"\\b"
        ))
     ;; all subcommand added to build 01-jul-2004
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
	   "gl" "glo" "glob" "globa" "global" 
	   "loc" "loca" "local" 
	   ) ado-builtin-harmless-face t)
	"[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
	   ) ado-variable-name-face t)
        "[ \t]*:[ \t]*"
	'(("all") ado-subcommand-face t)
	"[ \t]+"
        '((
	   "globals" "matrices" "scalars"
	   ) ado-subcommand-face t)
	"\\b"
        ))
     ;; all numeric/string
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
	   "gl" "glo" "glob" "globa" "global" 
	   "loc" "loca" "local" 
	   ) ado-builtin-harmless-face t)
	"[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
	   ) ado-variable-name-face t)
        "[ \t]*:[ \t]*"
	'(("all") ado-subcommand-face t)
	"[ \t]+"
	'((
	   "numeric" "string"
	   ) ado-subcommand-face t)
	"[ \t]+"
        '(("scalars") ado-subcommand-face t)
	"\\b"
        ))
    
    ;; choosing temp names
    (eval-when-compile
      (make-regexps
       "^[ \t]*"
       '(("tempname" "tempfile" "tempvar"
	  ) ado-builtin-harmless-face)
       '(("\\([ \t]+[a-zA-Z_]+[a-zA-Z_0-9`']*\\)+"
	  ) ado-variable-name-face t)
       ))
    ;; other macro commands
    (eval-when-compile
      (make-regexps
       "[ \t]*"
       '((
	  "ma" "mac" "macr" "macro"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "di" "dir"
	  "l" "li" "lis" "list"
	  )
	 ado-subcommand-face t)
       "\\b"
       ))
    (eval-when-compile
      (make-regexps
       "[ \t]*"
       '((
	  "ma" "mac" "macr" "macro"
	  ) ado-builtin-harmful-face)
       "[ \t]+"
       '((
	  "s" "sh" "shi" "shif" "shift"
	  )
	 ado-subcommand-face t)
       "\\b"
       ))

    ;; other macro commands
    (eval-when-compile
      (make-regexps
       "[ \t]*"
       '((
	 "sca" "scal" "scala" "scalar" 
	  ) ado-builtin-harmless-face t)
       "[ \t]+"
       '((
	  "di" "dir"
	  "l" "li" "lis" "list"
	  )
	 ado-subcommand-face t)
       "\\b"
       ))

    ;; stata functions i.e. things which require () after them 
    ;; obsolete functions are after this list
    ;; finally included matrix functions
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("_caller"
	  "abbrev" "abs" "acos" "asin" "atan" "atan2" "atanh" "autocode"
	  "Binomial"
	  "betaden" "binormal" "byteorder"
	  "c" "ceil" "char" "chi2" "chi2tail" "cholesky" "chop" "comb" "clip" "cloglog" 
	  "colnumb" "colsof" "cond" "corr" "cos" 
	  "d" "daily" "date" "day"
	  "det"
	  "dgammapda" "dgammapdada" "dgammapdadx" "dgammapdx" "dgammapdxdx"
	  "diag" "diag0cnt" "digamma" 
	  "dofd" "dofh" "dofm" "dofq" "dofw" "dofy" "dow" "doy"
	  "e" "el" "epsdouble" "epsfloat" "exp"
	  "F" "Fden" "Ftail" "float" "floor" 
	  "gammaden" "gammap" "get"
	  "h" "hadamard" "halfyear" "halfyearly" "has_eprop" "hofd"
	  "I" "ibeta" "indexnot" "inlist" "inrange" "int"
	  "inv" "invbinomial" "invchi2" "invchi2tail" "invcloglog" "invibeta" "invlogit" "invF" "invFtail" "invnFtail" 
	  "invgammap" 
	  "invnchi2" "invnibeta" "invnormal" "invsym" "invttail"
	  "irecode" "issymetric"
	  "J"
	  "length" "ln" "lnfactorial" "lngamma" "log" "log10" "logit" "lower" "ltrim" 
	  "m" "matmissing" "matrix" "matuniform" 
	  "max" "maxbyte" "maxdouble" "maxfloat" "maxint" "maxlong" 
	  "mdy" "mi"
	  "min" "minbyte" "mindouble" "minfloat" "minint" "minlong" 
	  "missing" "mod" "mofd" "month" "monthly" "mreldif"
	  "nbetaden" "nchi2" "normal" "normalden" "nFden" "nFtail" "nibeta" "npnchi2" "nullmat"
	  "plural" "proper"
	  "q" "qofd" "quarter" "quarterly"
	  "r" "real" "recode"
	  "regexm" "regexr" "regexs"
	  "reldif" "replay" "return" "reverse" "round" "rownumb" "rowsof" "rtrim" 
	  "s" "scalar" "sign" "sin" "sqrt" 
	  "string" "strlen" "strmatch" "strofreal" "strpos"
	  "subinstr" "subinword" "substr" "sum" 
	  "sweep" 
	  "tan" "tanh" "tden" "tin" "trace" "trigamma" "trim" "trunc" "ttail" "twithin"
	  "uniform" "upper"
	  "vec" "vecdiag"
	  "w" "week" "weekly" "wofd" "word" "wordcount"
	  "y" "year" "yearly" "yh" "ym" "yofd" "yq" "yw"
	  )
	 ado-function-name-face t)
       "("
       ))
    ;;
    ;; obsolete functions requiring () after them
     (eval-when-compile
       (make-regexps
        "\\b"
        '((
	   "binorm" 
	   "chiprob" 
	   "fprob"
	   "group"
	   "index"
	   "invchi" "invfprob" "invnchi" "invnorm" "invt" 
	   "issym"
	   "lnfact"
	   "match"
	   "nchi" "norm" "normd" "normden" "normprob" "npnchi" 
	   "syminv" "tprob" 
	   "uniform0"
	   )
	  ado-obsolete-face t)
        "("
        ))
    ;; stata 'functions' i.e. things which require [] after them
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("_b" "_coef" "_se")
	 ado-builtin-harmless-face t)
       "\\["
       ))
    ;; common Stata options which require a () after them
    (eval-when-compile
      (make-regexps
       "[, \t]+"
       '(("bands" "by" "connect" "density" "gap" "iterate" "ltolerance" "margin"
	  "psize" "saving" "tlabel" "tolerance"
	  "xlabel" "xscale" "ylabel" "yscale")
	 ado-subcommand-face t)
       "("
       ))
    ;; egen 'function' options
    (eval-when-compile
      (make-regexps
       "[ \t]*egen[ \t]+.*=[ \t]*"
       '((
	  "concat" "count" "cut" 
	  "diff"
	  "ends" 
	  "fill" "group" "iqr"
	  "kurt"
	  "ma" "mad" "max" "mdev" "mean" "median" "min" "mode" "mtr" 
	  "pc" "pctile" 
	  "rank" 
	  "rowfirst" "rowlast" "rowmax" "rowmean" "rowmin" "rowmiss" "rownonmiss" "rowsd" "rowtotal" 
	  "sd" "seq" "skew" "std" 
	  "tag" "total"
	  )
	 ado-function-name-face t)
       "(.*?)"
       ))
    ;; egen 'function' options -- obsolete
    (eval-when-compile
      (make-regexps
       "[ \t]*egen[ \t]+.*=[ \t]*"
       '((
	  "any"
	  "eqany"
	  "neqany"
	  "rfirst" "rlast" "rmax" "rmean" "rmin" "rmiss" "robs" "rsd" "rsum" 
	  "sum"
	  )
	 ado-obsolete-face t)
       "(.*?)"
       ))

    ;; All Custom ado files which are 'reliable' and which are not file killers
    ;; this might be a useless endeavor --- but I cannot generate tag files
    ;; all the s-extensions are listed under Stata's name (since they alter
    ;; data and will be moved tot he utils directory
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("anypath" "autolab" "checkvar" "ck1icd9" "ckicd9"
	  "dtapath" "dupclean" "echo"
	  "getdate" "getlbl" "getnames" "getobs"
	  "icd9x" "issorted" "isfile" "jultoe" "jultof" "jultomdy" "knowndup"
	  "labeldir" "linker" 
	  "markit" "makewide" "missize" "mpcounts" 
	  "nodups" "notefile" "prov2zip"
	  "qcolsum" "qorder" 
	  "random" "readraw" "readzip" "repart"
	  "setup" "stdrate"
	  "timeslot"
	  "_addext" "_brclean" "_brckado" "_brdlog"
	  "_ckbad" "_ckdunno" "_ckdupl" "_ckmiss" "_ckok" "_ckwarn"
	  "_delimit" "_filenm" "_lookup" "_mk_ck"
	  ) ado-site-harmless-face)
       "\\b"
       ))


    ;;
    ;; now, presenting smcl
    ;;
    ;; Syntax 1
    ;; comment region 23
    (eval-when-compile
      (make-regexps
       '(("{") ado-constant-face)
       "[ \t]*"
       '((
	  "\.\.\."
	  "\.-"
	  "asis"
 	  "bf" "break"
 	  "cmd"
	  "depvar" "depvars" "depvarlist" "dtype"
 	  "err" "error"
 	  "hi" "hilite" "hline"
 	  "ifin" "imp" "indepvars" "input" "it"
	  "newvar"
	  "p" "p_end" "p2colreset" "p2line" 
	  "phang" "phang2" "phang3" "pin" "pin2" "pin3" "pmore" "pmore2" "pmore3" "psee" "pstd"
 	  "res" "reset" "result"
	  "s6hlp"
 	  "sf" "synopthdr" "synoptline" "synoptset"
 	  "tab" "text" "txt"
	  "var" "varlist" "varname" "vars"
	  "weight"
 	  ) ado-builtin-harmless-face t)
       "[ \t]*?"
        '(("}") ado-constant-face)
        ))

    ;; Syntax 2
    (eval-when-compile
      (make-regexps
       '(("{") ado-constant-face)
       "[ \t]*"
       '((
	  "ado_d"
 	  "back" "bf" "bind" 
 	  "center" "centre" "clearmore" "cmd"
	  "depvar" "depvars" "depvarlist" "dlgtab"
 	  "err" "error"
 	  "help_d" "hi" "hilite"
 	  "indepvars" "inp" "input" "it"
	  "net_d" "netfrom_d" "news" "newvar"
	  "p2col" "p2coldent"
 	  "rcenter" "rcentre" "res" "reset" "result" "right"
	  "search_d"
 	  "sf" "synopt" "synopthdr" "syntab"
 	  "text" "title" "txt"
	  "ul" "update_d"
	  "var" "varlist" "varname" "vars" "view_d"
 	  ) ado-builtin-harmless-face t)
       "[ \t]*?"
       '((":") ado-constant-face)
       '(("[^}]+") ado-subcommand-face t)
       '(("}") ado-constant-face)
       ))
    ;; making comments look like comments
    (eval-when-compile
      (make-regexps
       '(("{") ado-constant-face)
       "[ \t]*"
       '(("\\*") ado-builtin-harmless-face t)
       "[ \t]*?"
       '((":") ado-constant-face)
       '(("[^}]+") ado-comment-face t)
       '(("}") ado-constant-face)
       ))

    ;; Syntax 2plus for cmdab
    (eval-when-compile
      (make-regexps
       '(("{") ado-constant-face)
       "[ \t]*"
       '(("cmdab") ado-builtin-harmless-face)
       '((":") ado-constant-face)
       '(("[a-zA-Z][a-zA_Z_0-9]*") ado-subcommand-face)
       '((":") ado-constant-face)
       '(("[^}]*?") ado-subcommand-face)
       '(("}") ado-constant-face)
       ))

    ;; Syntax 3 with free form args
    (eval-when-compile
      (make-regexps
       '(("{") ado-constant-face)
       "[ \t]*"
       '((
	  "ado"
	  "browse"
	  "c" "char" "col"
	  "dialog"
	  "help" "helpb"
	  "manhelp" "manhelpi" "matacmd"
	  "net"
	  "opt"
	  "p" "p2line" "p2colset" 
	  "search" "space" "stata" "synoptset"
	  "update"
	  "ul"
	  "view"
	  ) ado-builtin-harmless-face t)
       "[ \t]+"
       '((".*?") ado-subcommand-face t)
       '(("}") ado-constant-face)
       ))
    ;; Syntax 3 comments
    (eval-when-compile
      (make-regexps
       '(("{") ado-constant-face)
       "[ \t]*"
       '(("\\*") ado-builtin-harmless-face t)
       '((".*?") ado-comment-face)
       '(("}") ado-constant-face)
       ))
    ;; Syntax 3 with single numerical args
    (eval-when-compile
      (make-regexps
       '(("{") ado-constant-face)
       "[ \t]*"
       '((
	  "hline"
	  ) ado-builtin-harmless-face t)
       "[ \t]+"
       '(("[0-9]+") ado-subcommand-face t)
       "[ \t]*"
       '(("}") ado-constant-face)
       ))
       
    ;; Syntax 4
    ;; those whose subcommands are not easy
    ;;  on second thought: for all!
    (eval-when-compile
      (make-regexps
       '(("{") ado-constant-face)
       "[ \t]*"
       '((
	  "ado"
	  "browse"
	  "center" "centre"
	  "dialog" "dlgtab" "dup"
	  "help" "helpb"
	  "lalign"
	  "manhelp" "manhelpi" "matacmd"
	  "net"
	  "opt"
	  "p2col"
	  "ralign" "rcenter" "rcentre"
	  "search" "stata"
	  "update"
	  "view"
	  ) ado-builtin-harmless-face prepend)
       "[ \t]+"
       '((".*?") ado-subcommand-face)
       '((":") ado-constant-face t)
       '(("[^}]+?") ado-subcommand-face t)
       '(("}") ado-constant-face)
       ))

    
    ;; class stuff
    ;;; builtin prefix operators
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("\\.") ado-function-name-face)
       '(("Global" "Local" "Super") ado-function-name-face t)
       '(("\\.") ado-function-name-face t)
       ))
    ;;; builtin class functions and modifiers
    (eval-when-compile
      (make-regexps
       "\\>"
       '(("\\.") ado-function-name-face)
       '((
	  "Arrdropall" "Arrdropel" "Arrpop" "Arrpush"
	  "Declare"
	  "arrindexof" "arrnels"
	  "classmv" "classname" "copy"
	  "dynamicmv"
	  "instancemv" "isa" "isofclass"
	  "new"
	  "objkey" "objtype"
	  "ref" "ref_n"
	  "superclass"
	  "uname" 
	  ) ado-function-name-face t)
       "\\b"
       ))
    ;;; class command
     (eval-when-compile
       (make-regexps
        "\\b"
        '(("class") ado-builtin-harmless-face)
        "[ \t]+"
        '((
 	  "exit"
 	  ) ado-subcommand-face t)
        "\\b"
        ))
     ;; highlighting class names
     (eval-when-compile
       (make-regexps
	"\\b"
	'(("class") ado-builtin-harmful-face)
	"[ \t]+"
	'(("[_a-zA-Z][_a-zA-Z0-]*") ado-builtin-harmful-face t)
	))
     ;; all the different declarations
     (eval-when-compile
	(make-regexps
	 "\\b"
	 '((
	    "class" "classw" "classwi" "classwid" "classwide" 
	    "instance" "instances" "instancesp" "instancespe" "instancespec" 
	    "instancespeci" "instancespecif" "instancespecifi" "instancespecific" 
	    ) ado-builtin-harmless-face)
	 "[ \t]*:"
	 ))
    
    ;; classutil stuff
    (eval-when-compile
       (make-regexps
        "\\b"
        '(("classutil") ado-builtin-harmful-face)
        "[ \t]+"
        '((
 	  "drop"
 	  ) ado-subcommand-face t)
        "\\b"
        ))
    (eval-when-compile
       (make-regexps
        "\\b"
        '(("classutil") ado-builtin-harmless-face)
        "[ \t]+"
        '((
	   "cdir"
	   "d" "de" "des" "desc" "descr" "descri" "describ" "describe"
	   "dir"
	   "which"
 	  ) ado-subcommand-face t)
        "\\b"
        ))

    ;; oh my - the creturn info!
    (eval-when-compile
       (make-regexps
        "\\b"
        '(("cret" "cretu" "cretur" "creturn") ado-builtin-harmless-face)
        "\\W+"
        '((
 	  "l" "li" "lis" "list" 
 	  ) ado-subcommand-face)
        "\\b"
        ))

    ;;; the system 'constants' (which are not really constant) - the c() thingies
    (eval-when-compile
       (make-regexps
        "\\b"
        '(("c(") ado-builtin-harmless-face t)
        "[ \t]*"
        '((
	   "ALPHA" "Mons" "Months" "N" "SE" "Wdays" "Weekdays"
	   "adopath" "adosize" "alpha" 
	   "born_date" "byteorder"
	   "changed" "checksum" "console" "copycolor" "current_time" "current_date"
	   "dirsep" "dp"
	   "eolchar" "epsdouble" "epsfloat"
	   "filedate" "filename" "flavor"
	   "graphic" 
	   "httpproxy" "httpproxyauth" "httpproxyhost" "httpproxyport" "httpproxypw" "httpproxyuser"
	   "icmap"
	   "k"
	   "level" "linegap" "linesize" "logtype"
	   "machine_type" 
	   "matacache" "matafavor" "matalibs" "matalnum" "matamofirst" "mataoptimize" "matastrict"
	   "matsize" 
	   "max_N_theory" "max_cmdlen" "max_k_theory" "max_macrolen" "max_matsize" "max_width_theory" 
	   "maxbyte" "maxdb" "maxdouble" "maxfloat" "maxint" "maxiter" "maxlong" "maxstrvarlen" "maxvar" 
	   "memory"
	   "minbyte" "mindouble" "minfloat" "minint" "minlong"
	   "mode" "more"
	   "namelen"
	   "os" "osdtl"
	   "pagesize" "pi" "printcolor" "pwd"
	   "rc" "rmsg" "rmsg_time"
	   "scheme" "scrollbufsize" "searchdefault" "seed" "stata_version"
	   "sysdir_base" "sysdir_oldplace" "sysdir_personal" "sysdir_plus" "sysdir_site" "sysdir_stata"
	   "sysdir_updates" "timeout1" "timeout2" 
	   "trace" "tracedepth" "traceexpand" "tracehilite" "traceindent" "tracenumber" "tracesep" "type"
	   "varlabelpos" "varabbrev" "version" "virtual"
	   "width" 
	   ) ado-constant-face t)
	"[ \t]*"
	'((")") ado-builtin-harmless-face t)
        ))
    ;;; platform specific c() thingies
    (eval-when-compile
       (make-regexps
        "\\b"
        '(("c(") ado-builtin-harmless-face t)
        "[ \t]*"
        '((
	   "dockable" "dockingguides"
	   "fastscroll"
	   "locksplitters"
	   "macgphengine"
	   "persistfv" "persistvtopic" "piccomments"
	   "reventries" "revwindow"
	   "smalldlg" "smoothfonts" "smoothsize"
	   "varwindow"
	   "xptheme"
	   ) ado-platform-specific-face t)
	"[ \t]*"
	'((")") ado-builtin-harmless-face t)
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stuff for writing dlg files  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    (eval-when-compile
      (make-regexps
       "\\b"
       '(("VERSION") ado-builtin-harmless-face)
       "\\W+"
       '((
	  "8\\([.]\\(0\\|1\\|2\\)\\)?" "9\\([.]\\(0\\|00\\|1\\|2\\)\\)?"
	  ) ado-subcommand-face)
       "\\b"
       ))
    
;; general builtins for dialogs
;; here - the harmless faces define static text 
;;        whereas the harmful face defines dynamic text

    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "BUTTON"
	  "CANCEL" "CHECKBOX" "COMBOBOX"
	  "DEFINE" "DIALOG"
	  "EDIT" 
	  "FILE"
	  "HELP"
	  "INCLUDE"
	  "LISTBOX"
	  "OK"
	  "RADIO" "RESET"
	  "SPINNER" "SUBMIT"
	  "VARLIST" "VARNAME"
	  "stopbox[ \t]+note"
	  "stopbox[ \t]+rusure"
	  "stopbox[ \t]+stop"
	  ) ado-builtin-harmful-face)
       "\\b"
       ))
    
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "BEGIN"
	  "COLOR"
	  "END" "EXP"
	  "FRAME"
	  "GROUPBOX"
	  "LIST"
	  "POSITION" "PROGRAM"
	  "SCRIPT"
	  "TEXT"
	  "allowxi"
	  "beginoptions" "by" "bysort"
	  "endoptions" "exit"
	  "ifexp" "inrange"
	  "option" "optionarg"
	  "put"
	  "require"
	  "stata"
	  "varlist"
	  "weight"
	  ) ado-builtin-harmless-face)
       "\\b"
       ))

    ;; removed 	  "\\."
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "action"
	  "gaction"
	  "script"
	  "view"
	  "program"
	  ) ado-function-name-face)
       "\\b"
       ))

    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "call" ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "\\."
	  "action"
	  "gaction"
	  "script"
	  "view"
	  "program"
	  ) ado-subcommand-face t)
       "\\b"
       ))
    ;; stata functions i.e. things which require () after them 
    ;; obsolete functions are after this list
    ;; finally included matrix functions
    (eval-when-compile
      (make-regexps
       "\\>"
       '(("\\.") ado-function-name-face)
       '((
	  "contains"
	  "endswith"
	  "isdefault" "isenabled" "iseq" "iseqignorecase" "isge" "isgt" "isle" "islt" "isneq" "isvisible"
	  )
	 ado-function-name-face t)
       "("
       ))

    ;; mata keywords --- won't override, because they are only active in a mata block...
    ;;  and mata block checking has not been implemented
    (eval-when-compile
      (make-regexps
       "[ \t]+"
       '((
	  "break"
	  "colvector" "complex" "continue"
	  "do"
	  "external"
	  "for" "function"
	  "goto"
	  "if" 
	  "matrix"
	  "numeric"
	  "pointer" "pragma" 
	  "real" "return" "rowvector"
	  "scalar" "string" 
	  "transmorphic" 
	  "vector" "version" "void"
	  "while"
	  ) ado-mata-keyword-face)
       "\\b"
       ))

    (eval-when-compile
      (make-regexps
       "[ \t]+"
       '((
	  "aggregate" "array"
	  "boolean" "byte" 
	  "case" "catch" "class" "const"
	  "default" "delegate" "delete" "double" 
	  "else" "eltypedef" "end" "enum" "explicit" "export"
	  "float" "friend" 
	  "global"
	  "inline" "int" 
	  "local" "long" 
	  "namespace" "new" "NULL" 
	  "operator" "orgtypedef"
	  "polymorphic" "private" "protected" "public"
	  "quad"
	  "short" "signed" "sizeof" "static" "struct" "super" "switch"
	  "template" "this" "throw" "try" "typedef" "typename"
	  "union" "unsigned" "using" 
	  "virtual" "volatile"
	  ) ado-mata-future-keyword-face)
       "\\b"
       ))

    (eval-when-compile
      (make-regexps
       "[ \t]+"
       '(("pragma") ado-mata-keyword-face)
       "[ \t]+"
       '(("unset" "unused") ado-subcommand-face)
       ))
    
    (eval-when-compile
      (make-regexps
       "[ \t]+"
       '(("pointer" "return") ado-mata-keyword-face)
       "("
       ))

    ;; mata subcommands
    ;;  does this run into the need for extra harmful and harmless faces?!?!
    (eval-when-compile
      (make-regexps
       "[ \t]+"
       '(("mata") ado-mata-keyword-face t)
       "[ \t]+"
       '((
	  "clear"
	  "d" "de" "des" "desc" "descr" "descri" "describ" "describe" 
	  "drop"
	  "end"
	  "help"
	  "matd" "matde" "matdes" "matdesc" "matdescr" "matdescri" "matdescrib" "matdescribe" 
	  "matsave" "matuse" "memory" "mlib" "mosave"
	  "query"
	  "rename"
	  "set" "stata"
	  "which"
	  ) ado-mata-keyword-face t)
       "\\b"
       ))
    ;; mata subcommands
    ;;  does this run into the need for extra harmful and harmless faces?!?!
    (eval-when-compile
      (make-regexps
       "[ \t]+"
       '(("mata") ado-mata-keyword-face t)
       "[ \t]+"
       '(("mlib") ado-mata-keyword-face t)
       "[ \t]+"
       '((
	  "add"
	  "create"
	  "index"
	  "q" "qu" "que" "quer" "query"
	  ) ado-subcommand-face t)
       "\\b"
       ))

    ;; general mata set commands
    (eval-when-compile
      (make-regexps
       "[ \t]+"
       '(("mata") ado-mata-keyword-face t)
       "[ \t]+"
       '(("set") ado-mata-keyword-face t)
       "[ \t]+"
       '((
	  "matacache" "matalibs"
	  ) ado-subcommand-face t)
       "\\b"
       ))

    ;; general mata set on/off commands
    (eval-when-compile
      (make-regexps
       "[ \t]+"
       '(("mata") ado-mata-keyword-face t)
       "[ \t]+"
       '(("set") ado-mata-keyword-face t)
       "[ \t]+"
       '((
	  "matamofirst" "matalnum" "mataoptimize" "matastrict"
	  ) ado-subcommand-face t)
       "[ \t]+"
       '(("off" "on") ado-subcommand-face t)
       ))

    ;; specific mata set commands
    (eval-when-compile
      (make-regexps
       "[ \t]+"
       '(("mata") ado-mata-keyword-face t)
       "[ \t]+"
       '(("set") ado-mata-keyword-face t)
       "[ \t]+"
       '((
	  "matafavor"
	  ) ado-subcommand-face t)
       "[ \t]+"
       '(("space" "speed") ado-subcommand-face t)
       ))

    ;; mata functions (durn, this is a pain in the butt)
    ;; functions which exist for regular stata are NOT included
    ;; these, too ended up being split
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "_chdir" "_cholesky" "_cholinv" "_cholsolve" "_collate" "_conj" "_corr" 
	  "_editmissing" "_edittoint" "_edittointtol" "_edittozero" "_edittozerotol" "_editvalue"
	  "_eigensystem" "_eigenvalues"
	  "_equilc" "_equilr" "_equilrc" "_error" 
	  "_fft" "_fillmissing" 
	  "_fclose" "_fget" "_fgetmatrix" "_fgetnl" "_fopen" "_fput" "_fputmatrix" "_fread" 
	  "_fseek" "_ftell" "_ftruncate" "_fullsvd" "_fwrite"
	  "_halton" "_hqrd" "_hqrdp" "_hqrdp_la"
	  "_invfft" "_invsym"
	  "_jumble"
	  "_lefteigensystem" "_lowertriangle" "_lud" "_lud_la" "_luinv" "_luinv_la" "_lusolve" "_lusolve_la"
	  "_makesymmetric" "_matexpsym" "_matlogsym" "_matpowersym" "_mkdir" 
	  "_perhapsequilc" "_perhapsequilr" "_perhapsequilrc" "_pinv"
	  "_qrinv" "_qrsolve"
	  "_rmdir" 
	  "_solvelower" "_solveupper" "_sort" 
	  "_st_addobs" "_st_addvar" "_st_data" "_st_macroexpand" "_st_sdata" "_st_sstore" "_st_store"
	  "_st_varindex"
	  "_stata" "_svd" "_svd_la" "_svdsv" "_svsolve" "_symeigensystem" "_symeigenvalues"
	  "_transpose" "_transposeonly"
	  "_unlink" "_uppertriangle" 
	  ) ado-mata-function-name-face t)
       "("
       ))

    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "C" "Corr" "Hilbert" "Im" "Re" "Toeplitz" "Vandermonde"
	  "adosubdir" "all" "allof" "any" "anyof" "args" "ascii" "assert" "asserteq"
	  "blockdiag" "breakkey" "breakkeyreset"
	  "callersversion" "cat" "chdir" "cholsolve" "cholinv" 
	  "colmax" "colmaxabs" "colmin" "colminmax" "colmissing" "colnonmissing" "cols" "colscalefactors" "colshape" "colsum" 
	  "conj" "convolve" "correlation" "crexternal" "cross" "crossdev"
	  "designmatrix" "dettriangular"
	  "diag" "diagonal" "dir" "direxists" "direxternal" "display" "displayas" "displayflush" "dsign"
	  "editmissing" "edittoint" "edittointtol" "edittozero" "edittozerotol" "editvalue" 
	  "eigensystem" "eigenvalues" 
	  "eltype" "epsilon" "error" "errprintf" "exit"
	  "factorial" "favorspeed" "fclose" "fft""fileexists" "findfile" 
	  "fget" "fgetnl" "fgetmatrix" "findexternal" "fopen" "fput" "fputmatrix" 
	  "fread" "fseek" "fstatus" 
	  "ftell" "ftfreqs" "ftpad" "ftperiodogram" "ftretime" "ftruncate" "ftunwrap" "ftwrap" 
	  "fullsdiag" "fullsvd" "fwrite"
	  "gamma" "ghalton" "ghk"
	  "halton" "hqrd" "hqrdp" "hqrdmultq" "hqrdmultqlt" "hqrdq" "hqrdq1" "hqrdr" "hqrdr1"
	  "invHilbert" "invfft" "invorder" "invvech"
	  "iscomplex" "isdiagonal" "isfleeting" "ispointer" "isreal" 
	  "isrealvalues" "isstring" "issymmetric" "issymmetriconly" "isview" 
	  "jumble"
	  ) ado-mata-function-name-face t)
       "("
       ))

    (eval-when-compile
      (make-regexps
       "\\b"
       '((
 	  "lefteigensystem" "lnnormal" "lnnormalden" "lowertriangle" "lud" "luinv" "lusolve"
	  "makesymmetric" "matexpsym" "matlogsym" "matpowersym" "mean" "meanvariance" "minmax" "missingof" "more"
	  "mreldifre" "mreldifsym"
	  "nonmissing" "norm"
	  "order" "orgtype"
	  "panelsetup" "panelstats" "panelsubmatrix" "panelsubview"
	  "pathasciisuffix" "pathbasename" "pathisabs" "pathisurl" "pathjoin" "pathlist" 
	  "pathrmsuffix" "pathsearchlist" "pathsplit" "pathstatasuffix" "pathsubsysdir" "pathsuffix"
	  "pinv"
	  "polyadd" "polyderiv" "polydiv" "polyeval" "polyinteg" "polymult" "polyroots" "polysolve" "polytrim"
	  "printf"
	  "range" "rangen" "rmexternal" "rowmax" "rowmissing" "rowscalefactors"
	  "qrd" "qrdp" "qrinv" "qrsolve" 
	  "quadcorrelation" "quadcross" "quadcrossdev" "quadrant" "quadcolsum" "quadrowsum" "quadsum" "quadvariance" 
	  "querybreakintr"
	  "rank" "revorder" "rowmaxabs" "rowmin" "rowminmax" "rownonmissing" "rows" "rowshape" "rowsum"
	  "setbreakintr" "sizeof" "smallestdouble"
	  "solve_tol" "solvelower" "solveupper"
	  "sort" "spline3" "spline3val" "stata" "strdup" "svd" "svdsv" "svsolve" "swap"
	  "symeigensystem" "symeigenvalues"
	  "tokens" "transposeonly"
	  "uniqrows" "unitcircle" "uniformseed"
	  "valofexternal" "variance" "vec" "vech"
	  "unlink" "uppertriangle"
	  ) ado-mata-function-name-face t)
       "("
       ))
    ;; arrrgh the mata st_ commands
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("st_") ado-mata-function-name-face t)
       '((
	  "addobs" "addvar" 
	  "data" "dir" "dropobsif" "dropobsin" "dropvar"
	  "eclear"
	  "global"
	  "isfmt" "islmname" "isname" "isnumfmt" "isnumvar" "isstrfmt" "isstrvar"
	  "keepobsif" "keepobsin" "keepvar"
	  "local"
	  "macroexpand" "matrix" "matrixcolstripe" "matrixrowstripe"
	  "nobs" "numscalar" "nvar"
	  "rclear" "replacematrix"
	  "sclear" "sdata" "sstore" "store" "strscalar" "subview" "sview"
	  "tempfilename" "tempname" "tsrevar" 
	  "update"
	  "varformat" "varindex" "varlabel" "varname" "varrename" "vartype" "varvaluelabel"
	  "view" "viewobs" "viewvars"
	  "vldrop" "vlexists" "vlload" "vlmap" "vlmodify" "vlsearch"
	  ) ado-mata-function-name-face t)
       "("
       ))

;; all variable/macro stuff (put late so it will override)
;; keep just before the obsolete commands!
    ;; internal constants
    (eval-when-compile
      (make-regexps
       "[^a-zA-Z]"
       '(("_merge" "_n" "_pi" "_rc" "_N"
	  ) ado-constant-face t)
       "[^a-zA-Z]"
       ))
    ;; some generated vars
    ;; ... which are now out of date
    (eval-when-compile
      (make-regexps
       '(("_result([1-9]+[0-9]*)"
	  ) ado-obsolete-face t)
       ))
    ;; global macros
    (eval-when-compile
      (make-regexps
       '(("\\$[a-zA-Z_*]+[a-zA-Z_0-9]*"
	  ) ado-variable-name-face t)
       ))
    ;; local macros
    ;;   highlights *before* the macro is ended, which 
    ;;   could lead to typos, but gets rid of recursive
    ;;   definitions.
    (eval-when-compile
      (make-regexps
       "`+"
       '(("[a-zA-Z_0-9]+"	
	  ) ado-variable-name-face t)
       ;"'+"
       ))
    (eval-when-compile
      (make-regexps
       "`+"
       '(("\\(e\\|r\\|s\\)") ado-function-name-face t)
       '(("(") ado-constant-face t)
       '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
	  ) ado-variable-name-face t)
       '((")") ado-constant-face t)
       "'+"
       ))

    ;; obsolete mfp arguments
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("mfp") ado-builtin-harmless-face t)
       "[ \t]"
       '(( 
	  "ologit" "oprobit"
	  ) ado-obsolete-face t) 
       "\\b"
       ))

    ;; what few obsolete commands I've gathered
    (eval-when-compile
      (make-regexps
       "[ \t]*"
       '((
	  "archlm"
	  "bgodfrey"
	  "durbina" "dwstat"
	  "lfit"
	  "hettest"
	  "imtest"
 	  "loo" "look" "looku" "lookup"
	  "nlinit"
	  "ovtest"
	  "lstat"
	  "poisgof"
;	  "sco" "scor" "score" 
	  "stphtest" "svylc" "svytest" "szroeter"
	  "varfcast" "varirf" "vce" "vif"
	  "xtcorr" "xtrchh" "xthaus"
	  "shelltool"
	  ) ado-obsolete-face t)
       "\\b"
       ))

    ;; the estat commands
    (eval-when-compile
      (make-regexps
       "\\b"
       '(("estat") ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "alternatives" "anti" "archlm" 
	  "bgodfrey" "bootstrap"
	  "clas" "common" "compare" "con" "config" "coordinates" "correlation" "correlations" "covariance" 
	  "distances" "durbinalt" "dwatson"
	  "eff" "effe" "effec" "effect" "effects" 
	  "factors"
	  "gof" "group"
	  "hettest"
	  "ic" "imtest" "inertia"
	  "kmo"
	  "lceff" "lceffe" "lceffec" "lceffect" "lceffects" 
	  "loadings"
	  "mvreg"
	  "ovtest"
	  "pairwise" "phtest" "profiles"
	  "quantiles"
	  "recovariance" "residuals" "rotatecompare"
	  "size" "smc" "stress" "structure"
	  "su" "sum" "summ" "summa" "summar" "summari" "summariz" "summarize" 
	  "svyset"
	  "szroeter"
	  "tables"
	  "vce" "vif"
	  "wcorrelation"
	  ) ado-subcommand-face t)
       "\\b"
       ))


    ;; things which are partially obsolete
    (eval-when-compile
      (make-regexps
       "^[ \t]*"
       '((
	  "jknife"
 	  "parse"
	  "whelp"
	  ) ado-obsolete-face t)
       "\\b"
       ))
    
    ;; apparently obsolete window commands
    (eval-when-compile
      (make-regexps
       "\\b"
       '((
	  "win" "wind" "windo" "window"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "m" "me" "men" "menu"
	  ) ado-builtin-harmless-face)
       "[ \t]+"
       '((
	  "append[ \t]+popout"
	  "append[ \t]+string"
	  "popout"
	  "set"
	  ) 
	 ado-obsolete-face)
       "\\b"
       ))

    ;; semi-obsolete macro commands
    (eval-when-compile
      (make-regexps
       "[ \t]*"
       '((
	  "ma" "mac" "macr" "macro"
	 "sca" "scal" "scala" "scalar" 
	  ) ado-builtin-harmful-face)
       "[ \t]+"
       '((
	  "de" "def" "define" 
	  )
	 ado-subcommand-face t)
       "\\b"
       ))
         ;; multiword extended macro names using 'set', which are obsolete
     (eval-when-compile
       (make-regexps
        "[ \t]*"
        '((
	   "gl" "glo" "glob" "globa" "global" 
	   "loc" "loca" "local" 
 	  ) ado-builtin-harmless-face t)
	"[ \t]+"
        '(("[a-zA-Z_]+[a-zA-Z_0-9]*"
 	  ) ado-variable-name-face t)
        "[ \t]*:[ \t]*"
        '(("set") ado-obsolete-face t)
	"[ \t]+"
	'((
 	  "adosize" "graphics" "level" "linesize" "logtype" "matsize" "more" "pagesize"
 	  "rmsg" "trace" "type" "virtual" 
 	  ) ado-obsolete-face t)
	"\\b"
        ))



   )))

;;; now for fancy stuff: syntactic keywords
(defun ado-set-font-lock-syntactic-keywords ()
  (interactive)
  (setq font-lock-syntactic-keywords
	(list
	 '("\\(#\\)[dr]" 1 "w")
	)))



;;; Working with help files
;;; allowing for the user's name to be put into help files
(defun set-ado-signature-file ()
  (interactive)
  (setq ado-signature-file
	(read-file-name "Set ado signature file to: "
			(file-name-directory (expand-file-name
					      (if (not ado-signature-file)
						  (if (file-exists-p "~/.ado-signature")
						      "~/.ado-signature")
						ado-signature-file)))))
  )
(defun ado-help-file-6 ()
  "Gets a boilerplate for writing help files for Stata 6 and earlier, then inserts the users name and allows editing. This is not complete, since it has trouble putting the proper signature at the bottom of the file. To make this work properly, have a variable which contains the name of the signature file or a .signature file."
  (interactive)
  (let (name)
    (setq name (read-from-minibuffer "For what program should help be written? "))
    (switch-to-buffer
     (generate-new-buffer
      (generate-new-buffer-name (concat name ".hlp"))))
    (ado-insert-boilerplate "help6.blp" t)
    (if (and ado-new-dir (y-or-n-p "Put in 'new' directory? "))
	(cd ado-new-dir))
    (if (not ado-claim-name)
	(setq ado-claim-name 
	      (read-from-minibuffer "Whose name should be put at the top of the file? "
				    user-full-name)))
    (text-mode)
    (goto-char (point-max))
    (if ado-signature-prompt-flag
	(progn
	  (if (not ado-signature-file)
	      (set-ado-signature-file))
	  (insert-file-contents ado-signature-file))
      (insert ado-claim-name))
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (insert (concat "^" name "^"))
    (insert (concat
	     (make-string
	      (- 81 (+ (length ado-claim-name) (current-column)))
	      (string-to-char " "))
	     ado-claim-name))
    (search-forward "Put a")
    (beginning-of-line)
    (set-fill-column 79)
    (auto-fill-mode 0)
    (recenter)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stuff for writing help files ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ado-help-file ()
  "Gets a boilerplate for writing help files, inserts the users name and allows editing. This is not complete, since it has trouble putting the proper signature at the bottom of the file. To make this work properly, have a variable which contains the name of the signature file or a .signature file."
  (interactive)
  (let (name)
    (setq name (read-from-minibuffer "For what program should help be written? "))
    (switch-to-buffer
     (generate-new-buffer
      (generate-new-buffer-name (concat name ".hlp"))))
    (ado-insert-boilerplate "help.blp" t)
    (if (and ado-new-dir (y-or-n-p "Put in 'new' directory? "))
	(cd ado-new-dir))
    (if (not ado-claim-name)
	(setq ado-claim-name 
	      (read-from-minibuffer "Whose name should be put at the top of the file? "
				    user-full-name)))
    (text-mode)
    (goto-char (point-max))
    (if ado-signature-prompt-flag
	(progn
	  (if (not ado-signature-file)
	      (set-ado-signature-file))
	  (insert "
{title:Author}
")
	  (insert-file-contents ado-signature-file))
      (insert ado-claim-name))
    (goto-char (point-max))
    (insert (concat "
{title:Last Updated}: " (ado-nice-current-date)
""))
    (goto-char (point-min))
    ;; hard code stupidity which should be changed
    (forward-line 2)
    (end-of-line)
    (insert (concat "{hi:" name "}{right: " ado-claim-name "}"))
    (search-forward "hi:")
    (recenter)
    (ado-mode)
    ))

;;; useful insertions in smcl
(defun ado-help-insert-option-in-body (&optional option-name)
  (interactive)
  (if (not option-name)
      (setq option-name
	    (read-from-minibuffer "What is the full name of the option? " option-name)))
  (ado-insert-with-lfd (concat "{p 0 4}{cmd:" option-name "}"))
  (ado-insert-with-lfd "{p_end}")
  (newline)
  (forward-line -2)
  (end-of-line)
  (forward-char -1)
)

;;; Some utilities which should really be in a separate file (but which
;;;  would then cause extra installation instructions).

(defun ado-reset-value (value prompt &optional flag)
  (let (new-value value)
    (setq new-value (read-from-minibuffer (format (concat "Change " prompt " from %d to ") value)))
    (if (null new-value)
	value
      (if (= (setq new-value (string-to-number new-value )) value)
	  (progn 
	    (message (concat prompt " left unchanged."))
	    value)
	new-value
	)
      )))

;; (defun uncomment-region ()
;;   "Uncomments a region where each line is enclosed as a comment. Care
;;   must be taken that all lines are commented - otherwise odd behavior
;;   can result. Truly used to undo a previous comment-region."
;;   (interactive) 
;;   (comment-region (region-beginning) (region-end) -1))

(defun ado-insert-with-lfd (junk)
  "Used to insert and indent without needed to hit the indentation key
  (usually a tab). One day, when the ado-mode is complete, the other
  functions will no longer depend on this function."
  (insert junk)
  (newline-and-indent))

(defvar calendar-month-name-array
  ["January" "February" "March"     "April"   "May"      "June"
   "July"    "August"   "September" "October" "November" "December"])
;; scunged from the calendar.el file and then changed

(defun ado-nice-current-date ()
  "Returns the current date in a nice format: month date, year @
  time. Called within ado-mode to timestamp files. Since 'nice' is
  subjective, this can be changed at whim."
  (interactive)
  (let 
      ((s (current-time-string))) 
    (concat 
     (aref calendar-month-name-array 
	   (1-(length (member (substring s 4 7) 
			      '("Dec" "Nov" "Oct" "Sep"
				"Aug" "Jul" "Jun" "May"
				"Apr" "Mar" "Feb" "Jan"))))) " " 
				(let ((dd
				       (string-to-number (substring s 8 10)))) 
				  (if (< dd 10) (substring s 9 10) 
				    (substring s 8 10))) ", " 
				    (substring s 20 24) " @ " 
				    (substring s 11 19))) 
  )

(defun ado-set-imenu-items ()
  (interactive)
  (setq imenu-case-fold-search nil)
  (setq imenu-generic-expression
	(list 
	 (list nil "^\\s-*pr\\(o\\|og\\|ogr\\|ogra\\|ogram\\)\\(\\s-+\\(de\\|def\\|defi\\|defin\\|define\\)?\\)\\s-+\\([a-zA-Z_][a-zA-Z_0-9]*\\)" 4))))

(provide 'ado-mode)
;; ado-mode.el ends here
