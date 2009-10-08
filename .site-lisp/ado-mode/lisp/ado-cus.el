;;; ado-mode--cus.el --- customization things in ado mode
;; Copyright (c) 2003
;; Bill Rising
;;   much of this sponged from the ultex-cus.el which customizes
;;   the UltraTex mode (highly recommended) by
;; Mark Haiman, Nick Reingold, John Palmieri

;; Author:   Bill Rising
;; Maintainer: Same <brising@louisville.edu>
;;             URL: http://www.louisville.edu/~wrrisi01
;; Keywords: ado-mode
;; Version:  0.10 of November 9, 2003 

;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description:
;;
;; This file contains all of the customizable things for ado
;; mode.  If you are using Emacs 20, or an earlier version of Emacs
;; which has the customization package installed, you can change all
;; of the relevant variables here via customization.  This is
;; preferable to doing it "by hand" in your .emacs file.
;; .... unless you really really like tinkering with .emacs files.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization groups
;;

(defgroup ado nil
  "Ado mode: major mode for editing ado and do files for Stata."
  :tag "Ado mode"
;  :link '(custom-manual "(ultra)Top")
;  :link '(url-link :tag "Home Page" "http://www.math.washington.edu/~palmieri/Emacs/ultratex.html")
  :group 'local)

(defgroup ado-files nil
  "Files used by ado mode."
  :tag "Ado Files Used"
  :group 'ado)

(defgroup ado-help-info nil
  "Information needed for making good documenation"
  :tag "Ado hlp File Info"
  :group 'ado)

(defgroup ado-style nil
  "Look and Style of ado mode"
  :tag "Ado Mode Style"
  :group 'ado)

;; (defgroup ado-fontify nil
;;   "Fontification style in ado mode."
;;   :tag "Fontification in ado mode."
;;   :group 'ado
;;   :group 'font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; standard customizations

(defcustom ado-mode-hook nil
  "Hook for Ado mode."
  :type '(hook)
  :options '(turn-on-auto-fill)
  :group 'ado)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ado directories
;;

(defcustom ado-confirm-overwrite-flag t
  "*Controls whether emacs asks for confirmation when saving
a buffer will overwrite an already existing file. Defaults
to on, as this conforms with user interface guidelines. Made
buffer-local."
  :type 'boolean
  :group 'ado-files)

(defcustom ado-site-template-dir nil
  "*The directory where templates are stored. These should be
the site-wide templates"
  :type 'directory
  :group 'ado-files)

(defcustom ado-site-label-dir nil
  "*A directory of useful value labels used at a whole site.
Not used yet, though..."
  :type 'directory
  :group 'ado-files)

(defcustom ado-local-label-dir nil
  "*A directory of useful value labels for a particular user." 
  :type 'directory
  :group 'ado-files)

(defcustom ado-new-dir nil 
  "*The directory where new ado files are stored. This should hold ado
  files which could be of use in multiple projects, but which have not
  been properly debugged or documented yet. The value of ado-new-dir
  should be set in each user's .emacs file, but can be changed with the
  set-variable command."
  :type 'directory
  :group 'ado-files)

;; a couple of variables needed for help files.
(defcustom ado-signature-file nil
  "*Signature file to use for help files. This ought to be set to the
.signature file, but stata once used the @ symbol in a special fashion...
...and many folks don't use unix and hence have no .signature file.
If nil, the user will be prompted when writing the first help file. If
the user wants to be left alone, set ado-no-signature to non-nil. Should 
be set in each user's .emacs file."
  :type '(file :must-match t)
  :group 'ado-help-info
  :group 'ado-files)

(defcustom ado-signature-prompt-flag t
  "*Controls whether the user is prompted for a signature at the
bottom of the help files. If off, the user will never be asked,
and just the user's name will be appended to help files. Defaults
to on."
  :type 'boolean
  :group 'ado-help-info)

(defcustom ado-claim-name nil
  "*Name used in the top of help files. May be reset using
set-ado-claim-name. If nil, it will be set when the first help
file is written."
  :type 'string
  :group 'ado-help-info)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ado commenting and indentation variables
;;  all these are made buffer-local ...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom ado-smart-indent-flag t
  "*Turns on or off the smart indenting. May be turned off for 
working on do files. Default value is on."
  :type 'boolean
  :group 'ado-style)

(defcustom ado-return-also-indents-flag t
  "*Makes return (enter) act like newline-and-indent. Defaults
to on, because this is of great utility. May be turned off 
conform with emacs' creators desire to use C-j for this action."
  :type 'boolean
  :group 'ado-style)

(defcustom ado-do-indent-flag nil
  "*If on, then do files will be indented based on ado-smart-indent.
If off, there is no indentation of do files at all. Defaults to off."
  :type 'boolean
  :group 'ado-style)

(defcustom ado-use-modern-split-flag t
  "*If on, uses the /// method for splitting lines, otherwise
it uses the old-school /* */ method. Defaults to on."
  :type 'boolean
  :group 'ado-style)

(defcustom ado-close-under-line-flag t
  "*Indicates whether indentation of close brace is indented according to the
level which it closes. Turn on to have the One True Indentation Style (hehehe)
Turn off to nil to have close braces line up over the following line's 
indentation level. Defaults to being on (of course)."
  :type 'boolean
  :group 'ado-style)

(defcustom ado-auto-newline-flag nil
  "*Force automatic new line after special characters. While the auto newline 
can be really neat, it can also be a royal pain, depending on how often 
braces are inserted mistakenly. Defaults to off."
  :type 'boolean
  :group 'ado-style)

(defcustom ado-closing-brace-alone-flag nil
  "*Force closing braces to be alone on a line when entered. Defaults to off. If
turned on, it will make smcl editing verrry annoying."
  :type 'boolean
  :group 'ado-style)

(defcustom ado-fontify-new-flag t
  "*Set on for automatic fontifying of new programs. Cannot
really see any purpose for this to turned off."
  :type 'boolean
  :group 'ado-style)

(defcustom ado-tab-width 3
  "*Sets the size of tabs when opening or creating ado files. Defaults to 3. 
To change tab-width in an individual buffer, use \\[ado-change-tab-width]."
  :type 'integer
  :group 'ado-style)

(defcustom ado-continued-statement-indent-spaces 2
  "*Extra indentation for continued lines (which is used  when \"#delimit ;\"
has been used or a \\\ or \* comment has been used to split lines. 
Defaults to 2."
  :type 'integer
  :group 'ado-style)

(defcustom ado-comment-column 40
  "*Sets the column at which comments within commands begin. 
Defaults to 40"
  :type 'integer
  :group 'ado-style)

(defcustom ado-debugging-indent-flag t
  "*If off, then debugging commands (such as pause or set trace) are 
indented like any others. Otherwise, this forces the them to be indented 
at the column set in ado-debugging-indent."
  :type 'boolean
  :group 'ado-style)

(defcustom ado-debugging-indent-column 0
  "*Sets the amount by which debugging commands are indented,
given that ado-debugging-indent-flag is on. Defaults to 0."
  :type 'integer
  :group 'ado-style)

(defcustom ado-delimit-indent-flag t
  "*If off, then #delimit commands are indented like any others.
If on the #delimit commands are forced to be indented at the column 
by ado-delimit-indent."
  :type 'boolean
  :group 'ado-style)

(defcustom ado-delimit-indent-column 0
  "*Sets the amount by which #delimit commands are indented,
given that ado-delimit-indent-flag is on. Defaults to 0."
  :type 'integer
  :group 'ado-style)

(defcustom ado-comment-indent-flag t
  "*If off, then initial comments are indented like any others.
If on, the initial comments are forced to be indented at the column 
by ado-comment-indent."
  :type 'boolean
  :group 'ado-style)

(defcustom ado-comment-indent-column 0
  "*Sets the amount by which initial comments are indented,
given that ado-comment-indent-flag is on. Defaults to 0."
  :type 'integer
  :group 'ado-style)


(provide 'ado-cus)

;;; ado-cus.el ends here
