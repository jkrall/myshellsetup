;;; ido.el --- interactively do things with buffers and files.

;; Copyright (C) 1996-2000  Free Software Foundation, Inc.

;; Author: Kim F. Storm <stormware@get2net.dk>
;; Based on: iswitchb by Stephen Eglen <stephen@cns.ed.ac.uk>
;; Maintainer: Kim F. Storm <stormware@get2net.dk>
;; Location: http://hjem.get2net.dk/storm/emacs/
;; Version: 1.10
;; Keywords: extensions convenience

;; This file is (not yet) part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Acknowledgements

;; Infinite amounts of gratitude goes to Stephen Eglen <stephen@cns.ed.ac.uk>
;; who wrote iswitch-buffer mode - from which I ripped off 99% of the code 
;; for ido-switch-buffer and found the inspiration for ido-find-file.
;; The ido package would never have existed without his work.

;;; History

;; Since I discovered Stephen Eglen's excellent iswitchb package, I just
;; couldn't live without it, but once being addicted to switching buffers
;; with a minimum of keystrokes, I soon found that opening files in the
;; old-fashioned way was just too slow - so I decided to write a package
;; which could open files with the same speed and ease as iswitchb could
;; switch buffers.

;; I originally wrote a separate ifindf.el package based on a copy of
;; iswitchb.el, which did for opening files what iswitchb did for
;; switching buffers.  Along the way, I corrected a few errors in
;; ifindf which could have found its way back into iswitchb, but since
;; most of the functionality of the two package was practically
;; identical, I decided that the proper thing to do was to merge my
;; ifindf package back into iswitchb.
;;
;; This is basically what ido (interactively do) is all about; but I
;; found it ackward to merge my changes into the "iswitchb-" namespace,
;; so I invented a common "ido-" namespace for the merged packages.

;;; Commentary:

;; Installation:
;; To get the alternative switch-to-buffer and find-file functions in
;; this package bound to keys, do
;;  (ido-mode)

;; As you type in a substring, the list of buffers or files currently
;; matching the substring are displayed as you type.  The list is
;; ordered so that the most recent buffers or files visited come at
;; the start of the list.
;; The buffer or file at the start of the list will be the one visited
;; when you press return.  By typing more of the substring, the list is
;; narrowed down so that gradually the buffer or file you want will be
;; at the top of the list.  Alternatively, you can use C-s and C-r (or
;; the right and left arrow keys) to rotate buffer or file names in the
;; list until the one you want is at the top of the list.
;; Completion is also available so that you can see what is common to
;; all of the matching buffers or files as you type.

;; This code is based on the iswitchb package by Stephen Eglen, and 
;; large parts of the code and comments is copied directly from iswitchb
;; with only editorial changes on my part.

;;; Example 

;; If I have two buffers called "123456" and "123", with "123456" the
;; most recent, when I use ido-switch-buffer, I first of all get
;; presented with the list of all the buffers
;;
;;       Buffer:  {123456,123} 
;;
;; If I then press 2:
;;       Buffer: 2[3]{123456,123}
;;
;; The list in {} are the matching buffers, most recent first (buffers
;; visible in the current frame are put at the end of the list by
;; default).  At any time I can select the item at the head of the
;; list by pressing RET.  I can also bring the put the first element
;; at the end of the list by pressing C-s or [right], or put the last
;; element at the head of the list by pressing C-r or [left].
;; The item in [] indicates what can be added to my input by pressing TAB.
;; In this case, I will get "3" added to my input.  So, press TAB:
;;	 Buffer: 23{123456,123}
;;
;; At this point, I still have two matching buffers.
;; If I want the first buffer in the list, I simply press RET.  If I
;; wanted the second in the list, I could press C-s to move it to the
;; top of the list and then RET to select it.
;;
;; However, If I type 4, I only have one match left:
;;       Buffer: 234[123456] [Matched]
;;
;; Since there is only one matching buffer left, it is given in [] and we
;; see the text [Matched] afterwards.  I can now press TAB or RET to go
;; to that buffer.
;;
;; If however, I now type "a":
;;       Buffer: 234a [No match]
;; There are no matching buffers.  If I press RET or TAB, I can be
;; prompted to create a new buffer called "234a".
;;
;; Of course, where this function comes in really useful is when you
;; can specify the buffer using only a few keystrokes.  In the above
;; example, the quickest way to get to the "123456" file would be
;; just to type 4 and then RET (assuming there isn't any newer buffer
;; with 4 in its name).

;; To see a full list of all matching buffer in a separate buffer,
;; hit ? or press TAB when there are no further completions to the
;; substring.  Repeated TAB presses will scroll you through this
;; separate buffer.

;; The buffer at the head of the list can be killed by pressing C-k.
;; If the buffer needs saving, you will be queried before the buffer
;; is killed.

;; If you find that the file you are after is not in a buffer, you can
;; press C-f to immediately drop into ido-find-file.

;; Likewise, if you use ido-find-file, the list of files and
;; directories in the current directory is provided in the same
;; fashion as the buffers above. However, the files and directories
;; are simply sorted in alphabetical order.
;;
;; In addition to scrolling through the list using [right] and [left],
;; you can use [up] and [down] to quickly scroll the list to the next
;; or previous subdirectory.
;;
;; To go down into a subdirectory, and continue the file selection on
;; the files in that directory, simply move it to the head of the list
;; and hit RET.
;;
;; To go up to the parent directory, delete any partial file name
;; already specified (e.g. using [backspace]) and hit [backspace].
;;
;; To go to the root directory (on the current drive), enter two slashes.
;; On MS-DOS or Windows, to select the root of another drive, enter X:/
;; where X is the drive letter.
;;
;; If for some reason you cannot specify the proper file using
;; ido-find-file, you can press C-f to enter the normal find-file.
;; You can also press C-b to drop into ido-switch-buffer.

;; See the doc string of ido-switch-buffer and ido-find-file for full
;; keybindings and features.
;;  (describe-function 'ido-find-file)

;;; Customisation

;; See the User Variables section below for easy ways to change the
;; functionality of the program.  These are accessible using the
;; custom package.
;; To modify the keybindings, use the hook provided.  For example:
;;(add-hook 'ido-define-mode-map-hook
;;	  'ido-my-keys)
;;
;;(defun ido-my-keys ()
;;  "Add my keybindings for ido."
;;  (define-key ido-mode-map " " 'ido-next-match)
;;  )
;;
;; Seeing all the matching files
;;
;; If you have many matching files, they may not all fit onto one
;; line of the minibuffer.  In this case, you should use rsz-mini
;; (resize-minibuffer-mode).  You can also limit ido so that it
;; only shows a certain number of lines -- see the documentation for
;; `ido-minibuffer-setup-hook'.

;; Changing the list of files

;; By default, the list of current files is most recent first,
;; oldest last, with the exception that the files visible in the
;; current frame are put at the end of the list.  A hook exists to
;; allow other functions to order the list.  For example, if you add:
;;
;; (add-hook 'ido-make-buffer-list-hook 'ido-summary-buffers-to-end)
;;
;; then all files matching "Summary" are moved to the end of the
;; list.  (I find this handy for keeping the INBOX Summary and so on
;; out of the way.)  It also moves files matching "output\*$" to the
;; end of the list (these are created by AUC TeX when compiling.)
;; Other functions could be made available which alter the list of
;; matching files (either deleting or rearranging elements.)

;; Highlighting

;; The highlighting of matching items is controlled via ido-use-faces.
;; The faces used are ido-first-match-face and ido-only-match-face.
;; Colouring of the matching item was suggested by
;; Carsten Dominik (dominik@strw.leidenuniv.nl)

;; Replacement for read-buffer

;; ido-read-buffer has been written to be a drop in replacement
;; for the normal buffer selection routine `read-buffer'.  To use
;; iswitch for all buffer selections in Emacs, add:
;; (setq read-buffer-function 'ido-read-buffer)
;; (This variable should be present in Emacs 20.3+)
;; XEmacs users can get the same behaviour by doing:
;; (defalias 'read-buffer 'ido-read-buffer) 
;; since `read-buffer' is defined in lisp.

;; Regexp matching

;; There is limited provision for regexp matching within ido,
;; enabled through `ido-enable-regexp'.  This allows you to type `c$'
;; for example and see all file names ending in `c'.  This facility
;; is quite limited though in two respects.  First, you can't
;; currently type in expressions like `[0-9]' directly -- you have to
;; type them in when ido-enable-regexp is nil and then toggle on the
;; regexp functionality.  Likewise, don't enter an expression
;; containing `\' in regexp mode.  If you try, ido gets confused,
;; so just hit C-g and try again.  Secondly, no completion mechanism
;; is currently offered when regexp searching.

;;; TODO


;;; Code:

(provide 'ido)

;; CL needed for cadr and last
(if (not (and (fboundp 'cadr)
	      (fboundp 'last)))
    (require 'cl))

;; Set up the custom library.
;; taken from http://www.dina.kvl.dk/~abraham/custom/
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defcustom (var value doc &rest args) 
      (` (defvar (, var) (, value) (, doc))))))

;;; User Variables
;;
;; These are some things you might want to change.

(defgroup ido nil
  "Switch between files using substrings."
  :group 'extensions
  :group 'convenience
  :link '(emacs-commentary-link :tag "Commentary" "ido.el")
  :link '(emacs-library-link :tag "Lisp File" "ido.el"))

;;;###autoload
(defcustom ido-enabled nil
  "Non-nil means use ido speed-up for switching buffers and finding files.

Setting this variable directly does not take effect;
use either \\[customize] or the function `ido-mode'."
  :set (lambda (symbol value)
	 (if value (ido-mode value)))
  :require 'ido
  :link '(emacs-commentary-link "ido.el")
  :version "20.5"
  :type 'boolean
  :group 'ido)

(defcustom ido-case-fold case-fold-search
  "*Non-nil if searching of buffer and file names should ignore case."
  :type 'boolean
  :group 'ido)

(defcustom ido-ignore-buffers
  '("^ ")
  "*List of regexps or functions matching buffer names to ignore.
For example, traditional behavior is not to list buffers whose names begin
with a space, for which the regexp is `^ '.  See the source file for
example functions that filter buffernames."
  :type '(repeat regexp)
  :group 'ido)

(defcustom ido-ignore-files
  '("^CVS/" "^#" "^.#" "^\\.\\./" "^\\./")
  "*List of regexps or functions matching file names to ignore.
For example, traditional behavior is not to list files whose names begin
with a #, for which the regexp is `^#'.  See the source file for
example functions that filter filenames."
  :type '(repeat regexp)
  :group 'ido)

(defcustom ido-ignore-extensions t
  "*Non-nil means ignore files in completion-ignored-extensions list."
  :type 'boolean
  :group 'ido)

;;; Examples for setting the value of ido-ignore-buffers
;(defun ido-ignore-c-mode (name)
;  "Ignore all c mode buffers -- example function for ido."
;  (save-excursion
;    (set-buffer name)
;    (string-match "^C$" mode-name)))
;
;(setq ido-ignore-buffers '("^ " ido-ignore-c-mode))

;;; Examples for setting the value of ido-ignore-files
;(setq ido-ignore-files '("^ " "\\.c$" "\\.h$"))

(defcustom ido-default-file-method  'always-frame
    "*How to switch to new file when using `ido-find-file'.
Possible values:
`samewindow'	Show new file in same window
`otherwindow'	Show new file in another window (same frame)
`display'	Display file in another window without switching to it
`otherframe'	Show new file in another frame
`maybe-frame'	If a file is visible in another frame, prompt to ask if you
		you want to see the file in the same window of the current
  		frame or in the other frame.
`always-frame'  If a file is visible in another frame, raise that
		frame.  Otherwise, visit the file in the same window."
    :type '(choice (const samewindow) 
		   (const otherwindow)
		   (const display)
		   (const otherframe) 
		   (const maybe-frame)
		   (const always-frame))
    :group 'ido)

(defcustom ido-default-buffer-method  'always-frame
    "*How to switch to new buffer when using `ido-switch-buffer'.
See ido-default-file-method for details."
    :type '(choice (const samewindow) 
		   (const otherwindow)
		   (const display)
		   (const otherframe) 
		   (const maybe-frame)
		   (const always-frame))
    :group 'ido)

(defcustom ido-enable-regexp nil
  "*Non-nil means that `ido' will do regexp matching.
Value can be toggled within `ido' using `ido-toggle-regexp'."
  :type 'boolean
  :group 'ido)

(defcustom ido-record-commands t
  "*Non-nil means that `ido' will record commands in command history.
Note that the non-ido equivalent command is recorded."
  :type 'boolean
  :group 'ido)

(defcustom ido-enable-last-directory-history t
  "*Non-nil means that `ido' will remember latest selected directory paths."
  :type 'boolean
  :group 'ido)

(defcustom ido-create-new-buffer 'prompt
  "*Specify whether a new buffer is created if no buffer matches substring.
Choices are 'always to create new buffers unconditionally, 'prompt to
ask user whether to create buffer, or 'never to never create new buffer."
  :type '(choice (const always) 
		 (const prompt)
		 (const never))
  :group 'ido)

(defcustom ido-define-mode-map-hook  nil
  "*Hook to define keys in `ido-mode-map' for extra keybindings."
  :type 'hook
  :group 'ido)

(defcustom ido-use-faces t
  "*Non-nil means use ido faces to highlighting first or only match."
  :type 'boolean
  :group 'ido)

(defface ido-first-match-face  '((t (:bold t)))
  "*Font used by ido for highlighting first match."
  :group 'ido)

(defface ido-only-match-face  '((((class color)) 
				  (:foreground "red"))
				 (t (:italic t)))
  "*Font used by ido for highlighting only match."
  :group 'ido)

(defcustom ido-make-file-list-hook  nil
  "*Hook to run when list of matching files is created."
  :type 'hook
  :group 'ido)

(defcustom ido-make-buffer-list-hook  nil
  "*Hook to run when list of matching buffers is created."
  :type 'hook
  :group 'ido)

(defvar ido-all-frames 'visible
  "*Argument to pass to `walk-windows' when finding visible files.
See documentation of `walk-windows' for useful values.")

(defcustom ido-minibuffer-setup-hook nil
  "*Ido-specific customization of minibuffer setup.

This hook is run during minibuffer setup iff `ido' will be active.
It is intended for use in customizing ido for interoperation
with other packages.  For instance:

  \(add-hook 'ido-minibuffer-setup-hook 
	    \(function
	     \(lambda ()
	       \(make-local-variable 'resize-minibuffer-window-max-height)
	       \(setq resize-minibuffer-window-max-height 3))))

will constrain rsz-mini to a maximum minibuffer height of 3 lines when
ido is running.  Copied from `icomplete-minibuffer-setup-hook'."
  :type 'hook
  :group 'ido)

;;; Internal Variables

;; Persistent variables

(defvar ido-mode-map nil
  "Keymap for `ido-find-file' and `ido-switch-buffer'.")

(defvar  ido-file-history nil
  "History of files selected using `ido-find-file'.")

(defvar  ido-buffer-history nil
  "History of buffers selected using `ido-switch-buffer'.")

(defvar ido-xemacs  (string-match "XEmacs" (emacs-version))
  "Non-nil if we are running XEmacs.  Otherwise, assume we are running Emacs.")

(defvar ido-last-directory-list nil
  "List of last selected directory paths.")

;; Temporary storage

(defvar ido-eoinput 1
  "Point where minibuffer input ends and completion info begins.
Copied from `icomplete-eoinput'.")
(make-variable-buffer-local 'ido-eoinput)

(defvar ido-common-match-string  nil
  "Stores the string that is common to all matching files.")

(defvar ido-rescan nil
  "Non-nil means we need to regenerate the list of matching items.")

(defvar ido-text nil
  "Stores the users string as it is typed in.")

(defvar ido-text-init nil
  "The initial string for the users string it is typed in.")

(defvar ido-matches nil
  "List of files currently matching `ido-text'.")

(defvar ido-report-no-match t
  "Report [No Match] when no completions matches ido-text.")

(defvar ido-exit nil 
  "Flag to monitor how `ido-find-file' exits.  
If equal to `takeprompt', we use the prompt as the file name to be
selected.")

(defvar ido-current-directory nil
  "Current directory for ido-find-file.")


;; The following variables are dynamic variables created by ido,
;; but they are declared here to keep the byte compiler quiet.

(eval-when-compile
  (defvar ido-cur-item nil
    "Stores the current ido item type ('file or 'buffer).")

  (defvar ido-cur-list nil
    "Stores the current list of files that will be searched through.
The list is ordered, so that the most recent files come first,
although by default, the files visible in the current frame are put
at the end of the list.  Created by `ido-make-item-list'.")

  (defvar ido-keep-item-list nil
    "Keep current item list if non-nil")

  (defvar ido-use-mycompletion-depth 0
    "Non-nil means use `ido' completion feedback.  
Is set by ido functions to the current minibuffer-depth, so that
it doesn't interfere with other minibuffer usage.")

  (defvar ido-process-ignore-lists t
    "Process ido-ignore- lists.")

  (defvar ido-default-item nil
    "Default item for ido.")

  (defvar ido-require-match nil
    "Non-nil if matching file must be selected.")

  (defvar ido-temp-list nil
    "Stores a temporary version of the file list being created.")

  (defvar ido-bufs-in-frame nil
    "List of the files visible in the current frame.")

  (defvar ido-change-word-sub nil 
    "Private variable used by `ido-word-matching-substring'.")

  (defvar ido-saved-vc-mt nil
    "Original value of vc-master-templates for use in ido-toggle-vc.")

  (defvar ido-find-literal nil
    "Stores temporary state of literal find file.")
)

;;; FUNCTIONS

;;;###autoload
(defun ido-mode (arg &optional nobind)
  "Toggle ido speed-ups on or off.
With ARG, turn ido speed-up on if arg is positive, off otherwise.
If second argument NOBIND is non-nil, no keys are rebound; otherwise,
turning on ido-mode will modify the default keybindings for the 
find-file and switch-to-buffer families of commands to the ido
versions of these functions.
This function also adds a hook to the minibuffer."
  (interactive "P")
  (setq ido-enabled
	(if (null arg)
	    (not ido-enabled)
	  (> (prefix-numeric-value arg) 0)))
  (when ido-enabled
    (add-hook 'minibuffer-setup-hook 'ido-minibuffer-setup)
    (when (not nobind)
      (define-key ctl-x-map "\C-f"   'ido-find-file)
      ;;(define-key ctl-x-map "\C-r"   'ido-find-file-read-only)
      (define-key ctl-x-map "\C-v"   'ido-find-alternate-file)
      (define-key ctl-x-map "\C-w"   'ido-write-file)
      (define-key ctl-x-map "i"      'ido-insert-file)
      ;;(define-key ctl-x-map "\C-d"   'ido-list-directory)

      (define-key ctl-x-map "b"      'ido-switch-buffer)
      (define-key ctl-x-map "\C-i"   'ido-insert-buffer)
      (define-key ctl-x-map "k"      'ido-kill-buffer)

      (define-key ctl-x-4-map "f"    'ido-find-file-other-window)
      (define-key ctl-x-4-map "\C-f" 'ido-find-file-other-window)
      ;;(define-key ctl-x-4-map "r"    'ido-find-file-read-only-other-window)

      (define-key ctl-x-4-map "b"    'ido-switch-buffer-other-window)
      (define-key ctl-x-4-map "\C-o" 'ido-display-buffer)

      (define-key ctl-x-5-map "f"    'ido-find-file-other-frame)
      (define-key ctl-x-5-map "\C-f" 'ido-find-file-other-frame)
      ;;(define-key ctl-x-5-map "r"    'ido-find-file-read-only-other-frame)

      (define-key ctl-x-5-map "b"    'ido-switch-buffer-other-frame)
      )))
  

;;; IDO KEYMAP 
(defun ido-define-mode-map ()
  "Set up the keymap for `ido'."
  (let (map)
    ;; generated every time so that it can inherit new functions.
    ;;(or ido-mode-map

    (setq map (copy-keymap minibuffer-local-map))
    (define-key map "?" 'ido-completion-help)
    (define-key map "\C-s" 'ido-next-match)
    (define-key map [right] 'ido-next-match)
    (define-key map "\C-r" 'ido-prev-match)
    (define-key map [left] 'ido-prev-match)
    (define-key map "\t" 'ido-complete)
    (define-key map "\C-j" 'ido-select-text)
    (define-key map "\C-t" 'ido-toggle-regexp)
    (define-key map "\C-f" 'ido-enter-find-file)
    (define-key map "\C-a" 'ido-toggle-ignore)
    (define-key map "\C-c" 'ido-toggle-case)
    (define-key map "\C-m" 'ido-exit-minibuffer)
    (cond
     ((eq ido-cur-item 'file)
      (define-key map "\C-b" 'ido-enter-switch-buffer)
      (define-key map "\C-d" 'ido-enter-dired)
      (define-key map "\C-v" 'ido-toggle-vc)
      (define-key map "\C-l" 'ido-toggle-literal)
      (define-key map [down] 'ido-next-match-dir)
      (define-key map [up]   'ido-prev-match-dir)
      (define-key map [backspace] 'ido-delete-backward-updir))

     ((eq ido-cur-item 'buffer)
      (define-key map "\C-k" 'ido-kill-buffer-at-head)))

    (setq ido-mode-map map)
    (run-hooks 'ido-define-mode-map-hook)))

(defun ido-set-current-home ()
  (let ((home (expand-file-name (getenv "HOME"))))
    (if home
	(setq ido-current-directory
	      (if (string-equal (substring home -1) "/")
		  home
		(concat home "/"))))))

(defun ido-record-command (command arg)
  "Add (command arg) to command-history if ido-record-commands is t."
  (if ido-record-commands
      (let ((cmd (list command arg)))
	(if (or (not command-history)
		(not (equal cmd (car command-history))))
	    (setq command-history (cons cmd command-history))))))
	
(defun ido-read-internal (item prompt history &optional default require-match)
  "Perform the ido-read-buffer and ido-read-filename functions.
Return the name of a buffer or file selected.  
PROMPT is the prompt to give to the user.
DEFAULT if given is the default directory to start with.
If REQUIRE-MATCH is non-nil, an existing file must be selected."
  (let
      ((ido-cur-item item)
       (ido-process-ignore-lists t)
       (ido-set-default-item t)
       selected
       final-text
       (done nil)
       (icomplete-mode nil) ;; prevent icomplete starting up
       ;; Exported dynamic variables:
       ido-cur-list
       (ido-keep-item-list nil)
       )

    (ido-define-mode-map)
    (while (not done)
      (setq ido-exit nil)
      (setq ido-rescan t)
      (setq ido-text "")
      (if ido-set-default-item
       (setq ido-default-item
	     (cond
	      ((eq item 'buffer)
	       (if (bufferp default) (buffer-name default) default))
	      ((eq item 'file)
	       (and ido-enable-last-directory-history 
		    (assoc-default ido-current-directory ido-last-directory-list))))
	     ido-set-default-item nil))
      (if ido-keep-item-list
	  (setq ido-keep-item-list nil)
	(ido-make-item-list ido-default-item))
      (ido-set-matches)
      (let 
	  ((minibuffer-local-completion-map ido-mode-map)
	   (ido-prepost-hooks t)
	   (ido-require-match require-match)
	   (ido-use-mycompletion-depth (1+ (minibuffer-depth))))
	;; prompt the user for the file name
	(setq ido-exit nil)
	(setq final-text (completing-read 
			  (if (and (eq item 'file) ido-current-directory)
			      (concat prompt 
				      (if (and (boundp 'ido-find-literal) ido-find-literal)
					  "(literal) " "")
				      (if (and ido-saved-vc-mt (not vc-master-templates))
					  "[-VC] " "") 
				      ido-current-directory)	;the prompt
			    prompt)
			  '(("dummy".1))	;table
			  nil	;predicate
			  nil	;require-match [handled elsewhere]
			  (prog1 ido-text-init (setq ido-text-init nil))	;initial-contents
			  history)))
      (cond
       ((eq ido-exit 'refresh)
	nil)

       ((eq ido-exit 'keep)
	(setq ido-keep-item-list t))

       ((memq ido-exit '(dired findfile findbuffer))
	(setq done t))

       ((eq ido-exit 'updir)
	(setq ido-current-directory (file-name-directory (substring ido-current-directory 0 -1)))
	(setq ido-set-default-item t))

       ;; Handling the require-match must be done in a better way.
       ((and require-match (not (ido-existing-item-p)))
	(error "must specify valid item"))

       (t
	(setq selected
	      (if (and ido-matches (equal final-text ""))
		  (car ido-matches)	;; possibly choose the default file
		(if (or (eq ido-exit 'takeprompt)
			(null ido-matches))
		    final-text
		  ;; else take head of list
		  (car ido-matches))))

	(cond
	 ((eq item 'buffer)
	  (setq done t))

	 ((and (string-equal ido-current-directory "/")
	       (string-match "..:$" selected)) ;; Ange-ftp 
	  (setq ido-current-directory (concat "/" selected "/"))
	  (setq ido-set-default-item t))

	 ((or (string-match "[/\\][^/\\]" selected)
	      (and (memq system-type '(windows-nt ms-dos))
		   (string-match "^.:" selected)))
	  (setq ido-current-directory (file-name-directory selected))
	  (setq ido-set-default-item t))

	 ((string-match "^~" selected)
	  (ido-set-current-home))

	 ((string-equal (substring selected -1) "/")
	  (if ido-enable-last-directory-history
	      (let ((x (assoc ido-current-directory ido-last-directory-list)))
		(if x
		    (setcdr x selected)
		  (setq ido-last-directory-list
			(cons (cons ido-current-directory selected) ido-last-directory-list)))))
	  (setq ido-current-directory (concat ido-current-directory selected))
	  (setq ido-set-default-item t))

	 (t
	  (setq done t))))))
    selected))


;;; MAIN FUNCTIONS
(defun ido-buffer-internal (method &optional prompt default)
  "Internal function for ido-switch-buffer and friends."
  	
  (let ((buf (ido-read-buffer (or prompt "Buffer: ") default)))

    ;; Choose the buffer name: either the text typed in, or the head
    ;; of the list of matches

    (cond 
     ((eq ido-exit 'findfile)
      (setq ido-text-init ido-text)
      (call-interactively 'ido-find-file))

     ;; Check buf is non-nil.
     ((not buf) nil)

     ;; View buffer if it exists
     ((get-buffer buf)
      (if (eq method 'insert)
	  (progn
	    (ido-record-command 'insert-buffer buf)
	    (insert-buffer buf))
	(ido-visit-buffer buf method t)))

     ;; buffer doesn't exist
     ((eq ido-create-new-buffer 'never)
      (message "no buffer matching `%s'" buf))

     ((and (eq ido-create-new-buffer 'prompt)
	   (not (y-or-n-p (format "No buffer matching `%s', create one? " buf))))
      nil)

     ;; create a new buffer
     (t
      (setq buf (get-buffer-create buf))
      (if (fboundp 'set-buffer-major-mode)
	  (set-buffer-major-mode buf))
      (ido-visit-buffer buf method t)))))

;;;###autoload
(defun ido-read-buffer (prompt &optional default require-match)
  "Replacement for the built-in `read-buffer'.
Return the name of a buffer selected.  
PROMPT is the prompt to give to the user.  DEFAULT if given is the default
buffer to be selected, which will go to the front of the list.
If REQUIRE-MATCH is non-nil, an existing-buffer must be selected."
  (let ((ido-current-directory nil))
    (ido-read-internal 'buffer prompt 'ido-buffer-history default require-match)))


(defun ido-file-internal (method &optional default prompt)
  "Internal function for ido-find-file and friends."
  (let (filename ido-saved-vc-mt
	(vc-master-templates
	 (and (boundp 'vc-master-templates) vc-master-templates))
	(ido-current-directory
	 (or default default-directory))
	(ido-find-literal nil))

    (setq ido-saved-vc-mt vc-master-templates)
    (setq filename (ido-read-internal 'file
				      (or prompt "Find file: ")
				      'ido-file-history default))

    ;; Choose the file name: either the text typed in, or the head
    ;; of the list of matches

    (cond
     ((eq ido-exit 'findfile)
      (let ((default-directory ido-current-directory))
	(call-interactively 'find-file)))

     ((eq ido-exit 'findbuffer)
      (setq ido-text-init ido-text)
      (call-interactively 'ido-switch-buffer))

     ((eq ido-exit 'dired)
      (dired ido-current-directory))

     ((eq method 'alt-file)
      (let ((default-directory ido-current-directory))
	(find-alternate-file filename)))

     ((eq method 'write)
      (let ((default-directory ido-current-directory))
	(ido-record-command 'write-file (concat ido-current-directory filename))
	(write-file filename)))

     ((eq method 'insert)
      (let ((default-directory ido-current-directory))
	(ido-record-command 
	 (if ido-find-literal 'insert-file-literally 'insert-file)
	 filename)
	(if ido-find-literal
	    (insert-file-contents-literally filename)
	  (insert-file-contents filename))))

     (filename
      (setq filename (concat ido-current-directory filename))
      (ido-record-command 'find-file filename)
      (ido-visit-buffer (find-file-noselect filename nil ido-find-literal) method)))))

(defun ido-existing-item-p ()
  "Return non-nil if there is a matching item."
  (not (null ido-matches)))

;;; COMPLETION CODE

(defun ido-set-common-completion  ()
  "Find common completion of `ido-text' in `ido-matches'.
The result is stored in `ido-common-match-string'."

  (let* (val)
    (setq  ido-common-match-string nil)
    (if (and ido-matches
	     (not ido-enable-regexp) ;; testing
             (stringp ido-text)
             (> (length ido-text) 0))
        (if (setq val (ido-find-common-substring
                       ido-matches ido-text))
            (setq ido-common-match-string val)))
    val))

(defun ido-complete ()
  "Try and complete the current pattern amongst the file names."
  (interactive)
  (let (res)
    (cond ((not  ido-matches)
	   (ido-completion-help))
	  
	  ((= 1 (length ido-matches))
	   ;; only one choice, so select it.
	   (exit-minibuffer))
	  
	  (t
	   ;; else there could be some completions
	   (setq res ido-common-match-string)
	   (if (and (not (memq res '(t nil)))
		    (not (equal res ido-text)))
	       ;; found something to complete, so put it in the minibuffer.
	       (progn
		 (setq ido-rescan nil)
		 (delete-region (point-min) (point))
		 (insert res))
	     ;; else nothing to complete
	     (ido-completion-help)
	     )))))

;;; TOGGLE FUNCTIONS

(defun ido-toggle-case ()
  "Toggle the value of `ido-case-fold'."
  (interactive)
  (setq ido-case-fold (not ido-case-fold))
  ;; ask for list to be regenerated.
  (setq ido-rescan t))

(defun ido-toggle-regexp ()
  "Toggle the value of `ido-enable-regexp'."
  (interactive)
  (setq ido-enable-regexp (not ido-enable-regexp))
  ;; ask for list to be regenerated.
  (setq ido-rescan t))

(defun ido-toggle-ignore ()
  "Toggle ignoring files specified with `ido-ignore-files'."
  (interactive)
  (setq ido-process-ignore-lists (not ido-process-ignore-lists))
  (ido-make-item-list ido-default-item)
  ;; ask for list to be regenerated.
  (setq ido-rescan t))

(defun ido-toggle-vc ()
  "Disable version control for this file."
  (interactive)
  (if (and ido-enabled (eq ido-cur-item 'file))
      (progn
	(setq vc-master-templates 
	      (if vc-master-templates nil ido-saved-vc-mt))
	(setq ido-text-init ido-text)
	(setq ido-exit 'keep)
	(exit-minibuffer))))

(defun ido-toggle-literal ()
  "Toggle literal reading of this file."
  (interactive)
  (if (and ido-enabled (eq ido-cur-item 'file))
      (progn
	(setq ido-find-literal (not ido-find-literal))
	(setq ido-text-init ido-text)
	(setq ido-exit 'keep)
	(exit-minibuffer))))

(defun ido-exit-minibuffer ()
  "Exit minibuffer, but make sure we have a match if one is needed."
  (interactive)
  (if (or (not ido-require-match)
	   (ido-existing-item-p))
      (throw 'exit nil)))

(defun ido-select-text ()
  "Select the buffer or file named by the prompt.
If no buffer or file exactly matching the prompt exists, maybe create a new one."
  (interactive)
  (setq ido-exit 'takeprompt)
  (exit-minibuffer))

(defun ido-enter-find-file ()
  "Drop into find-file from file switching."
  (interactive)
  (setq ido-exit 'findfile)
  (exit-minibuffer))

(defun ido-enter-switch-buffer ()
  "Drop into ido-switch-buffer from file switching."
  (interactive)
  (setq ido-exit 'findbuffer)
  (exit-minibuffer))

(defun ido-enter-dired ()
  "Drop into dired from file switching."
  (interactive)
  (setq ido-exit 'dired)
  (exit-minibuffer))

(defun ido-up-directory ()
  "Go up one directory level."
  (interactive)
  (setq ido-exit 'updir)
  (exit-minibuffer))

(defun ido-delete-backward-updir (count)
  "Delete char backwards, or at beginning of buffer, go up one level."
  (interactive "P")
  (if (bobp)
      (if (not count)
	  (ido-up-directory))
    (delete-backward-char (prefix-numeric-value count))))

(defun ido-next-match () 
  "Put first element of `ido-matches' at the end of the list."
  (interactive)
  (if ido-matches
      (let ((next  (cadr ido-matches)))
	(setq ido-cur-list (ido-chop ido-cur-list next))
	(setq ido-rescan t))))

(defun ido-prev-match () 
  "Put last element of `ido-matches' at the front of the list."
  (interactive)
  (if ido-matches
      (let ((prev  (car (last ido-matches))))
	(setq ido-cur-list (ido-chop ido-cur-list prev))
	(setq ido-rescan t))))

(defun ido-next-match-dir () 
  "Find next directory in match list."
  (interactive)
  (let ((cnt (length ido-matches))
	(i 1))
    (while (and (< i cnt)
		(not (string-equal (substring (nth i ido-matches) -1) "/")))
      (setq i (1+ i)))
    (if (< i cnt)
	(setq ido-cur-list (ido-chop ido-cur-list (nth i ido-matches))))))

(defun ido-prev-match-dir () 
  "Find previous directory in match list."
  (interactive)
  (let* ((cnt (length ido-matches))
	(i (1- cnt)))
    (while (and (> i 0)
		(not (string-equal (substring (nth i ido-matches) -1) "/")))
      (setq i (1- i)))
    (if (> i 0)
	(setq ido-cur-list (ido-chop ido-cur-list (nth i ido-matches))))))

(defun ido-chop (list elem)
  "Remove all elements before ELEM and put them at the end of LIST."
  (let ((ret nil)
	(next nil)
	(sofar nil))
    (while (not ret)
      (setq next (car list))
      (if (equal next elem)
	  (setq ret (append list (nreverse sofar)))
	;; else
	(progn
	  (setq list (cdr list))
	  (setq sofar (cons next sofar)))))
    ret))

;;; CREATE LIST OF ALL CURRENT FILES

(defun ido-make-item-list (&optional default)
  "Set `ido-cur-list' to the current list of buffers or files."
  (setq ido-cur-list
	(cond
	 ((eq ido-cur-item 'file)
	  (ido-make-file-list default))
	 ((eq ido-cur-item 'buffer)
	  (ido-make-buffer-list default))
	 (t nil))))

(defun ido-make-buffer-list (default)
  "Return the current list of buffers.
Currently visible buffers are put at the end of the list.
The hook `ido-make-buflist-hook' is run after the list has been 
created to allow the user to further modify the order of the buffer names
in this list.  If DEFAULT is non-nil, and corresponds to an existing buffer,
it is put to the start of the list."
  (let* ((ido-current-buffers (ido-get-buffers-in-frames))
	 (ido-temp-list
	  (delq nil 
		(mapcar
		 (lambda (x)
		   (let ((b-name (buffer-name x)))
		     (if (not 
			  (or 
			   (ido-ignore-item-p b-name)
			   (memq b-name ido-current-buffers)))
			 b-name)))
		 (buffer-list)))))
    (if ido-temp-list
	(nconc ido-temp-list ido-current-buffers)
      (setq ido-temp-list ido-current-buffers))
    (run-hooks 'ido-make-buffer-list-hook)
    ;; Should this be after the hooks, or should the hooks be the
    ;; final thing to be run?
    (if default
	(progn
	  (setq ido-temp-list 
		(delete default ido-temp-list))
	  (setq ido-temp-list 
		(cons default ido-temp-list))))
    ido-temp-list))

(defun ido-make-file-list (default)
  "Return the current list of files.
Currently visible files are put at the end of the list.
The hook `ido-make-file-list-hook' is run after the list has been 
created to allow the user to further modify the order of the file names
in this list."
  (let ((ido-temp-list
	 (delq nil 
	       (mapcar
		(lambda (f-name)
		  (and (not (ido-ignore-item-p f-name)) f-name))
		(file-name-all-completions "" ido-current-directory))))
	dot-files)
    (setq ido-temp-list (sort ido-temp-list (lambda (a b) (string-lessp a b))))
    (setq dot-files
	  (delq nil (mapcar 
		     (lambda (x) (if (string-equal (substring x 0 1) ".") x))
		     ido-temp-list)))
    (ido-to-end dot-files)
    (run-hooks 'ido-make-file-list-hook)
    ;; Should this be after the hooks, or should the hooks be the
    ;; final thing to be run?
    (if (and default (member default ido-temp-list))
	(progn
	  (setq ido-temp-list 
		(delete default ido-temp-list))
	  (setq ido-temp-list 
		(cons default ido-temp-list))))
    ido-temp-list))

(defun ido-to-end (lst)
  "Move the elements from LST to the end of `ido-temp-list'."
  (mapcar 
   (lambda (elem)  
     (setq ido-temp-list (delq elem ido-temp-list)))
   lst)
  (nconc ido-temp-list lst))

(defun ido-get-buffers-in-frames (&optional current)
  "Return the list of buffers that are visible in the current frame.
If optional argument `current' is given, restrict searching to the
current frame, rather than all frames, regardless of value of
`ido-all-frames'."
  (let ((ido-bufs-in-frame nil))
    (walk-windows 'ido-get-bufname nil
		  (if current 
		      nil
		    ido-all-frames))
    ido-bufs-in-frame))

(defun ido-get-bufname (win)
  "Used by `ido-get-buffers-in-frames' to walk through all windows."
  (let ((buf (buffer-name (window-buffer win))))
	(if (not (member buf ido-bufs-in-frame))
	    ;; Only add buf if it is not already in list.
	    ;; This prevents same buf in two different windows being
	    ;; put into the list twice.
	    (setq ido-bufs-in-frame
		  (cons buf ido-bufs-in-frame)))))

;;; FIND MATCHING ITEMS

(defun ido-set-matches ()
  "Set `ido-matches' to the list of items matching prompt."
  (if ido-rescan
      (let* ((case-fold-search  ido-case-fold)
	     (re (if ido-enable-regexp ido-text (regexp-quote ido-text))))

	(setq ido-matches nil)
	(mapcar
	 (lambda (name)
	   (if (string-match re name)
		;(and ...    (not (ido-ignore-item-p name)))
	       (setq ido-matches (cons name ido-matches)))
	   t)
	 (reverse ido-cur-list)))))
	 
(defun ido-ignore-item-p (name)
  "Return t if the buffer or file NAME should be ignored."
  (and ido-process-ignore-lists
       (let ((data       (match-data))
	     (re-list    (if (eq ido-cur-item 'file)
			     ido-ignore-files
			   ido-ignore-buffers))
	     (ext-list   (and (eq ido-cur-item 'file)
			      ido-ignore-extensions
			      completion-ignored-extensions))
	     ignorep nextstr 
	     (flen (length name)) slen)
	 (while ext-list
	   (setq nextstr (car ext-list))
	   (cond
	    ((stringp nextstr)
	     (setq slen (length nextstr))
	     (if (and (>= flen slen)
		      (string-equal (substring name (- flen slen)) nextstr))
		 (setq ignorep t
		       ext-list nil
		       re-list nil)))
	    ((fboundp nextstr)
	     (if (funcall nextstr name)
		 (setq ignorep t
		       ext-list nil
		       re-list nil))))
	   (setq ext-list (cdr ext-list)))
	 (while re-list
	   (setq nextstr (car re-list))
	   (cond
	    ((stringp nextstr)
	     (if (string-match nextstr name)
		 (progn
		   (setq ignorep t)
		   (setq re-list nil))))
	    ((fboundp nextstr)
	     (if (funcall nextstr name)
		 (progn
		   (setq ignorep t)
		   (setq re-list nil)))))
	   (setq re-list (cdr re-list)))
	 (set-match-data data)

	 ;; return the result
	 ignorep)))

(defun ido-find-common-substring (lis subs)
  "Return common string following SUBS in each element of LIS."
  (let (res
        alist
        ido-change-word-sub)
    (setq ido-change-word-sub
          (if ido-enable-regexp
              subs
            (regexp-quote subs)))
    (setq res (mapcar 'ido-word-matching-substring lis))
    (setq res (delq nil res)) ;; remove any nil elements (shouldn't happen)
    (setq alist (mapcar 'ido-makealist res)) ;; could use an  OBARRAY

    ;; try-completion returns t if there is an exact match.
    (let ((completion-ignore-case ido-case-fold))

    (try-completion subs alist))))

(defun ido-word-matching-substring (word)
  "Return part of WORD before 1st match to `ido-change-word-sub'.
If `ido-change-word-sub' cannot be found in WORD, return nil."
  (let ((case-fold-search ido-case-fold)) 
    (let ((m (string-match ido-change-word-sub word)))
      (if m
          (substring word m)
        ;; else no match
        nil))))

(defun ido-makealist (res)
  "Return dotted pair (RES . 1)."
  (cons res 1))


(defun ido-completion-help ()
  "Show possible completions in a *File Completions* buffer."
  ;; we could allow this buffer to be used to select match, but I think
  ;; choose-completion-string will need redefining, so it just inserts
  ;; choice with out any previous input.  
  (interactive)
  (setq ido-rescan nil)
  (let ((completion-setup-hook nil)	;disable fancy highlight/selection.
	(buf (current-buffer))
	(temp-buf "*File Completions*")
	(win)
	(again (eq last-command this-command)))

    (if again
	;; scroll buffer
	(progn
	  (set-buffer temp-buf)
	  (setq win (get-buffer-window temp-buf))
	  (if (pos-visible-in-window-p (point-max) win)
	      (set-window-start win (point-min))
	    (scroll-other-window))
	  (set-buffer buf))
      
      (with-output-to-temp-buffer temp-buf
	(if ido-xemacs 
	    
	    ;; XEmacs extents are put on by default, doesn't seem to be
	    ;; any way of switching them off.
	    (display-completion-list (or ido-matches ido-cur-list)
				     :help-string "ido "
				   :activate-callback 
				   '(lambda (x y z) 
				      (message "doesn't work yet, sorry!")))
	  ;; else running Emacs
	  (display-completion-list (if ido-matches
				     ido-matches
				     ido-cur-list))
	  )))))

;;; KILL CURRENT BUFFER
(defun ido-kill-buffer-at-head ()
  "Kill the buffer at the head of `ido-matches'."
  (interactive)
  (let ( (enable-recursive-minibuffers t)
	 buf)

    (setq buf (car ido-matches))
    ;; check to see if buf is non-nil.
    (if buf
	(progn
	  (kill-buffer buf)

	  ;; Check if buffer exists.  XEmacs gnuserv.el makes alias
	  ;; for kill-buffer which does not return t if buffer is
	  ;; killed, so we can't rely on kill-buffer return value.
	  (if (get-buffer buf)
	      ;; buffer couldn't be killed.
	      (setq ido-rescan t)	
	    ;; else buffer was killed so remove name from list.
	    (setq ido-cur-list  (delq buf ido-cur-list)))))))

;;; VISIT CHOSEN BUFFER
(defun ido-visit-buffer (buffer method &optional record)
  "Visit file named FILE according to METHOD.
Record command in command-history if optional RECORD is non-nil."

  (let (win newframe)
    (cond
     ((eq method 'kill)
      (if record
	  (ido-record-command 'kill-buffer buffer))
      (kill-buffer buffer))

     ((eq method 'samewindow)
      (if record
	  (ido-record-command 'switch-to-buffer buffer))
      (switch-to-buffer buffer))

     ((memq method '(always-frame maybe-frame))
      (cond
       ((and (setq win (ido-window-buffer-p buffer))
	     (or (eq method 'always-frame)
		 (y-or-n-p "Jump to frame? ")))
	(setq newframe (window-frame win))
	(raise-frame newframe)
	(select-frame newframe)
	(select-window win)
	(if (not ido-xemacs)
	    ;; reposition mouse to make frame active.  not needed in XEmacs
	    ;; This line came from the other-frame defun in Emacs.
	    (set-mouse-position (selected-frame) (1- (frame-width)) 0)))
       (t
	;;  No buffer in other frames...
	(if record
	    (ido-record-command 'switch-to-buffer buffer))
	(switch-to-buffer buffer)
	)))

     ((eq method 'otherwindow)
      (if record
	  (ido-record-command 'switch-to-buffer buffer))
      (switch-to-buffer-other-window buffer))

     ((eq method 'display)
      (display-buffer buffer))

     ((eq method 'otherframe)
      (progn
	(switch-to-buffer-other-frame buffer)
	(if (not ido-xemacs)
	    (set-mouse-position (selected-frame) (1- (frame-width)) 0))
	)))))


(defun ido-window-buffer-p  (buffer)
  "Return window pointer if BUFFER is visible in another frame.
If BUFFER is visible in the current frame, return nil."
  (let ((blist (ido-get-buffers-in-frames 'current)))
    ;;If the buffer is visible in current frame, return nil
    (if (memq buffer blist)
	nil
      ;;  maybe in other frame or icon
      (get-buffer-window buffer 0) ; better than 'visible
      )))


(defun ido-switch-buffer ()
  "Switch to another buffer.
The buffer is displayed according to `ido-default-buffer-method' -- the
default is to show it in the same window, unless it is already visible
in another frame.

As you type in a string, all of the buffers matching the string are
displayed.  When you have found the buffer you want, it can then be
selected.  As you type, most keys have their normal keybindings,
except for the following:
\\<ido-mode-map>

RET Select the buffer at the front of the list of matches.  If the
list is empty, possibly prompt to create new buffer.

\\[ido-select-text] Select the current prompt as the buffer.
If no buffer is found, prompt for a new one.

\\[ido-next-match] Put the first element at the end of the list.
\\[ido-prev-match] Put the last element at the start of the list.
\\[ido-complete] Complete a common suffix to the current string that 
matches all buffers.  If there is only one match, select that buffer.
If there is no common suffix, show a list of all matching buffers
in a separate window.
\\[ido-toggle-regexp] Toggle regexp searching.
\\[ido-toggle-case] Toggle case-sensitive searching of buffer names.
\\[ido-completion-help] Show list of matching buffers in separate window.
\\[ido-enter-find-file] Drop into ido-find-file.
\\[ido-kill-buffer-at-head] Kill buffer at head of buffer list."
  ;;\\[ido-toggle-ignore] Toggle ignoring certain buffers (see `ido-buffer-ignore')

  (interactive)
  (if ido-enabled
      (ido-buffer-internal ido-default-buffer-method)
    (call-interactively 'switch-to-buffer)))

;;;###autoload
(defun ido-switch-buffer-other-window ()
  "Switch to another buffer and show it in another window.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido'."
  (interactive)
  (if ido-enabled
      (ido-buffer-internal 'otherwindow)
    (call-interactively 'switch-to-buffer-other-window)))

;;;###autoload
(defun ido-display-buffer ()
  "Display a buffer in another window but don't select it.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido'."
  (interactive)
  (if ido-enabled
      (ido-buffer-internal 'display)
    (call-interactively 'display-buffer)))

;;;###autoload
(defun ido-kill-buffer ()
  "Kill a buffer.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido'."
  (interactive)
  (if ido-enabled
      (ido-buffer-internal 'kill "Kill buffer: " (buffer-name (current-buffer)))
    (call-interactively 'kill-buffer)))

;;;###autoload
(defun ido-insert-buffer ()
  "Insert contents of a buffer in current buffer after point.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido'."
  (interactive)
  (if ido-enabled
      (ido-buffer-internal 'insert "Insert buffer: ")
    (call-interactively 'kill-buffer)))

;;;###autoload
(defun ido-switch-buffer-other-frame ()
  "Switch to another buffer and show it in another frame.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido'."
  (interactive)
  (if ido-enabled
      (ido-buffer-internal 'otherframe)
    (call-interactively 'switch-to-buffer-other-frame)))

;;;###autoload
(defun ido-find-file-in-dir (dir)
  "Switch to another file starting from DIR."
  (interactive "DDir: ")
  (if (not (equal (substring dir -1) "/"))
      (setq dir (concat dir "/")))
  (ido-file-internal ido-default-file-method dir))

;;;###autoload
(defun ido-find-file ()
  "Edit file with name obtained via minibuffer.
The file is displayed according to `ido-default-file-method' -- the
default is to show it in the same window, unless it is already
visible in another frame.

The file name is selected interactively by typing a substring. 
As you type in a string, all of the filenames matching the string are
displayed.  When you have found the filename you want, it can then be
selected.  As you type, most keys have their normal keybindings,
except for the following:
\\<ido-mode-map>

RET Select the file at the front of the list of matches.  If the
list is empty, possibly prompt to create new file.

\\[ido-select-text] Select the current prompt as the buffer or file.
If no buffer or file is found, prompt for a new one.

\\[ido-next-match] Put the first element at the end of the list.
\\[ido-prev-match] Put the last element at the start of the list.
\\[ido-complete] Complete a common suffix to the current string that 
matches all files.  If there is only one match, select that file.
If there is no common suffix, show a list of all matching files
in a separate window.
\\[ido-toggle-regexp] Toggle regexp searching.
\\[ido-toggle-case] Toggle case-sensitive searching of file names.
\\[ido-toggle-vc] Toggle version control for this file.
\\[ido-toggle-literal] Toggle literal reading of this file.
\\[ido-completion-help] Show list of matching files in separate window.
\\[ido-normal-find-file] Exit ido and drop into find-file.
\\[ido-kill-buffer-at-head] Kill buffer at head of buffer list."
  ;;\\[ido-toggle-ignore] Toggle ignoring certain files (see `ido-ignore-files')

  (interactive)
  (if ido-enabled
      (ido-file-internal ido-default-file-method)
    (call-interactively 'find-file)))

;;;###autoload
(defun ido-find-file-other-window ()
  "Switch to another file and show it in another window.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'."
  (interactive)
  (if ido-enabled
      (ido-file-internal 'otherwindow)
    (call-interactively 'find-file-other-window)))

;;;###autoload
(defun ido-find-alternate-file ()
  "Switch to another file and show it in another window.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'."
  (interactive)
  (if ido-enabled
      (ido-file-internal 'alt-file nil "Find alternate file: ")
    (call-interactively 'find-alternate-file)))

;;;###autoload
(defun ido-display-file ()
  "Display a file in another window but don't select it.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'."
  (interactive)
  (ido-file-internal 'display))

;;;###autoload
(defun ido-find-file-other-frame ()
  "Switch to another file and show it in another frame.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'."
  (interactive)
  (if ido-enabled
      (ido-file-internal 'otherframe)
    (call-interactively 'find-file-other-frame)))

;;;###autoload
(defun ido-write-file ()
  "Write current buffer to a file.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'."
  (interactive)
  (if ido-enabled
      (let ((ido-process-ignore-lists t)
	    (ido-report-no-match nil)
	    (ido-ignore-files (cons "[^/]$" ido-ignore-files)))
	(ido-file-internal 'write nil "Write file: "))
    (call-interactively 'write-file)))

;;;###autoload
(defun ido-insert-file ()
  "Insert contents of file in current buffer.
The file name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] ido-find-file'."
  (interactive)
  (if ido-enabled
      (ido-file-internal 'insert nil "Insert file: ")
    (call-interactively 'insert-file)))

;;; XEmacs hack for showing default buffer

;; The first time we enter the minibuffer, Emacs puts up the default
;; buffer to switch to, but XEmacs doesn't -- presumably there is a
;; subtle difference in the two versions of post-command-hook.  The
;; default is shown for both whenever we delete all of our text
;; though, indicating its just a problem the first time we enter the
;; function.  To solve this, we use another entry hook for emacs to
;; show the default the first time we enter the minibuffer.

(defun ido-init-XEmacs-trick ()
  "Display default buffer when first entering minibuffer.
This is a hack for XEmacs, and should really be handled by `ido-exhibit'."
  (if (ido-entryfn-p)
      (progn
	(ido-exhibit)
	(goto-char (point-min)))))

;; add this hook for XEmacs only.
(if ido-xemacs
    (add-hook 'ido-minibuffer-setup-hook 
	      'ido-init-XEmacs-trick))

;;; XEmacs / backspace key
;; For some reason, if the backspace key is pressed in XEmacs, the
;; line gets confused, so I've added a simple key definition to make
;; backspace act like the normal delete key.  

(defun ido-xemacs-backspacekey ()
  "Bind backspace to `backward-delete-char'."
  (define-key ido-mode-map '[backspace] 'backward-delete-char)
  (define-key ido-mode-map '[(meta backspace)] 'backward-kill-word))

(if ido-xemacs
    (add-hook 'ido-define-mode-map-hook 
	      'ido-xemacs-backspacekey))

;;; ICOMPLETE TYPE CODE

(defun ido-exhibit ()
  "Find matching files and display a list in the minibuffer.
Copied from `icomplete-exhibit' with two changes:
1. It prints a default file name when there is no text yet entered.
2. It calls my completion routine rather than the standard completion."

  (if (= ido-use-mycompletion-depth (minibuffer-depth))
      (let ((contents (buffer-substring (point-min)(point-max)))
	    (buffer-undo-list t))
	(save-excursion
	  (goto-char (point-max))
                                        ; Register the end of input, so we
                                        ; know where the extra stuff
                                        ; (match-status info) begins:
	  (if (not (boundp 'ido-eoinput))
	      ;; In case it got wiped out by major mode business:
	      (make-local-variable 'ido-eoinput))
	  (setq ido-eoinput (point))

	  ;; Handle explicit directory changes
	  (if (and
	       (eq ido-cur-item 'file)
	       (> (length contents) 1))
	      (cond 
	       ((string-equal contents "~/")
		(if (ido-set-current-home)
		    (progn
		      (setq ido-exit 'refresh)
		      (exit-minibuffer))))

	       ((and (memq system-type '(windows-nt ms-dos))
		     (string-equal (substring contents 1) ":/"))
		(setq ido-current-directory (file-name-directory contents))
		(setq ido-exit 'refresh)
		(exit-minibuffer))

	       ((and (string-equal ido-current-directory "/")
		     (string-match "..:$" contents)) ;; Ange-ftp 
		(setq ido-current-directory (concat "/" contents "/"))
		(setq ido-exit 'refresh)
		(exit-minibuffer))
		
	       ((string-equal (substring contents -2 -1) "/")
		(let ((c (string-to-char (substring contents -1))))
		  (if (= c ?/)
		      (setq ido-current-directory 
			    (if (memq system-type '(windows-nt ms-dos))
				(expand-file-name "/" ido-current-directory)
			      "/"))
		    (setq ido-current-directory 
			  (if (= (length contents) 2)
			      "/"
			    (concat ido-current-directory (substring contents 0 -1))))
		    (setq unread-command-events (cons c unread-command-events)))
		  (setq ido-exit 'refresh)
		  (exit-minibuffer)))))

	  ;; Update the list of matches
	  (setq ido-text contents)
	  (ido-set-matches)
	  (setq ido-rescan t)
	  (ido-set-common-completion)

	  ;; Insert the match-status information:
	  (insert-string
	   (ido-completions 
	    contents
	    minibuffer-completion-table
	    minibuffer-completion-predicate
	    (not minibuffer-completion-confirm)))
	  ))))

(defun ido-completions
  (name candidates predicate require-match)
  "Return the string that is displayed after the user's text.
Modified from `icomplete-completions'."
  
  (let ((comps ido-matches)
	;; "-determined" - only one candidate
        (open-bracket-determined (if require-match "(" "["))
        (close-bracket-determined (if require-match ")" "]"))
	;; "-prospects" - more than one candidate
        (open-bracket-prospects "{")
        (close-bracket-prospects "}")
	first)

    (if (and ido-use-faces  comps)
	(progn
	  (setq first (car comps))
	  (setq first (format "%s" first))
	  (put-text-property 0 (length first) 'face
			     (if (= (length comps) 1)
				 'ido-only-match-face
			       'ido-first-match-face)
			     first) 
	  (setq comps  (cons first (cdr comps)))))

    (cond ((null comps)
	   (if ido-report-no-match
	       (format " %sNo match%s"
		       open-bracket-determined close-bracket-determined)
	     ""))

	  ((null (cdr comps))		;one match
	   (concat (if (and (> (length (car comps))
			       (length name)))
		       (concat open-bracket-determined
			       ;; when there is one match, show the 
			       ;; matching file name in full
			       (car comps)
			       close-bracket-determined)
		     "")
		   (if (not ido-use-faces) " [Matched]")))
	  (t				;multiple matches
	   (let* (
		  ;;(most (try-completion name candidates predicate))
		  (most nil)
		  (most-len (length most))
		  most-is-exact
		  first
		  (alternatives
		   (apply
		    (function concat)
		    (cdr (apply
			  (function nconc)
			  (mapcar '(lambda (com)
				     (if (= (length com) most-len)
					 ;; Most is one exact match,
					 ;; note that and leave out
					 ;; for later indication:
					 (progn
					   (setq most-is-exact t)
					   ())
				       (list ","
					     (substring com
							most-len))))
				  comps))))))

	     (concat

	      ;; put in common completion item -- what you get by
	      ;; pressing tab
	      (if (> (length ido-common-match-string) (length name))
		  (concat open-bracket-determined
			  (substring ido-common-match-string 
				     (length name))
			  close-bracket-determined))
	      ;; end of partial matches...

	      ;; think this bit can be ignored.
	      (and (> most-len (length name))
		   (concat open-bracket-determined
			   (substring most (length name))
			   close-bracket-determined))
	      
	      ;; list all alternatives
	      open-bracket-prospects
	      (if most-is-exact
		  (concat "," alternatives)
		alternatives)
	      close-bracket-prospects))))))

(defun ido-minibuffer-setup ()
  "Set up minibuffer for `ido-find-file'.
Copied from `icomplete-minibuffer-setup-hook'."
  (if (ido-entryfn-p)
      (progn

	(make-local-hook 'pre-command-hook)
	(add-hook 'pre-command-hook
		  'ido-pre-command
		  nil t)
	(make-local-hook 'post-command-hook)
	(add-hook 'post-command-hook
		  'ido-post-command
		  nil t)
	
	(run-hooks 'ido-minibuffer-setup-hook))))

(defun ido-pre-command ()
  "Run before command in `ido-find-file'."
  (ido-tidy))

(defun ido-post-command ()
  "Run after command in `ido-find-file'."
  (ido-exhibit))

(defun ido-tidy ()
  "Remove completions display, if any, prior to new user input.
Copied from `icomplete-tidy'."

  (if (= ido-use-mycompletion-depth (minibuffer-depth))
      (if (and (boundp 'ido-eoinput)
	       ido-eoinput)
      
	  (if (> ido-eoinput (point-max))
	      ;; Oops, got rug pulled out from under us - reinit:
	      (setq ido-eoinput (point-max))
	    (let ((buffer-undo-list buffer-undo-list )) ; prevent entry
	      (delete-region ido-eoinput (point-max))))
    
	;; Reestablish the local variable 'cause minibuffer-setup is weird:
	(make-local-variable 'ido-eoinput)
	(setq ido-eoinput 1))))

(defun ido-entryfn-p ()
  "Return non-nil if `this-command' shows we are using `ido-read-internal'."
  (boundp 'ido-prepost-hooks))

(defun ido-summary-buffers-to-end ()
  "Move the summaries to the end of the buffer list.
This is an example function which can be hooked on to
`ido-make-buffer-list-hook'.  Any buffer matching the regexps
`Summary' or `output\*$'are put to the end of the list."
  (let ((summaries (delq nil (mapcar 
			      (lambda (x) 
				 (if (or 
				      (string-match "Summary" x)
				      (string-match "output\\*$" x))
				     x))
			      ido-temp-list))))
    (ido-to-end summaries)))

;;; ido.el ends here
