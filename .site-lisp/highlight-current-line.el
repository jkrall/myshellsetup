;;; highlight-current-line.el --- highlight line where the cursor is

;; Copyright (c) 1997 Christoph Conrad

;; Author: Christoph Conrad <Christoph.Conrad@post.rwth-aachen.de>
;; Created: 10 Oct 1997
;; Version: 0.5
;; Keywords: faces

;; This file is not yet part of any Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Highlights the line the cursor is in. You can change colors of foreground
;; (text) and background. Highlighting is (currently) switched on in ALL
;; buffers including minibuffers. Default behaviour is to set only background
;; color, so that font-lock fontification colors remain visible (syntax
;; coloring).  See functions `highlight-current-line-on',
;; `highlight-current-line-set-fg-color',
;; `highlight-current-line-set-bg-color'. There's a special color "none"
;; defined to set no color.

;; You can select whether the whole line (from left to right window border)
;; is marked or only the really filled parts of the line (from left window
;; border to the last char in the line). The second behaviour is suitable if
;; its important for you to see trailing spaces or tabs in a line. See
;; function `highlight-current-line-whole-line-on'.

;; You can ignore buffers, whose buffer-name match some regular expression,
;; so they never get highlighted. Some buffers are ignored by default, see
;; variable `highlight-current-line-ignore-regexp'. You can extend or
;; redefine this regexp. This works together with the default ignore function
;; `highlight-current-line-ignore-function'. You can redefine this function
;; to implement your own criterias.

;;; People which made contributions or suggestions:

;; - Jari Aalto		<jari.aalto@ntc.nokia.com>
;; - Shawn Ostermann	<sdo@picard.cs.OhioU.Edu>
;; - Peter Ikier	<p_ikier@infoac.rmi.de>
;;   Many thanks to him for the idea. He liked this behaviour in another
;;   editor ("Q").

;;; Installation:

;; e.g. in .emacs
;; (require 'highlight-current-line)
;; ;; If you want to mark only to the end of line:
;; (highlight-current-line-whole-line-on nil)
;; ;; switch highlighting on
;; (highlight-current-line-on t)
;;
;; ;; If you want to change default-foreground/background color add something
;; ;; like:
;; (highlight-current-line-set-fg-color "red")
;; (highlight-current-line-set-bg-color "white")
;; ;; There's a special color "none" defined to set no color.
;;
;; ;; Ignore no buffer
;; (setq highlight-current-line-ignore-regexp nil) ; or set to ""
;; ;; alternate way to ignore no buffers
;; (fmakunbound 'highlight-current-line-ignore-function)
;; ;; Ignore more buffers
;; (setq highlight-current-line-ignore-regexp
;;      (concat "Dilberts-Buffer\\|"
;;	      highlight-current-line-ignore-regexp))
;;
;; Put a copy of highlight-current-line.el/.elc into some path of
;; `load-path'. To show `load-path': <C-h v> load-path RET
;;

;;; Troubleshooting:

;; - Q: I do not see matching parens from paren.el any more!
;; - A: Check the colors from highlight-current-line or from show-paren-face
;;   and choose some combination which works together.

;;; ToDo:

;; - provide overlay priorities
;;   (overlay-put highlight-current-line-overlay 'priority 60)
;; - better way to switch off 'ignore buffer'
;; - face fore/backgroundcolor depending on major-mode
;; - better way to detect xemacs
;; - usage of custom-package. Suggested by Jari Aalto.
;; - some suggestions for default keys
;; - highlight-current-line as minor mode. Suggested by Shawn Ostermann.

;;; Change log:

;; 18 Oct 1997 - v0.5:
;; - GNU General Public License
;; - ignore user-definable buffernames which are ignored for
;;   highlighting. Suggested by Jari Aalto.
;; - works with XEmacs, at least version 19.15. Mark whole line doesnt work
;;   yet. Suggested by Jari Aalto.
;; - highlight-current-line-set-fg/bg-color understand "none" as color
;; - overlay-put moved from post-command-hook to initialization-code
;; - version-variable: `highlight-current-line-version'. Always
;;   "major.minor". Suggested by Jari Aalto.

;; 11 Oct 1997 - v0.4:
;; - Possibility to highlight whole line (from left to right windowborder) or
;;   only from left window border to the last char in the line.
;;
;; 20 Aug 1997 - v0.3:
;; - First public released version.

;;; Code:


;; Initialization for XEmacs

;; XEmacs needs overlay emulation package.
;; Old XEmacs won't have the package and we must quit.
(eval-and-compile
  (if (boundp 'xemacs-logo)
      (if (not (load "overlay" 'noerr))
	  (error "\
highlight-current-line.el: ** This package requires overlays. Abort."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; can be set by user

(defvar highlight-current-line-ignore-regexp
  (concat
   "Faces\\|Colors\\|Minibuf"
   ;; for example:
   ;; "\\|RMAIL.*summary\\|\\*Group\\|\\*Summary"
   )
  "*Regexps for buffers to ignore.
Used by `highlight-current-line-ignore-function'.")


(defvar highlight-current-line-whole-line t
  "*If non-nil, then mark up to end-of-line. If nil, mark up to window-border.
Use `highlight-current-line-whole-line-on' to set this value."
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; should not be set by user

(defvar highlight-current-line-version "0.5"
  "Version number." )

(defvar highlight-current-line-overlay
  ;; Dummy initialization
  (make-overlay 1 1)
  "Overlay for highlighting."
  )

;; Create default-face for highlighting.
(make-face 'highlight-current-line-face)

(defvar highlight-current-line-no-color (if (boundp 'xemacs-logo)
					  '[]
					  nil)
  "'color' value that represents \"no color\".")

(set-face-foreground 'highlight-current-line-face
		     highlight-current-line-no-color)
(set-face-background 'highlight-current-line-face
		     "wheat")

;; Set face-property of overlay
(overlay-put highlight-current-line-overlay
	     'face 'highlight-current-line-face)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Decide whether to highlight the buffer.
(defun highlight-current-line-ignore-function  ()
  "Default ignore function. If evaluates to nil, buffer can be highlighted."
  (if (or (equal "" highlight-current-line-ignore-regexp)
	  (not highlight-current-line-ignore-regexp))
      nil
       (string-match highlight-current-line-ignore-regexp (buffer-name))))


;; Set foregroundcolor of cursor-line.
(defun highlight-current-line-set-fg-color (color)
  "Set foregroundcolor for highlighting cursor-line.
Key: \\[highlight-current-line-set-fg-color]"
  (interactive "sForeground color (\"none\" means no color): ")
  (if (equal "none" color)
      (setq color highlight-current-line-no-color))
  (set-face-foreground 'highlight-current-line-face color)
  )


;; Set backgroundcolor of cursor-line.
(defun highlight-current-line-set-bg-color (color)
  "Set backgroundcolor for highlighting cursor-line.
Key: \\[highlight-current-line-set-bg-color]"
  (interactive "sBackground color (\"none\" means no color): ")
  (if (equal "none" color)
      (setq color highlight-current-line-no-color))
  (set-face-background 'highlight-current-line-face color)
  )


;; Post-Command-Hook for highlighting
(defun highlight-current-line-hook ()

  (if (and (fboundp 'highlight-current-line-ignore-function)
	   (not (highlight-current-line-ignore-function)))

	  (let ((current-point (point)))

	    ;; Set overlay
	    (move-overlay highlight-current-line-overlay
			  (progn (beginning-of-line)
				 (point))
			  (progn (if highlight-current-line-whole-line
				     (forward-line 1)
				   (end-of-line))
				 (point))
			  (current-buffer))

	    (goto-char current-point))))


;; Enable/Disable Highlighting
(defun highlight-current-line-on (&optional on-off)
  "Switch highlighting of cursor-line on/off.
Key: \\[highlight-current-line-on]"
  (interactive (list (y-or-n-p "Highlight line with cursor? ")))

  (if on-off
      (add-hook 'post-command-hook 'highlight-current-line-hook)
    (remove-hook 'post-command-hook 'highlight-current-line-hook)
    (delete-overlay highlight-current-line-overlay))
  )

;; Enable/Disable whole line marking
(defun highlight-current-line-whole-line-on (&optional on-off)
  "Switch highlighting of whole line on/off.
Key: \\[highlight-current-line-whole-line-on]"
  (interactive (list (y-or-n-p "Highlight whole line? ")))

  (setq highlight-current-line-whole-line on-off)
  )

(provide 'highlight-current-line)

;;; highlight-current-line.el ends here
