;;; eshell --- an Emacs command shell

;; Copyright (C) 1999 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Created: 27 Apr 1999
;; Version: 1.5
;; Keywords: processes
;; X-URL: http://oneworld.new-era.com/johnw/emacs.html

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A shell is a layer which metaphorically surrounds the kernel, or
;; heart of an operating system.  This kernel can be seen as an engine
;; of pure functionality, waiting to serve, while the user programs
;; take advantage of that functionality to accomplish their purpose.

;; The shell's role is to make that functionality accessible to the
;; user in an unformed state.  Very roughly, it associates kernel
;; functionality with textual commands, allowing the user to interact
;; with the operating system via linguistic constructs.  Process
;; invocation is perhaps the most significant form this takes, using
;; the kernel's "fork" and "exec" functions.

;; Other programs also interact with the functionality of the kernel,
;; but these user applications typically offer a specific range of
;; functionality, and thus are not classed as "shells" proper.  (What
;; they lose in quiddity, they gain in rigidity).

;; Emacs is also a user application, but it does make the
;; functionality of the kernel accessible through an interpreted
;; language -- namely, Lisp.  For that reason, there is little
;; preventing Emacs from serving the same role as a modern shell.  It
;; too can manipulate the kernel in an unpredetermined way to cause
;; system changes.  All it's missing is the shell-ish linguistic
;; model.

;; Enter eshell.  Eshell translates "shell-like" syntax into Lisp, in
;; order to exercise the kernel in the same manner as typical system
;; shells.  There is a fundamental difference here, however, although
;; it may seem subtle at first..

;; Shells like csh and Bourne shell were written several decades ago,
;; in different times, under more restrictive circumstances.  This
;; confined perspective shows itself in the paradigm used by nearly
;; all command-line shells since.  They are linear in conception, byte
;; stream-based, sequential, and confined to movement within a single
;; host machine.

;; Emacs, on the other hand, is more than just a limited translator
;; that can invoke subprocesses and redirect file handles.  It also
;; manages character buffers, windowing frames, network connections,
;; registers, bookmarks, processes, etc.  In other words, it's a very
;; multi-dimensional enviroment, within which eshell emulates a highly
;; linear methodology.

;; Taking a moment, let's look at how this could affect the future of
;; a shell allowed to develop in such a wider field of play:
;;
;; - There is no reason why directory movement should be linear, and
;;   confined to a single filesystem.  Emacs, through w3 and
;;   ange-ftp, has access to the entire Web.  Why not allow a user to
;;   cd to multiple directories simultaneously, for example?  It
;;   might make some tasks easier, such as diff'ing files separated
;;   by very long pathnames.
;; 
;; - Data sources are avaliable from anywhere Emacs can derive
;;   information from: not just from files or the output of other
;;   processes.
;; 
;; - Multiple shell invocations all share the same environment -- even
;;   the same process list!  It would be possible to have "process
;;   views", so that one buffer is watching standard output, another
;;   standard error, and another the result of standard output grep'd
;;   through a regular expression..
;; 
;; - It is not necessary to "leave" the shell, losing all input and
;;   output history, environment variables, directory stack, etc.
;;   Emacs could save the contents of your eshell environment, and
;;   restore all of it (or at least as much as possible) each time you
;;   restart.  This could occur automatically, with requiring complex
;;   initialization scripts.
;; 
;; - Typos occur all thof e time; many of them are repeats of common
;;   errors, such as "dri" for "dir".  Since executing non-existent
;;   programs is rarely the intention of the user, eshell could prompt
;;   for the replacement string, and then record that in a database of
;;   known mispellings.
;; 
;; - Emacs' register and bookmarking facilities can be used for
;;   remembering where you've been, and what you've seen -- to varying
;;   levels of persistence.  They could perhaps even be tied to
;;   specific "moments" during eshell execution, which would include
;;   the environment at that time, as well as other variables.
;;   Although this would require functionality orthogonal to Emacs'
;;   own bookmarking facilities, the interface used could be made to
;;   operate very similarly.
;;
;;  This presents a brief idea of what the fuller dimensionality of an
;;  Emacs shell could offer.  It's not just the language of a shell
;;  that determines how it's used, but also the Weltanschauung
;;  underlying its design -- and which is felt behind even the
;;  smallest feature.  I would hope the freedom provided by using
;;  Emacs as a parent environment will invite rich ideas from others.
;;  It certainly feels as though all I've done so far is to tie down
;;  the horse, so to speak, so that he will run at a man's pace.

;;; Usage:

;; Basic usage

;; Basically, eshell is used just like shell mode (M-x shell).  The
;; keystrokes for navigating the buffer, and accessing the command
;; history, are identical.  Unlike shell mode, however, eshell mode's
;; governing process is Emacs itself.  With shell mode, an inferior
;; shell process is executed that communicates with Emacs via comint
;; -- a mode for handling sub-process interaction.  Eshell mode, on
;; the other hand, is a true, native Emacs shell.  No subprocess are
;; invoked except for the ones you request at the eshell prompt.

;; After entering a command at the prompt, use RET to invoke the
;; command.  If there is a command on disk, it will be run just like
;; in a normal shell.  If there is no command by that name on disk,
;; but a Lisp function with that name is defined, the lisp function
;; will be called instead, using the arguments that you passed on the
;; command line.  (It is possible to have Emacs always prefer Lisp
;; functions to disk command, by setting
;; `eshell-prefer-lisp-functions' to a non-nil value).

;; As each command is executed, a mark is pushed at the location where
;; RETURN was pressed.  This enables you to repeat the last command by
;; typing: C-x C-x (switch mark and point), RET.  Alternately, you can
;; use a command-based history mechanism, by saying !! to repeat the
;; last command, or !text to repeat the last command which begin with
;; that text.

;; Setting the path

;; One of the first things you may notice is that there's no way to
;; run commands from the current directory, if your PATH variable
;; doesn't already include ".".  To set the path so that Emacs will
;; recognize the change, use this command:
;;
;;    setq exec-path (cons "." exec-path)
;;
;; At the moment, setting exec-path doesn't not cause PATH to be
;; changed; nor will setting PATH effect the executable search path
;; that Emacs uses for subprocesses.

;; Output redirection

;; The output from a command, besides being sent into a pipe, can also
;; be redirected.  Redirection can point to a file, by using the file
;; name, or to a buffer or process, by using the Emacs syntax #<buffer
;; NAME> and #<process NAME>.  To easily insert a buffer or process
;; name, using the keystrokes C-c b and C-c p.  It will prompt you
;; from among all possible names at the minibuffer.

;; There is no limit to the number of redirection targets you may
;; specify.  All targets will receive the same data.  So to redirect a
;; directory listing to both a file on disk, and a buffer, you just
;; say:
;;
;;     ls > #<buffer *scratch*> > my-file
;;
;; You can also redirect output to Lisp variables, Lisp functions
;; (which will be repeatedly called with the string data, as it
;; becomes available) or "/dev/null", which is a pseudo-device that
;; gobbles up data into the Bit Bucket.  Here are more examples:
;;
;;     ls > 'my-lisp-variable
;;     ls > 'my-lisp-function
;;     ls > '(lambda (string) (eshell-echo string))

;; To redirect error output from a Lisp command (right now, Emacs does
;; not permit fine-grained enough control to separate out standard
;; output and standard error from asynchronous processes), use the
;; Bourne-ish "2>" syntax:
;;
;;     ls blah 2> errors

;; Pipes

;; To feed the output from a Lisp command or disk command to another
;; disk command (NOTE: Lisp commands will simply ignore the input),
;; using the pipe syntax:
;;
;;     ls | egrep *.el | wc -l

;; Even if you are redirecting output to a file, the output from the
;; command will still go into the pipe unless `eshell-always-to-pipe'
;; is nil.  This obviates the need for "tee" in many situations.
;;
;;     ls | egrep *.el > results-of-egrep | wc -l

;; Sequenced commands

;; Multiple commands may be executed in series by separating them with
;; ";".  The output from one command may be sent to another using the
;; pipe character "|".  The only restriction with pipes is that lisp
;; command cannot be on the receiving side of a pipe.  Only disk
;; commands can.  (Perhaps this will be added in the future).
;;
;;     cd ~; rm my-tar-file.gz; tar cvzf my-tar-file.gz .

;; Background processes

;; Since all processes invoke by eshell are asynchronous, the meaning
;; of "&" at the end of a command takes on a slightly different
;; meaning.  In this case, it just means that the process is meant to
;; run in the "background", and that eshell won't wait for it to
;; finish before outputting another prompt.

;; Accessing variables

;; Variables, both Lisp and from the system environment, can be
;; accessed using the syntax "$NAME".  To make it easier on Lisp
;; programmers, "-" is considered to be part of a legal variable name.
;; If this becomes upsetting to any diehard shell people, you can
;; change the regexp specified by `eshell-variable-name-regexp'.

;; To disambiguate which characters are meant to be part of a variable
;; name, and which aren't, use $<NAME>:
;;
;;    ls $<SOME_VAR>-tar.gz

;; Variables can be indexed, using the syntax $NAME[index].  "index"
;; can be either a numeric value, implying an array index, or a
;; string, which implies an associative lookup within an alist using
;; `assoc'.  If the variable's value is not a list, but a string, the
;; string will be separated by `split-string' before doing the access.
;; The default separation regexp is whitespace.
;;
;; To specify a different separation regexp, specify it as the first
;; array index:
;;
;;     $NAME["[0-9]+" 10]       or     $CDPATH[: 2]

;; Then there "pseudo-variables", whose values can be interpolated
;; into a string just like a normal variable, but which are actually
;; eshell and Lisp commands:
;;
;;     ls $VAR/some-element/$(+ 1 2)/nother-element/${echo blah}/hello
;;
;; This will interpolate the value of VAR, the result of the Lisp form
;; (+ 1 2), and the value of the synchronous eshell command "echo
;; blah".

;; History mechanism

;; The history mechanism used is identical to that provided by comint
;; (the code is the same).  Please refer to that documentation in the
;; Emacs manual.

;; Using subcommands

;; Subcommands allow you to make buffer-relative changes that will not
;; affect your eshell buffer.  Changing directory, for example.  The
;; syntax is:
;;
;;    {cd $home; ls} > output-here
;;
;; The reason that parens weren't used is because they are reserved
;; for Lisp evaluation.

;; Using Lisp forms directly

;; You can embed a lisp form anywhere on your command line.  You can
;; even interpolate the results of a Lisp evalution by using the
;; syntax "$form".  You can eval quoted Lisp objects, forms and
;; backquoted expressions.

;; Variable aliases

;; To be documented.

;; Command aliases

;; Commands may be quite extensively aliased, to provide any kind of
;; behavior desired.  This is a somewhat complicated subject, however,
;; and the best that I can offer at the moment is to look at the
;; aliases provided in `eshell-command-aliases-list', and the
;; documentation there.  If you only desire is to make a simple
;; command alias, however, such as "alias ll ls -l", then just enter
;; that text, and it will create the proper alias for you.

;; These are really a bit complex at the moment, because they need to
;; interact with eshell's output handling scheme.  See
;; `eshell-ls-alias' and `eshell-cd-alias' for coding examples.  Good
;; luck.

;; Preferring Lisp commands and variables

;; Normally, filesystem command names are preferred to Lisp function
;; names, and environment variables are preferred to Lisp variables.
;; You can change this lookup order either by defining an alias, or by
;; setting the variables `eshell-prefer-lisp-functions' and/or
;; `eshell-prefer-lisp-variables' to a non-nil value.

;; Extending the behavior of eshell

;; In the future, there will be many more hooks and filter functions
;; provided then there are currently.  Also, nearly even detail of
;; syntax and user interaction will be placed in a customizable
;; variable.  Even the code itself has been broken into fairly small
;; pieces, to allow you to use `defadvice' to your heart's content.
;; But things aren't quite there yet in all places.  The intention is
;; for eshell is to be as customizable as possible, without
;; introducing impossible ambiguities.

;;; New in 1.5:

;; - eshell now uses comint directly, rather than duplicating that
;;   code.  For this to work, you will need an updated copy of comint,
;;   which can be found at
;;       http://oneworld.new-era.com/johnw/comint.el
;;   Or ask the author to send you a copy!
;;
;; - optimized handling of output from a subprocess.  In cases where
;;   both standard output and standard error are both headed for the
;;   *eshell* buffer (which is most of the time), the comint filter is
;;   used directly, bypassing eshell's redirection code.
;;
;; - there is a new user variable, `eshell-visual-commands'.  If any
;;   command name matches a member of this list, it will be executed
;;   using "term", and started in a new buffer.  When that process
;;   exits -- and if you were viewing that buffer at the time -- it
;;   will return you to eshell.
;;
;; - fixed "/dev/null".  It now dumps output once again (before, it
;;   would give an error about trying to change the value of the
;;   constant symbol "nil").
;;
;; - fixed a problem with subcommands that was causing "echo
;;   ${whoami}" not to work.
;;
;; - implemented a simple version of "history" that mimics the
;;   behavior of bash's same command.  None of the command switches
;;   are supported, however, nor can an alternate history file be
;;   specified.  Yet.
;;
;; - if `eshell-input-ring-file-name' is non-nil (and it now defaults
;;   to "~/.history"), eshell will write out the current command
;;   history whenever the eshell buffer is killed.
;;
;; - the "exit" alias will bury the eshell buffer; but it does not
;;   kill it (for the time being, there are some problems with trying
;;   to do that, while at the same time keep everything sane).
;;
;; - after a call to `eshell-print', the display is now refreshed --
;;   with consquent result that some of the lisp functions are now
;;   visibly slower, although you can at least now see that they're
;;   doing something.  Setting `eshell-refresh-interval' to a higher
;;   value will make the functions run faster, but it will also make
;;   them seem a little choppier (no frequent updates).  Setting this
;;   to a really high number will match the functionality of 1.4.
;;
;; - if a "foreground" process is active, pressing RETURN will send
;;   the current string to that process; if there are multiple
;;   foreground processes active, the user will be prompted for which
;;   one.  Note that eshell echos locally, so the app you're using
;;   (such as telnet) may have to turn echoing off.
;;
;; - executables are searched for along the PATH, not the `exec-path'.
;;   Also, specifying a relative executable name now works (before,
;;   saying "src/ls", even if it was there, would not work).
;;
;; - added an alias for "export" which behaves just as in bourne
;;   shell.  That is, you can now add to the path by saying:
;;
;;     export PATH=$PATH:/usr/local/bin"
;;
;; - fixed a bug which made it impossible to say "echo $exec-path",
;;   and was causing $_ to be inaccessible.
;;
;; - "ls -l" (the alias version, written in Lisp) now shows symbolic
;;   link targets.  "ls -r" correctly reverses the file list.  "ls -t"
;;   sorts by mod time.  Ganging up options ("ls -ltr") now also
;;   works.
;;
;; - before calling start-process, use `expand-file-name' on the
;;   program name, so that relative paths are seen as absolute
;;
;; - looking up the command interpretor got broken again in 1.4
;;
;; - fixed problem where you couldn't cd to a directory with a numeric
;;   name
;;
;; - `eshell-kill-alias' was reversing its arguments, so that "kill -9
;;   PROC" wasn't working.
;;
;; - fixed a problem in `eshell-argument-syntax-list', that was
;;   causing process references (e.g., #<process ispell>) to cause a
;;   syntax error.
;;
;; - if output continues to generate from a subprocess, and you move
;;   point to another window or frame, eshell will now scroll to the
;;   end of the buffer.  In 1.4, it would freeze the point of display
;;   at the location you were last at when leaving the window.
;;
;; - protected a reference to `transient-mark-mode' with a call to
;;   `boundp', in case it's not bound yet.
;;
;; - "date" now calls (current-time-string).  If you want the other
;;   functions of the date command, call it with an explicit path,
;;   such as /bin/date.  In the future, I'll make "date" an alias, and
;;   just call the real "date" if the alias doesn't recognize any of
;;   the command line switches.
;;
;; - beyond these features and bugs, there were several other, smaller
;;   bugs fixed.  Thanks to everyone who submitted feedback, and
;;   provided test cases.  Most everything else on my TODO list is
;;   either a major bug, requiring redesign, or a new feature.

;;; Code:

(require 'comint)
(require 'pp)
(require 'env)

(defconst eshell-version "1.5"
  "This version of eshell.")

(defgroup eshell nil
  "emacs command interpretor."
  :group 'applications)

;; User Variables:

(defcustom eshell-load-hook nil
  "*A hook that gets run after \"eshell.el\" has been loaded."
  :type 'hook
  :group 'eshell)

(defcustom eshell-mode-hook nil
  "*A hook that gets run when eshell mode is entered."
  :type 'hook
  :group 'eshell)


(defcustom eshell-buffer-name "*eshell*"
  "*The name to be used for the Emacs Shell buffer."
  :type 'string
  :group 'eshell)

(defcustom eshell-display-banner-p t
  "*If non-nil, display a simple banner message on startup."
  :type 'boolean
  :group 'eshell)


(defcustom eshell-prefer-lisp-functions nil
  "*If non-nil, prefer Lisp functions to those on disk."
  :type 'boolean
  :group 'eshell)

(defcustom eshell-prefer-lisp-variables nil
  "*If non-nil, prefer Lisp variables to environment variables."
  :type 'boolean
  :group 'eshell)


(defcustom eshell-expand-filename-regexp "\\(^\\(/:\\|~\\)\\|[*?]\\)"
  "*If a filename matches this regexp, expand it.
This involves first calling `expand-file-name', and then resolving any
globbing patterns."
  :type 'regexp
  :group 'eshell)

(defcustom eshell-prompt-regexp "^[^#$%>\n]*\\$ *"
  "*A regexp which fully matches your eshell prompt.
This setting is important, since it affects how eshell will interpret
the lines that are passed to it.  To output a prompt, just set the
value of the variable \"prompt\".  But don't forget to set this regexp
as well!"
  :type 'regexp
  :group 'eshell)

(defcustom eshell-variable-name-regexp "[A-Za-z0-9_-]+"
  "*A regexp identifying what constitutes a variable name reference.
Note that this only applies for $NAME.  If the syntax $<NAME> is used,
then NAME can contain any character, including angle brackets, if they
are quoted using a backslash."
  :type 'regexp
  :group 'eshell)

(defvar eshell-font-lock-keywords
  '((eval . (cons eshell-prompt-regexp 'font-lock-warning-face))
    ("[ \t]\\([+-][^ \t\n]+\\)" 1 font-lock-comment-face)
    ("^[^ \t\n]+:.*" . font-lock-string-face)
    ("^\\[[1-9][0-9]*\\]" . font-lock-string-face))
  "Additional expressions to highlight in EShell mode.")


(defcustom eshell-prefer-to-shell nil
  "*If non-nil, M-! and M-| use eshell instead of shell."
  :set (lambda (symbol value)
         (if value
             (progn
               (define-key global-map "\M-!" 'eshell-command)
               (define-key global-map "\M-|" 'eshell-command-on-region))
           (define-key global-map "\M-!" 'shell-command)
           (define-key global-map "\M-|" 'shell-command-on-region))
         (setq eshell-prefer-to-shell value))
  :type 'boolean
  :require 'eshell
  :group 'eshell)

(defcustom eshell-push-marks
  (not (and (boundp 'transient-mark-mode)
            (symbol-value 'transient-mark-mode)))
  "*If non-nil, push a mark at every point of command invocation.
Not recommended if transient mark mode is non-nil."
  :type 'boolean
  :group 'eshell)

(defcustom eshell-point-after-result t
  "*If non-nil, keep point after command result.
Otherwise, it will stay at the end of the command just sent, which
mimics the behavior of Plan9's 9term."
  :type 'boolean
  :group 'eshell)

(defcustom eshell-buffer-shorthand nil
  "*If non-nil, a symbol name can be used for a buffer in redirection.
If nil, redirecting to a buffer requires buffer name syntax.  If this
variable is set, redirection directly to Lisp symbols will be
impossible.

Example:

    echo hello > '*scratch*  ; works if `eshell-buffer-shorthand' is t
    echo hello > #<buffer *scratch*>  ; always works"
  :type 'boolean
  :group 'eshell)

(defcustom eshell-always-to-pipe t
  "*If non-nil, always send standard output to the next leg of a pipe.
If nil, redirection can grab all of the output from the earlier
process."
  :type 'boolean
  :group 'eshell)

(defcustom eshell-escape-control-x t
  "*If non-nil, allow C-x to be handled by Emacs key in visual buffers.
See the variable `eshell-visual-commands'.  If this variable is set to
nil, C-x will send that control character to the invoked process."
  :type 'boolean
  :group 'eshell)

(defcustom eshell-pwd-convert-function 'file-truename
  "*A function to be used to convert output from Lisp's `pwd' function.
Setting this variable `file-truename' mimics the behavior of most UNIX
operating systems.  `expand-file-name' and `identity' or other
possible options."
  :type 'function
  :group 'eshell)

(put 'eshell-pwd-convert-function 'risky-local-variable t)

(defcustom eshell-cd-shows-directory nil
  "*If non-nil, using `cd' will report the directory it changes to."
  :type 'boolean
  :group 'eshell)

(defcustom eshell-input-ignoredups nil
  "*If non-nil, don't add input matching the last on the input ring.
This mirrors the optional behavior of bash.

This variable is buffer-local."
  :type 'boolean
  :group 'eshell)

(defcustom eshell-password-prompt-regexp
  "\\(\\([Oo]ld \\|[Nn]ew \\|Kerberos \\|'s \\|login \\|^\\)[Pp]assword\\|pass phrase\\|Enter passphrase\\)\
\\( for [^@ \t\n]+@[^@ \t\n]+\\)?:\\s *\\'"
  "*Regexp matching prompts for passwords in the inferior process.
This is used by `comint-watch-for-password-prompt'."
  :type 'regexp
  :group 'eshell)


(defcustom eshell-number-of-handles 3
  "*The number of file handles that eshell supports.
Currently this is standard input, output and error.  But even all of
these Emacs does not currently support with asynchronous processes
\(which is what eshell uses so that you can continue doing work in
other buffers) ."
  :type 'integer
  :group 'eshell)

(defcustom eshell-output-handle 1
  "*The index of the standard output handle."
  :type 'integer
  :group 'eshell)

(defcustom eshell-error-handle 2
  "*The index of the standard error handle."
  :type 'integer
  :group 'eshell)

(defcustom eshell-refresh-interval 10
  "*The number of `eshell-print's before refreshing the display."
  :type 'integer
  :group 'eshell)


(defcustom eshell-input-filter
  (function
   (lambda (str)
     (not (string-match "\\`\\s *\\'" str))))
  "*Predicate for filtering additions to input history.
Takes one argument, the input.  If non-nil, the input may be saved on
the input history list.  Default is to save anything that isn't all
whitespace."
  :type 'function
  :group 'eshell)

(put 'eshell-input-filter 'risky-local-variable t)

(defcustom eshell-input-filter-functions nil
  "*Functions to call before input is processed.
These functions get one argument, a string containing the text to send.

This variable is buffer-local."
  :type '(choice (const :tag "No filters" nil)
                 (repeat function))
  :group 'eshell)

(put 'eshell-input-filter-functions 'risky-local-variable t)

(defcustom eshell-output-filter-functions
  '(comint-postoutput-scroll-to-bottom
    comint-watch-for-password-prompt)
  "*Functions to call before output is displayed.
These functions are only called for output that is displayed
interactively, and not for output which is redirected."
  :type '(choice (const :tag "No filters" nil)
                 (repeat function))
  :group 'eshell)

(put 'eshell-output-filter-functions 'risky-local-variable t)

(defcustom eshell-preoutput-filter-functions nil
  "*Functions to call before output is inserted into the buffer.
These functions get one argument, a string containing the text to be
inserted.  They return the string as it should be inserted.

This variable is buffer-local."
  :type '(choice (const :tag "No filters" nil)
                 (repeat function))
  :group 'eshell)

(put 'eshell-preoutput-filter-functions 'risky-local-variable t)

(defcustom eshell-scroll-to-bottom-on-input nil
  "*Controls whether input to interpreter causes window to scroll.
If nil, then do not scroll.  If t or `all', scroll all windows showing buffer.
If `this', scroll only the selected window.

The default is nil.

See `comint-preinput-scroll-to-bottom'.  This variable is buffer-local."
  :type '(choice (const :tag "off" nil)
		 (const t)
		 (const all)
		 (const this))
  :group 'eshell)

(defcustom eshell-scroll-to-bottom-on-output nil
  "*Controls whether interpreter output causes window to scroll.
If nil, then do not scroll.  If t or `all', scroll all windows showing buffer.
If `this', scroll only the selected window.
If `others', scroll only those that are not the selected window.

The default is nil.

See variable `eshell-scroll-show-maximum-output' and function
`eshell-postoutput-scroll-to-bottom'.  This variable is buffer-local."
  :type '(choice (const :tag "off" nil)
		 (const t)
		 (const all)
		 (const this)
		 (const others))
  :group 'eshell)

(defcustom eshell-scroll-show-maximum-output nil
  "*Controls how interpreter output causes window to scroll.
If non-nil, then show the maximum output when the window is scrolled.

See variable `eshell-scroll-to-bottom-on-output' and function
`eshell-postoutput-scroll-to-bottom'.  This variable is buffer-local."
  :type 'boolean
  :group 'eshell)

(defcustom eshell-buffer-maximum-size 1024
  "*The maximum size in lines for comint buffers.
Comint buffers are truncated from the top to be no greater than this number, if
the function `comint-truncate-buffer' is on `eshell-output-filter-functions'."
  :type 'integer
  :group 'eshell)

(defcustom eshell-eol-on-send t
  "*Non-nil means go to the end of the line before sending input.
See `comint-send-input'."
  :type 'boolean
  :group 'eshell)


(defcustom eshell-file-name-quote-list
  '(?\| ?> ?\; ?\  ?$)
  "List of characters to quote when in a file name.
This variable is used to initialize `comint-file-name-quote-list' in
the eshell buffer.  The value may depend on the operating system or
shell.  Note that ?< will be added to this list when input redirection
is supported.

This is a fine thing to set in your `.emacs' file."
  :type '(repeat character)
  :group 'eshell)

(defcustom eshell-command-interpretor-max-length 256
  "*The maximum length of any command interpretor string, plus args."
  :type 'integer
  :group 'eshell)


(defcustom eshell-process-wait-seconds 0
  "*The number of seconds to delay waiting for a synchronous process."
  :type 'integer
  :group 'eshell)

(defcustom eshell-process-wait-milliseconds 50
  "*The number of milliseconds to delay waiting for a synchronous process."
  :type 'integer
  :group 'eshell)


(defcustom eshell-input-autoexpand 'input
  "*If non-nil, expand input command history references on completion.
This mirrors the optional behavior of tcsh (its autoexpand and histlit).

If the value is `input', then the expansion is seen on input.
If the value is `history', then the expansion is only when inserting
into the buffer's input ring."
  :type '(choice (const :tag "off" nil)
 (const input)
 (const history)
 (const :tag "on" t))
  :group 'eshell)

(defcustom eshell-input-ring-file-name "~/.history"
  "*If non-nil, name of the file to read/write input history.
See also `eshell-read-input-ring' and `eshell-write-input-ring'.
If it is nil, eshell will use the value of HISTFILE.

This variable is buffer-local, and is a good thing to set in mode hooks."
  :type 'file
  :group 'eshell)

(defcustom eshell-input-ring-size 32
  "*Size of the input history ring.  If nil, use envvar HISTSIZE."
  :type 'integer
  :group 'eshell)


(defcustom eshell-completion-fignore nil
  "*List of suffixes to be disregarded during file/command completion.
This variable is used to initialize `comint-completion-fignore' in the shell
buffer.  The default is nil, for compatibility with most shells.
Some people like (\"~\" \"#\" \"%\").

This is a fine thing to set in your `.emacs' file."
  :type '(repeat (string :tag "Suffix"))
  :group 'eshell)

(defcustom eshell-dynamic-complete-functions
  '(comint-replace-by-expanded-history
;   eshell-dynamic-complete-environment-variable
;   eshell-dynamic-complete-command
;   eshell-replace-by-expanded-directory
    comint-dynamic-complete-filename)
  "List of functions called to perform completion.
This variable is used to initialize
`comint-dynamic-complete-functions' in the eshell buffer.

This is a fine thing to set in your `.emacs' file."
  :type '(choice (const :tag "No completion functions" nil)
                 (repeat function))
  :group 'eshell)

(put 'eshell-dynamic-complete-functions 'risky-local-variable t)


(defcustom eshell-argument-kinds-list
  '(whitespace command lisp argument operator buffer process)
  "*A list of the possible argument kinds recognized by eshell.
The possible options right now are:

  `whitespace'  A group of whitespace between arguments.
  `command'     A sub-command (i.e., \"{command}\")
  `lisp'        A Lisp sub-expression (i.e., \"(+ 1 2)\" or \"'hello\")
  `argument'    A textual argument.  Maybe a file, command, etc.
  `operator'    An operator, such as >>, |...
  `buffer'      The name of a buffer.
  `process'     The name of a process.

Note: Any changes to this list will not be available to eshell until
you restart Emacs."
  :type '(repeat symbol)
  :group 'eshell)

(defun eshell-argument-kinds-choice ()
  "Return the arguments kinds in defcustom `choice' format."
  (append '(choice)
          (mapcar
           (function
            (lambda (kind)
              (list 'const kind)))
           eshell-argument-kinds-list)))

(defcustom eshell-argument-syntax-list
  '(("\\s-+" whitespace ignore)         ; whitespace is skipped
;   (lambda (command end)
;     (cons (substring command 0 end) end)))

    ;; Running a sub-command in a context is different from the main
    ;; eshell buffer is specified by {command}, where there is no
    ;; whitespace following the initial brace.
    ("{[^} ]" command
     (lambda (command end)
       (eshell-read-delimited-string command ?{ ?})))

    ;; A term beginning with ` ' or (, and followed by a non-
    ;; whitespace character, is read in as a lisp expression.  A
    ;; double-quote followed by anything is read in as a lisp string.
    ("\\(['`(]\\S-\\|\"\\)" lisp (lambda (command end)
                                   (read-from-string command)))

    ;; A term such as #<buffer NAME>, or #<process NAME> is taken to
    ;; refer to a buffer or process.
    ("#<buffer " buffer
     (lambda (command end)
       (eshell-read-delimited-string (substring command end)
                                      ?< ?> nil end)))

    ("#<process " process
     (lambda (command end)
       (eshell-read-delimited-string (substring command end)
                                      ?< ?> nil end)))

    ;; Simple shell operator: |, >>, etc.
    ("\\(;\\||\\|[0-9]?>+\\)" operator nil)

    ;; The next rule will matching anything that is not a member of
    ;; `eshell-file-name-quote-list', with the option that any members
    ;; of that list may be quoted by a backslash to remove their
    ;; special nature.
    ((lambda (command)
       (and (or (eq (aref command 0) ?$)
                (not (memq (aref command 0)
                           eshell-file-name-quote-list)))
            0))
     (lambda (object)
       (if (stringp object)
           'argument
         'lisp))
     eshell-read-argument))
  "*Define how to process eshell command arguments.
Each member of this alist should itself be a list of the format:

  (MATCH ITEM-KIND PROCESS-ARG)

MATCH is should be a regular expression to match the argument.  It
will always be bound to the beginning of the command string, so using
\"^\" is unnecessary.

ITEM-KIND is either a symbol identifying the kind of argument, or a
function returning a symbol.  The function should take the command
string as its argument.

PROCESS-ARG is either nil, or a function.  If a function, it should
take the command string as its argument, and return a cons cell, whose
car is the entity identified by the argument, and whose cdr is the
next index in the command string where parsing should resume.

The function `eshell-split-args' will use this alist to divide each
textual eshell command into arguments.  The resultant list will be in
raw form, meaning that each argument will have the list syntax: (TYPE
CONTENT) (or (TOKEN LEXEME), for parser people).  The TYPE is given by
the second argument of each entry in the alist above, and the CONTENT
is identified by the third function argument (or nil) in that alist.
See `eshell-argument-kinds-list' for more info."
  :type `(repeat
          (list (choice regexp function)
                ,(eshell-argument-kinds-choice)
                (choice function (const :tag "Use Match" nil))))
  :group 'eshell)

(put 'eshell-argument-syntax-list 'risky-local-variable t)

(defcustom eshell-argument-eval-alist
  '((lisp       . eval)
    (operator   . identity)
    (buffer     . get-buffer-create)
    (process    . get-process)

;   (whitespace . <ignored>)

    (argument
     . (lambda (arg)
         (if (not (string-match eshell-expand-filename-regexp arg))
             (eshell-convert-from-string arg)
           (setq arg (eshell-expand-file-name arg))
           (if (string-match "[*?]" arg)
               (or (eshell-glob (file-name-directory arg)
                                (file-name-nondirectory arg))
                   arg)
             arg))))

    (command
     . (lambda (cmd)
         (let ((result (eshell-subcommand cmd)))
           (if (not (stringp result))
               result
             (let ((elements (split-string result)))
               (if (= (length elements) 1)
                   (eshell-convert-from-string result)
                 (mapcar 'eshell-convert-from-string elements))))))))
  "*This list is used to evaluate different kinds of arguments.
The cdr of each element must be a function, and should either be
ignore, or return the resultant argument.  Always return the argument
in its most native Lisp form, so that the string \"3\" is always
returned as an integer.  There is other code in eshell to make certain
that this is converted back to a string before being passed to a disk
command."
  :type `(repeat
          (cons ,(eshell-argument-kinds-choice)
                function))
  :group 'eshell)

(put 'eshell-argument-eval-alist 'risky-local-variable t)

(defcustom eshell-argument-exec-alist
  '((lisp      . eshell-insert-lisp-command)
    (argument  . eshell-insert-named-command)
    (command   . eshell-command-in-temp-buffer))
  "*This list is used to execute different kinds of arguments.
The first argument in any command is the \"command argument\".  This
will be executed according to the above alist.  Each execution type is
responsible for doing its own argument evaluations.  The cdr of each
member of this alist should be a function taking two arguments: the
entity to be executed, and the arguments to (possibly) pass to it.
This function must return the object to which other process may send
their output, in the case of a pipe scenario."
  :type `(repeat
          (cons ,(eshell-argument-kinds-choice)
                function))
  :group 'eshell)

(put 'eshell-argument-exec-alist 'risky-local-variable t)


(defvar eshell-print-count 0
  "An integer representing the current command nesting level.")

(defsubst eshell-print (object handles)
  "Output OBJECT to the error handle of HANDLES."
  (eshell-output-object object handles eshell-output-handle)
  (setq eshell-print-count (1+ eshell-print-count))
  (if (= (mod eshell-print-count eshell-refresh-interval) 0)
      (sit-for 0)))

(defsubst eshell-error (object handles)
  "Output OBJECT to the error handle of HANDLES."
  (eshell-output-object object handles eshell-error-handle)
  (setq eshell-print-count (1+ eshell-print-count))
  (if (= (mod eshell-print-count eshell-refresh-interval) 0)
      (sit-for 0)))

(defsubst eshell-errorn (object handles)
  "Output OBJECT to the error handle of HANDLES."
  (eshell-error object handles)
  (eshell-error "\n" handles))

(defmacro eshell-transform (func handles)
  "Using FUNC to transform the output headed to HANDLES."
  `(cons (lambda (string handles)
           (if (stringp string)
               (eshell-print (,(eval func) string) handles)
             (eshell-finish handles string)))
         (list ,handles)))


(defcustom eshell-visual-commands
  '("vi" "emacs"                        ; what is going on??
    "top"                               ; ok, a valid program...
    "less" "more"                       ; M-x view-file
    "lynx" "ncftp"                      ; w3.el, ange-ftp
    "pine" "tin" "trn" "elm")           ; GNUS!!
  "*A list of commands that present their output in a visual fashion."
  :type '(repeat string)
  :group 'eshell)

(defcustom eshell-visual-term-name "eterm"
  "*Name to use for the TERM variable when running visual commands.
See `term-term-name' in term.el for more information on how this is
used."
  :type 'string
  :group 'eshell)


(defcustom eshell-command-aliases-list
  '(("alias" redef
     (lambda (args handles)
       (eshell-alias (eshell-eval-arg (car args)) (cdr args))
       (eshell-finish-command handles t)))

    ("define"   func  eshell-define)
    ("echo"     func  eshell-echo)
    ("which"    func  eshell-which)
    ("dos2unix" func  eshell-dos-to-unix)
    ("wait"     func  eshell-wait-for-process)
    ("list"     func  view-file)
    ("export"   func  eshell-export)
    ("exit"     func  bury-buffer)
    ("date"     func  current-time-string)
    ("man"      func  man)
;   ("mkdir"    func  make-directory)
;   ("rmdir"    func  delete-directory)
;   ("mv"       func  rename-file)
;   ("cp"       func  copy-file)
;   ("ln"       func  make-symbolic-link)
;   ("cat"      func  insert-file)

    ("setq"     redef  eshell-setq-alias)
    ("if"       redef  eshell-if-alias)
    ("jobs"     redef  eshell-jobs-alias)
    ("kill"     redef  eshell-kill-alias)
    ("cd"       redef  eshell-cd-alias)
    ("pwd"      redef  eshell-pwd-alias)
    ("rm"       redef  eshell-rm-alias)
    ("ls"       redef  eshell-ls-alias)
    ("history"  redef  eshell-history-alias))
  "*A list of command aliases.
You can alias a command name to either a Lisp function, by naming the
symbol, or a disk command, by passing a string.

If the alias is a redefinition, it should be a quoted function taking
the following arguments:

  (function
   (lambda (args handles)
     \"Redefine command, given ARGS.  HANDLES is the output destinations.\"
     (eshell-exec-command 'other-command args handles)))

This redefines the command to call the Lisp function `other-command'
\(Note: this is exactly what an alias does).  If you want to give disk
commands a chance to be preferred (based on the setting of
`eshell-prefer-lisp-functions'), use the string name \"other-command\"
instead of a symbol reference.

In the case of a redefinition, the function is responsible for setting
the value of `eshell-last-arguments' to be the set of cooked
arguments.  `eshell-insert-plain-command' will set this variable for
you, so it really depends on how low-level your alias is if you have
to worry about this or not.  See the examples above.

Keep in mind what the requirements of the various insertion functions
are.  You have the following choices, show below.  The all taken
arguments of the form (OBJECT ARGUMENTS HANDLES) (see below for more
about the HANDLES argument).  Each one of these functions will result in
a prompt being output after a non-backgrounded command.

 `eshell-exec-command'
    Execute a command named OBJECT, with ARGUMENTS.  ARGUMENTS are in
    raw form, meaning unevaluated.  They should each be a cons cell,
    with the car indicating the type of the argument, and the cdr
    being the actual contents.

    Basically this function looks up the argument type of the command
    OBJECT in `eshell-argument-exec-alist', and calls it with each of
    the above arguments.

 `eshell-insert-command'
    Execute a command which is in textual form.  ARGUMENTS specifies
    any additional, raw arguments, to add to the arguments specified
    for each command in the command string.  It's best to leave this
    argument alone!

    This function, after processing the textual command into raw
    argument form, calls `eshell-exec-command'.

 `eshell-insert-disk-command'
    Execute a command from disk, named OBJECT.  ARGUMENTS should be
    cooked at this point, with each argument being fully evaluated to
    its natural representation.  These arguments will be converted to
    strings for you.

    This function, after expanding and substituting environment
    variables, and finding what the interpretor program name is, calls
    `eshell-insert-process-output'.

 `eshell-insert-plain-command'
    Execute a command in which OBJECT might either be the name of a
    Lisp function, or the name of a disk command.  Based on the
    setting of `eshell-prefer-lisp-functions', and what eshell finds
    on disk, it will decide which one to call.  Aliases will not be
    consulted at this stage.  Also, the ARGUMENTS passed in must be
    raw, and this function will cook them.  If you call this function
    from an alias definition with your own arguments, remember to
    present them in raw format.  See `eshell-argument-syntax-list' for
    more information.

 `eshell-insert-named-command'
    Execute a named command, OBJECT, which could really be anything.
    The only thing for certain is that a textual name was given for
    OBJECT.  This could result in an alias being called, or a disk
    command, or Lisp command, etc.  The ARGUMENTS must be raw.

 `eshell-insert-command-alias'
    Invoke an alias, named OBJECT.  The ARGUMENT must be raw.  The
    alias may cook them if it wishes, and call a function that takes
    cooked arguments, or it can just pass them on to another function
    which accepts raw arguments.

 `eshell-insert-lisp-command'
    Insert a Lisp OBJECT.  The ARGUMENTS must be raw.  If it is a
    function, it will be called with the ARGUMENTS, after cooking
    them.  Otherwise, the text representation of the object will be
    inserted.

 `eshell-insert-process-output'
    Invoke the process named OBJECT, with the cooked ARGUMENTS.

If you want to modify the output of a command during execution, so
that a different result is presented, you will have to pass a function
in the HANDLES argument which will perform the modification and then
pass the modified data to `eshell-print' using the original HANDLES
argument that was passed to the alias (phew!).

A HANDLES argument is a list of target locations, or nil.  Possible
targets can be a buffer, a process, a file name, a Lisp variable, a
Lisp function, or a cons cells whose car is a function, and whose cdr
is a list of additional arguments to pass to that function -- in
addition to the string argument containing the data itself.

The macro `eshell-transform' allows you to do all of this easily, by
passing the function which will transform its string argument.  Unless
you need some special behavior (like messing with the contents of the

HANDLES argument during execution), you should use this macro.

Here is an full example that will upcase the output from \"ls\":

  (\"ls\" (lambda (args handles)
            (eshell-exec-command
             \"ls\" args
             (list
              (cons (lambda (string handles)
                      (if string
                          (eshell-print (upcase string) handles)
                        (eshell-finish handles string)))
                    (list handles))) t)))

Here is the same example, using `eshell-transform':

  (\"ls\" (lambda (args handles)
            (eshell-exec-command
             \"ls\" args (list (eshell-transform 'upcase handles)) t)))"
  :type `(repeat
          (list string
                (choice (const :tag "Name Alias" alias)
                        (const :tag "Lisp Function" func)
                        (const :tag "Redefinition" redef)
                        (const :tag "EShell Command"))
                (choice string function
                        (cons
                         ,(eshell-argument-kinds-choice)
                         sexp))))
  :group 'eshell)

(put 'eshell-command-aliases-list 'risky-local-variable t)

(defcustom eshell-variable-aliases-list
  '(("prompt"
     (let ((path (expand-file-name default-directory)))
       (concat (if (> (length path) 1)
                   (substring path 0 (1- (length path)))
                 path) " $ ")))
    ("_"                                ; provide $_, like ksh
     (lambda (indices get-len)
       (if (not (or indices get-len))
           (last eshell-last-arguments)
         (eshell-apply-indices eshell-last-arguments indices)))))
  "*This list provides aliasing for variable references.
It is very similar in concept to what `eshell-command-aliases-list'
does for commands.  Each member of this defines defines the name of a
command, and the Lisp value to return for that variable if it is
accessed via the syntax \"$NAME\".

If the value is a function, that function will be called with two
arguments: the list of the indices that was used in the reference, and
whether the user is requesting the length of the ultimate element.
For example, a reference of \"$NAME[10][20]\" would result in the
function for alias \"NAME\" being called (assuming it was aliased to a
function), and the arguments passed to this function would be the list
'(10 20), and nil."
  :type '(repeat (list string sexp))
  :group 'eshell)

(put 'eshell-variable-aliases-list 'risky-local-variable t)


(defface eshell-command-face
  '((t (:bold t)))
  "*Face used to highlight command text.")

;; Internal Variables:

(defvar eshell-process-list nil
  "A list of the current status of subprocesses.")

(defvar eshell-next-background-p nil
  "Indicates whether the next process type is background (&).")

(defvar eshell-output-file-buffer nil
  "If non-nil, the current buffer is a file output buffer.")

(defvar eshell-parent-buffer nil
  "Used to remember the eshell buffer that spawned a visual process.")

(defvar eshell-command-depth 0
  "An integer representing the current command nesting level.")

(defvar eshell-last-cd-location nil
  "The last directory that eshell was in.")

(defvar eshell-last-arguments nil
  "The arguments to the last eshell command.")

(defvar eshell-mode-map nil)
(unless eshell-mode-map
  (setq eshell-mode-map (nconc (make-sparse-keymap) comint-mode-map))

  (define-key eshell-mode-map "\C-cp" 'eshell-insert-process)
  (define-key eshell-mode-map "\C-cb" 'eshell-insert-buffer)
  (define-key eshell-mode-map "\C-ce" 'eshell-insert-envvar)

  (define-key eshell-mode-map "\M-\t" 'lisp-complete-symbol)
  (define-key eshell-mode-map "\t"    'comint-dynamic-complete)
  (define-key eshell-mode-map "\M-?"
    'comint-dynamic-list-filename-completions)

  (define-key eshell-mode-map "\C-c\C-c"  'eshell-interrupt-process)
  (define-key eshell-mode-map "\C-c\C-d"  'eshell-send-eof-to-process)
  (define-key eshell-mode-map "\C-c\C-q"  'eshell-continue-process)
  (define-key eshell-mode-map "\C-c\C-s"  'list-processes)
  (define-key eshell-mode-map "\C-c\C-z"  'eshell-stop-process)
  (define-key eshell-mode-map "\C-c\C-\\" 'eshell-quit-process))

;; User Functions:

(defsubst eshell-interactive-print (string)
  "Print STRING to the eshell display buffer."
  (comint-output-filter nil string))

;;;###autoload
(defun eshell ()
  "Create an interactive Emacs shell buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create eshell-buffer-name))
  (unless (eq major-mode 'eshell-mode)
    (random t)                            ; needed by `eshell-subcommand'
    (eshell-mode)
    (if eshell-display-banner-p
        (eshell-interactive-print
         (format "Welcome to the Emacs shell, version %s\n\n"
                 eshell-version)))
    (eshell-emit-prompt)))

;;;###autoload
(defun eshell-mode ()
  "Emacs shell mode.  Basically a smarter Lisp interaction mode."
  (interactive)
  (comint-mode)
  (setq major-mode 'eshell-mode)
  (setq mode-name "EShell")
  (setq mode-line-process nil)
  (use-local-map eshell-mode-map)

  (setq comint-prompt-regexp eshell-prompt-regexp
        comint-completion-fignore eshell-completion-fignore
        comint-input-ignoredups eshell-input-ignoredups
        comint-file-name-quote-list eshell-file-name-quote-list
        comint-dynamic-complete-functions eshell-dynamic-complete-functions
        comint-input-filter eshell-input-filter
        comint-input-filter-functions eshell-input-filter-functions
        comint-output-filter-functions eshell-output-filter-functions
        comint-preoutput-filter-functions eshell-preoutput-filter-functions
        comint-scroll-to-bottom-on-input eshell-scroll-to-bottom-on-input
        comint-scroll-to-bottom-on-output eshell-scroll-to-bottom-on-output
        comint-scroll-show-maximum-output eshell-scroll-show-maximum-output
        comint-buffer-maximum-size eshell-buffer-maximum-size
        comint-eol-on-send eshell-eol-on-send
        comint-input-autoexpand eshell-input-autoexpand
        comint-input-sender (function eshell-handle-input)
        comint-password-prompt-regexp eshell-password-prompt-regexp
        comint-input-ring-file-name
        (or eshell-input-ring-file-name (getenv "HISTFILE"))
        comint-input-ring-size
        (or eshell-input-ring-size (getenv "HISTSIZE")))

  (set (make-local-variable 'paragraph-start) comint-prompt-regexp)

  (set (make-local-variable 'font-lock-defaults)
       '(eshell-font-lock-keywords t))
  (and (boundp 'font-lock-global-modes)
       font-lock-global-modes
       (or (eq font-lock-global-modes t)
           (memq 'eshell-mode font-lock-global-modes))
       (not font-lock-mode)
       (font-lock-mode 1))

  (set (make-local-variable 'dired-directory) default-directory)
  (set (make-local-variable 'list-buffers-directory)
       (expand-file-name default-directory))
  (set (make-local-variable 'eshell-last-cd-location) default-directory)

  (set (make-local-variable 'eshell-process-list)
       (list (list 'eshell-stub-sym nil t)))
  (make-local-variable 'eshell-next-background-p)
  (make-local-variable 'eshell-last-arguments)
  (make-local-variable 'eshell-command-depth)
  (make-local-variable 'eshell-print-count)

  (if comint-input-ring-file-name
      (comint-read-input-ring t))

  (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'comint-write-input-ring nil t)

  (run-hooks 'eshell-mode-hook))

(eval-when-compile
  (defvar comint-last-input-start)
  (defvar comint-last-input-end))

(defun eshell-handle-input (process string)
  "Handle input from the user, either a string to send or a command.
If PROCESS is non-nil, send the input STRING directly to the current
interactive process.  Otherwise, assume that STRING contains a command
to be evaluated."
  (if (eshell-process-active-p)
      (eshell-process-interact
       "Process to send string to: "
       (function
        (lambda (proc)
          (process-send-string proc (concat string "\n")))))
    (if eshell-push-marks
        (push-mark comint-last-input-end t))
    (if (string-match "^\\s-*$" string)
        (eshell-emit-prompt)
      (unwind-protect
          (let ((background-p
                 (and (string-match "\\s-*&\\s-*$" string)
                      (setq string (replace-match "" t t string)))))
            (setq eshell-next-background-p background-p)
            (eshell-insert-command
             string nil (eshell-create-handles t)))
        (setq eshell-next-background-p nil)
        (unless eshell-point-after-result
          (goto-char comint-last-input-start))))))

;;;###autoload
(defun eshell-command (command output-target &optional error-target)
  "Execute string COMMAND; display output, if any.
Output is sent to OUTPUT-TARGET, which can be many things (see
`eshell-command-aliases-list' for more information).  Any error output
is sent to ERROR-TARGET.  If a prefix argument is given when called
interactively, or ARG is set, it will insert the result of the command
at point, barring any redirections."
  (interactive
   (list
    (let ((str (read-string "Emacs shell command: ")))
      (if (string-match "\\s-*&\\s-*$" str)
          (setq str (replace-match "" t t str)))
      str)
    (if current-prefix-arg
        (current-buffer)
      (let ((buf (get-buffer-create "*EShell Command Output*")))
        (with-current-buffer buf
          (erase-buffer))
        buf))))
  (save-excursion
    (with-temp-buffer
      (eshell-mode)
      (let ((value (eshell-subcommand command))
            multiline-p)
        (and (stringp value)
             (setq multiline-p (string-match "\n." value)))
        (if (not value)
            (message "(There was no command output)")
          (if (not multiline-p)
              (let ((val (if (stringp value)
                             value
                           (eshell-stringify value))))
                (if (string-match "\n+\\'" val)
                    (setq val (replace-match "" t t val)))
                (message val))
            (if (bufferp output-target)
                (display-buffer output-target))
            (eshell-print value (eshell-create-handles output-target
                                                       error-target))))))))

(defun eshell-process-active-p ()
  "Return non-nil if a foreground process is currently executing."
  (let ((entries eshell-process-list)
        process-active)
    (while entries
      (if (not (nth 2 (car entries)))
          (setq process-active t
                entries nil))
      (setq entries (cdr entries)))
    process-active))

(defun eshell-emit-prompt ()
  "Emit a prompt if eshell is being used interactively."
  (let ((prompt (eshell-variable "prompt")))
    ;; jww (1999-06-28): this could be hiding information the user
    ;; wants to see, such as the lack of a final newline in a file
    ;; that's being cat'd...
    (if (not (bolp))
        (setq prompt (concat "\n" prompt)))
    (eshell-interactive-print prompt)))

;; Command Handling Functions:

(defun eshell-command-in-temp-buffer (command args handles)
  "Execute COMMAND with ARGS using a temp buffer for context.
This is used so that certain Lisp commands, such as `cd', when
executed in a subshell, do not disturb the environment of the main
eshell buffer."
    (let ((tmp-buf (generate-new-buffer " *emacs cmd*"))
          (orig-buf (car handles)) result)
      (unwind-protect
          (progn
            (setcar handles tmp-buf)
            (setq result
                  (eshell-insert-command command args handles)))
        (setcar handles orig-buf)
        ;; since this function is really only for Lisp commands, it
        ;; doesn't matter if we kill the context of a still-executing
        ;; disk command.
        (kill-buffer tmp-buf)
        result)))


(defun eshell-insert-command (command args handles)
  "Insert output from COMMAND, using ARGS, into location HANDLES.
Note that COMMAND is a string at the point, and will have to be
divided up according to its component arguments."
  (let* ((terms (eshell-split-args command))
         (commands (eshell-separate-commands terms ";"))
         (orig-command-depth eshell-command-depth)
         proc)
    (condition-case err
        (while commands
          (let* ((pipeline (car commands))
                 (pieces (eshell-separate-commands pipeline "|" t))
                 pipe-handles)
            (setq proc nil)
            (while pieces
              ;; The next function call separates out any redirections,
              ;; and returns the new argument set, after modifying
              ;; `handles' (if need be).
              (setq pipe-handles
                    (or (and proc (eshell-create-handles proc))
                        (and (cdr commands)
                             (eshell-protect-handles handles))
                        handles)
                    args (eshell-find-output-targets
                          (append (cdar pieces) args) pipe-handles)
                    proc (eshell-exec-command (caar pieces)
                                              args pipe-handles)
                    args nil
                    pieces (cdr pieces))))
          ;; If there were ever threading available in Emacs, we would
          ;; have to block for the thread to complete here.
          (when (cdr commands)
            (eshell-wait-for-process proc))
          (setq commands (cdr commands)))
      (error
       (setq eshell-command-depth orig-command-depth)
       (let ((msg (error-message-string err)))
         (eshell-errorn msg handles)
         nil)))
    proc))

(defun eshell-exec-command (command args handles)
  "Execute COMMAND, passing it ARGS, and sending output to HANDLES.
HANDLES can be many things.  See the docstring for
`eshell-command-aliases-list'.

This function returns the object to which output for any subsequent
commands within the pipe should go, or nil if such output should be
dropped."
  (let ((handler (assq (car command) eshell-argument-exec-alist)))
    (if (not handler)
        (error "Unable to execute command: %s"
             (pp-to-string command))
      (funcall (cdr handler) (cdr command) args handles))))

(defun eshell-insert-disk-command (command args handles)
  "Insert output from a disk COMMAND, with ARGS, to HANDLES."
  ;; Resolve the file name of the command.  Globbing is not processed
  ;; in command position, however.
  (if (string-match "\\(^\\(/:\\|~\\)\\|[$]\\)" command)
      (setq command (eshell-expand-file-name
                     (substitute-in-file-name command))))
  (if (string-match "@[^:]+:" default-directory)
      (let (result)
        (with-temp-buffer
          (shell-command
           (mapconcat 'shell-quote-argument
                      (append (list command)
                              (eshell-flatten-list args)) " ") t)
          (setq result (buffer-substring (point-min) (point-max))))
        (eshell-print result handles)
        (eshell-finish-command handles t))
    (let ((interp (eshell-find-interpretor command handles)))
      (if interp
          (if (member (file-name-nondirectory (car interp))
                      eshell-visual-commands)
              (progn
                (eshell-exec-visual
                 (car interp) (append (cdr interp)
                                      (eshell-flatten-list args)))
                (eshell-finish-command handles t))
            (eshell-insert-process-output
             (car interp) (append (cdr interp)
                                  (eshell-flatten-list args)) handles))
        (eshell-finish-command handles nil)))))

(defun eshell-insert-plain-command (command args handles)
  "Insert output from a plain COMMAND, with ARGS, to HANDLES.
COMMAND may result in either a Lisp function being executed by name,
or a disk command."
  (let ((sym (intern-soft command)))
    (if (and sym (fboundp sym)
             (or eshell-prefer-lisp-functions
                 (not (eshell-which command))))
        (eshell-insert-lisp-command sym args handles)
      (setq eshell-last-arguments (eshell-eval-args args)
            eshell-command-depth (1+ eshell-command-depth))
      (eshell-insert-disk-command command eshell-last-arguments handles))))

(defun eshell-insert-named-command (command args handles)
  "Insert output from a plain COMMAND, passing ARGS, to HANDLES.
COMMAND may result in an alias being executed, or a plain command."
  (let ((alias (assoc command eshell-command-aliases-list)))
    (if (not alias)
        (eshell-insert-plain-command command args handles)
      ;; Each alias is responsible for setting
      ;; `eshell-last-arguments' and `eshell-command-depth'.
      (eshell-insert-command-alias alias args handles))))

(defun eshell-insert-command-alias (alias args handles)
  "Insert output from a command ALIAS, passing ARGS, into HANDLES."
   (cond ((eq (nth 1 alias) 'alias)
          (eshell-insert-named-command (nth 2 alias) args handles))
         ((eq (nth 1 alias) 'func)
          (eshell-insert-lisp-command (nth 2 alias) args handles))
         ((eq (nth 1 alias) 'redef)
          (funcall (nth 2 alias) args handles))
         ((eq (nth 1 alias) 'command)
          (eshell-insert-command (nth 2 alias) args handles))))

(defun eshell-exec-lisp (handles printer errprint func-or-form args form-p)
  "Execute a lisp FUNC-OR-FORM, maybe passing ARGS.
HANDLES is the list of output handles, while PRINTER and ERRPRINT
functions to use for printing regular messages, and errors.  FORM-P
should be non-nil if FUNC-OR-FORM represent a lisp form; ARGS will be
ignored in that case."
  (let (result)
    (condition-case err
        (progn
          (setq result
                (save-current-buffer
                  (with-current-buffer (or (car handles)
                                           (current-buffer))
                    (if form-p
                        (eval func-or-form)
                      (apply func-or-form args)))))
          (and result (funcall printer result handles))
          result)
      (error
       (funcall errprint (error-message-string err) handles)
       nil))))
  
(defsubst eshell-apply* (handles printer errprint func args)
  "Call FUNC, with ARGS, trapping errors and return them as output.
HANDLES is the list of output handles, while PRINTER and ERRPRINT
functions to use for printing regular messages, and errors."
  (eshell-exec-lisp handles printer errprint func args nil))

(defsubst eshell-funcall* (handles printer errprint func &rest args)
  "Call FUNC, with ARGS, trapping errors and return them as output."
  (eshell-apply* handles printer errprint func args))

(defsubst eshell-eval* (handles printer errprint form)
  "Evaluate FORM, trapping errors and returning them."
  (eshell-exec-lisp handles printer errprint form nil t))

(defsubst eshell-apply (handles func args)
  "Call FUNC, with ARGS, trapping errors and return them as output.
HANDLES is the list of output handles, while PRINTER and ERRPRINT
functions to use for printing regular messages, and errors."
  (eshell-apply* handles 'eshell-print 'eshell-error func args))

(defsubst eshell-funcall (handles func &rest args)
  "Call FUNC, with ARGS, trapping errors and return them as output."
  (eshell-apply handles func args))

(defsubst eshell-eval (handles form)
  "Evaluate FORM, trapping errors and returning them."
  (eshell-eval* handles 'eshell-print 'eshell-error form))

(defsubst eshell-applyn (handles func args)
  "Call FUNC, with ARGS, trapping errors and return them as output.
HANDLES is the list of output handles, while PRINTER and ERRPRINT
functions to use for printing regular messages, and errors."
  (eshell-apply* handles 'eshell-printn 'eshell-errorn func args))

(defsubst eshell-funcalln (handles func &rest args)
  "Call FUNC, with ARGS, trapping errors and return them as output."
  (eshell-applyn handles func args))

(defsubst eshell-evaln (handles form)
  "Evaluate FORM, trapping errors and returning them."
  (eshell-eval* handles 'eshell-printn 'eshell-errorn form))

(defun eshell-insert-lisp-command (object args handles)
  "Insert Lisp OBJECT, using ARGS if a function, to HANDLES."
  (setq eshell-last-arguments (eshell-eval-args args)
        eshell-command-depth (1+ eshell-command-depth))
  (let* ((call-buf (current-buffer))
         (status
          (if (functionp object)
              (eshell-apply handles object eshell-last-arguments)
            (eshell-eval handles object))))
    (eshell-finish-command handles status)))

(defun eshell-finish-command (handles status)
  "Finish up a command, usually an alias, with final output to HANDLES.
STATUS should be non-nil if the command was \"successful\" (this is
open to interpretation; but imagine that someone were using the result
of the command in an `if' statement)."
  (eshell-close-handles handles status)
  (if (> eshell-command-depth 0)
      (setq eshell-command-depth (1- eshell-command-depth)))
  (unless (> eshell-command-depth 0)
    (eshell-emit-prompt)))

(defsubst eshell-record-process-object (object handles)
  "Record OBJECT as now running, with output to HANDLES."
  (if (and (processp object) eshell-next-background-p)
      (eshell-interactive-print (format "[%s] %d\n" (process-name object)
                                        (process-id object))))
  (nconc eshell-process-list
         (list (list object handles eshell-next-background-p))))

(defun eshell-insert-process-output (command args handles)
  "Insert the output from COMMAND, passing ARGS, into location HANDLES."
  (let* ((introut (and (eq (car (aref (cadr handles)
                                 eshell-output-handle)) t)
                       (eq (car (aref (cadr handles)
                                 eshell-error-handle)) t)))
         (proc
          (apply 'start-process
                 (file-name-nondirectory command) nil
                 (append (list command)
                         (eshell-flatten-list
                          (eshell-stringify-list args))))))
    (eshell-record-process-object proc handles)
    (set-process-buffer proc (current-buffer))
    (if introut
        (set-process-filter proc 'comint-output-filter)
      (set-process-filter proc 'eshell-insertion-filter))
    (set-process-sentinel proc 'eshell-sentinel)
    proc))

(defun eshell-subcommand (command)
  "Run COMMAND as a synchronous subcommand.
Returns a string comprising the output from the command."
  (let (unique-var)
    (while (not unique-var)
      (let ((name (concat "eshell-tmpvar-"
                          (int-to-string (random)))))
        (unless (intern-soft name)
          (setq unique-var (intern name))
          (set unique-var nil))))
    (let ((handles (eshell-create-handles unique-var)))
      (unwind-protect
          (progn
            ;; don't output a prompt after a subcommand
            (setq eshell-command-depth (1+ eshell-command-depth))
            (eshell-wait-for-process    ; run it synchronously
             (eshell-command-in-temp-buffer
              command nil (eshell-protect-handles handles))))
        (setq eshell-command-depth (1- eshell-command-depth)))
      (let ((value (symbol-value unique-var)))
        (eshell-close-handles handles value)
        (unintern (symbol-name unique-var))
        value))))

(defalias 'esh 'eshell-subcommand)

(defun eshell-separate-commands (terms separator &optional reverse)
  "Separate TERMS using SEPARATOR.  Possibly REVERSE the arguments."
  (let ((sub-terms (list t))
        commands)
    (while terms
      (if (and (eq (caar terms) 'operator)
               (string= (cdar terms) separator))
          (setq commands (cons (cdr sub-terms) commands)
                sub-terms (list t))
        (nconc sub-terms (list (car terms))))
      (setq terms (cdr terms)))
    (if (> (length sub-terms) 1)
        (setq commands (cons (cdr sub-terms) commands)))
    (if reverse
        commands                        ; already reversed
      (reverse commands))))

(defun eshell-find-interpretor (file handles)
  "Find the command interpretor with which to execute FILE.
Any errors to output will go to HANDLES."
  (let ((fullname
         (if (string-match
              (regexp-quote (char-to-string directory-sep-char))
              file)
             (expand-file-name file)
           (eshell-which file))))
    (if (not fullname)
        (progn
          (eshell-errorn (concat file " not found") handles)
          nil)
      (with-temp-buffer
        (insert-file-contents-literally
         fullname nil 0 eshell-command-interpretor-max-length)
        (goto-char (point-min))
        (if (re-search-forward "^#!\\(.*\\)" nil t)
            (append (split-string (match-string 1))
                    (list fullname))
          (list fullname))))))

;; Output Handling Functions:

(defun eshell-create-handles (standard-output &optional standard-error
                                              context-buffer)
  "Create a new set of output handles.
The default location for standard output and standard error will go to
STANDARD-OUTPUT and STANDARD-ERROR, respectively.  CONTEXT-BUFFER, if
non-nil, is the buffer in which operations should be performed.  This
could modify the input/output characteristics of the command, which is
why we include it here."
  (let ((handles (list context-buffer
                       (make-vector eshell-number-of-handles nil)))
        (output-target (eshell-get-target standard-output))
        (error-target (eshell-get-target standard-error)))
    ;; the format of handles is (CONTEXT HANDLES)
    (aset (cadr handles) 1 (cons output-target 1))
    (if standard-error
        (aset (cadr handles) 2 (cons error-target 1))
      (aset (cadr handles) 2 (cons output-target 1)))
    handles))

(defun eshell-protect-handles (handles)
  "Protect HANDLES from a being closed."
  (let ((idx 0))
    (while (< idx eshell-number-of-handles)
      (if (aref (cadr handles) idx)
          (setcdr (aref (cadr handles) idx)
                  (1+ (cdr (aref (cadr handles) idx)))))
      (setq idx (1+ idx))))
  handles)

(defun eshell-close-target (target status)
  "Close an output TARGET, passing STATUS as the result.
STATUS should be non-nil on successful termination of the output."
  (cond
   ;; If we were redirecting to a file, save the file and close the
   ;; buffer.
   ((markerp target)
    (let ((buf (marker-buffer target)))
      (when buf                         ; somebody's already killed it!
        (save-current-buffer
          (set-buffer buf)
          (when eshell-output-file-buffer
            (save-buffer)
            (kill-buffer buf))))))

   ;; If we're redirecting to a process (via a pipe, or process
   ;; redirection), send it EOF so that it knows we're finished.
   ((processp target)
    (if (eq (process-status target) 'run)
        (process-send-eof target)))

   ;; A plain function redirection needs no additional arguments
   ;; passed.  We pass a string of nil so that it knows that output is
   ;; done.
   ((functionp target)
    (funcall target status))

   ;; But a more complicated function redirection (which can only
   ;; happen with aliases at the moment) has arguments that need to be
   ;; passed along with it.
   ((consp target)
    (apply (car target) status (cdr target)))))

(defun eshell-close-handles (handles status)
  "Close all of the output HANDLES, taking refcounts into account.
If the argument STATUS is provided, it will be passed to any function
handles.  The value of STATUS should be non-nil on successful
completion, or nil for failure."
  (let ((idx 0))
    (while (< idx eshell-number-of-handles)
      (let ((ohs (cadr handles)))       ; get output handles
      (when (aref ohs idx)
        (setcdr (aref ohs idx)
                (1- (cdr (aref ohs idx))))
        (when (= (cdr (aref ohs idx)) 0)
          (let ((target (car (aref ohs idx))))
            (if (not (listp target))
                (eshell-close-target target status)
              (while target
                (eshell-close-target (car target) status)
                (setq target (cdr target)))))
          (setcar (aref ohs idx) nil))))
      (setq idx (1+ idx)))))

(defun eshell-get-target (target &optional mode)
  "Convert TARGET, which is a raw argument, into a valid output target.
MODE is either `overwrite', `append' or `insert'."
  (unless mode (setq mode 'insert))
  (cond
   ((stringp target)
    (let ((buf (find-file-noselect target t)))
      (with-current-buffer buf
        (set (make-local-variable 'eshell-output-file-buffer) t)
        (cond ((eq mode 'overwrite)
               (erase-buffer))
              ((eq mode 'append)
               (goto-char (point-max))))
        (point-marker))))
   ((or (bufferp target)
        (and eshell-buffer-shorthand
             (symbolp target)))
    (let ((buf (if (bufferp target)
                   target
                 (get-buffer-create
                  (symbol-name target)))))
      (with-current-buffer buf
        (cond ((eq mode 'overwrite)
               (erase-buffer))
              ((eq mode 'append)
               (goto-char (point-max))))
        (point-marker))))
   ((or (symbolp target)
        (processp target))
    target)
   (t
    (error "Illegal redirection target: %s"
           (pp-to-string target)))))

(defun eshell-set-output-handle (handles index target mode)
  "Set the output HANDLES entry INDEX, to point to TARGET, using MODE."
  (setq target (eshell-eval-arg target))
  (if (and (stringp target)
           (string= target "/dev/null"))
      (aset (cadr handles) index nil)
    (let ((where (eshell-get-target target mode))
          (current (car (aref (cadr handles) index))))
      (if (listp current)
          (setq current (append current (list where)))
        (setq current (list where)))
      (if (not (aref (cadr handles) index))
          (aset (cadr handles) index (cons nil 1)))
      (setcar (aref (cadr handles) index) current))))

(defun eshell-find-output-targets (args handles)
  "Given a list of ARGS, determine where its output should go.
HANDLES is the set of output handles to use.  NYI: Further
documentation to come.

This function returns a list containing the rewritten arguments and a
list of output destinations (or nil).  DEFAULT is where output should
go if nothing can be determined from the arguments.  DEFAULT defaults
to the current buffer if nil."
  (let ((new-args (list t)))
    (while args
      (if (not (and (eq (caar args) 'operator)
                    (string-match "^\\([0-9]?\\)\\(>+\\)$" (cdar args))))
          (nconc new-args (list (car args)))
        (eshell-set-output-handle
         handles (or (and (> (length (match-string 1 (cdar args))) 0)
                          (string-to-int (match-string 1 (cdar args))))
                     1)
         (cadr args) (aref [overwrite append insert]
                           (1- (length (match-string 2 (cdar args))))))
        (setq args (cdr args)))
      (setq args (cdr args)))
    (cdr new-args)))

(defsubst eshell-printn (object handles)
  "Output OBJECT to the error handle of HANDLES."
  (eshell-print object handles)
  (eshell-print "\n" handles))

(defun eshell-output-object-to-target (object target)
  "Insert OBJECT into TARGET.
Returns what was actually sent, or nil if nothing was sent."
  (cond
   ((markerp target)
    (if (buffer-live-p (marker-buffer target))
        (with-current-buffer (marker-buffer target)
          (let ((moving (= (point) target)))
            (save-excursion
              (goto-char target)
              (setq object (eshell-stringify object))
              (insert object)
              (set-marker target (point-marker)))
            (if moving
                (goto-char target))))))

   ((processp target)
    (when (eq (process-status target) 'run)
      (setq object (eshell-stringify object))
      (process-send-string target object)))

   ((functionp target)
    (funcall target object))

   ((consp target)
    (apply (car target) object (cdr target)))

   ((symbolp target)
    (if (eq target t)                   ; means "print to display"
        (eshell-interactive-print (eshell-stringify object))
      (if (not (symbol-value target))
          (set target object)
        (setq object (eshell-stringify object))
        (if (not (stringp (symbol-value target)))
            (set target (eshell-stringify
                         (symbol-value target))))
        (set target (concat (symbol-value target) object))))))
  object)

(defun eshell-output-object (object handles &optional handle-index)
  "Insert OBJECT into HANDLES, using HANDLE-INDEX specifically)."
  (when handles
    (let ((target (car (aref (cadr handles)
                             (or handle-index eshell-output-handle)))))
      (if (and target (not (listp target)))
          (eshell-output-object-to-target object target)
        (while target
          (eshell-output-object-to-target object (car target))
          (setq target (cdr target)))))))

;; Process Handling Functions:

(eval-when-compile
  (require 'term))

(defun eshell-exec-visual (program args)
  "Run the specified PROGRAM in a terminal emulation buffer.
ARGS are passed to the program.  At the moment, no piping of input is
allowed."
  (require 'term)
  (let ((term-buf (generate-new-buffer (concat "*" program "*")))
        (eshell-buf (current-buffer)))
    (save-current-buffer
      (set-buffer term-buf)
      (term-mode)
      (set (make-local-variable 'term-term-name)
           eshell-visual-term-name)
      (set (make-local-variable 'eshell-parent-buffer) eshell-buf)
      (term-exec term-buf program program nil args)
      (set-process-sentinel
       (get-buffer-process term-buf) 'eshell-visual-sentinel)
      (term-char-mode)
      (if eshell-escape-control-x
          (term-set-escape-char ?\C-x))
      (switch-to-buffer term-buf)))
  nil)

(defun eshell-visual-sentinel (proc string)
  "Destroy the buffer visiting PROC."
  (let ((proc-buf (process-buffer proc)))
    (if (and (buffer-live-p proc-buf)
             (eq (current-buffer) proc-buf))
        (let ((buf (or (and (boundp 'eshell-parent-buffer)
                            eshell-parent-buffer)
                       (get-buffer eshell-buffer-name))))
          (and buf (switch-to-buffer buf))))
    (if (buffer-live-p proc-buf)
        (kill-buffer proc-buf))))

(defun eshell-remove-process-entry (entry)
  "Record the process ENTRY as fully completed; output was to HANDLES."
  (if (and (processp (car entry))
           (nth 2 entry))
      (eshell-interactive-print (format "[%s]+ Done %s\n"
                                        (process-name (car entry))
                                        (process-command (car entry)))))
  (delq entry eshell-process-list))

(defun eshell-insertion-filter (proc string)
  "Insert a string into the eshell buffer, or a process/file/buffer.
PROC is the process for which we're inserting output.  STRING is the
output."
  (when (buffer-live-p (process-buffer proc))
    (set-buffer (process-buffer proc))
    (eshell-output-object string (cadr (assq proc eshell-process-list)))))

(defun eshell-sentinel (proc string)
  "Generic sentinel for command processes.  Reports only signals.
PROC is the process that's exiting.  STRING is the exit message."
  (when (and (not (string= string "run"))
             (buffer-live-p (process-buffer proc)))
    (set-buffer (process-buffer proc))
    (unless (string-match "^\\(finished\\|exited\\)" string)
      (eshell-insertion-filter proc string))
    (let* ((entry (assq proc eshell-process-list)))
      (when entry
        (eshell-finish-command
         (cadr entry) (= (process-exit-status proc) 0))
        (eshell-remove-process-entry entry)))))

(defun eshell-wait-for-process (proc)
  "Wait until PROC has successfully completed."
  (when (processp proc)
    ;; NYI: If the process gets stopped here, that's bad.
    (while (assq proc eshell-process-list)
      (if (input-pending-p)
          (discard-input))
      (sit-for eshell-process-wait-seconds
               eshell-process-wait-milliseconds))))

(defun eshell-read-process-name (prompt)
  "Read the name of a process from the minibuffer, using completion.
The prompt will be set to PROMPT."
  (completing-read prompt
                   (mapcar
                    (function
                     (lambda (proc)
                       (cons (process-name proc) t)))
                    (process-list)) nil t))

(defun eshell-insert-process ()
  "Insert PROCESS-NAME into the current buffer at point."
  (interactive)
  (eshell-process-interact
   "Name of process: "
   (function
    (lambda (proc)
      (insert "#<process " (process-name proc) ">")))))

(defun eshell-insert-buffer (buffer-name)
  "Insert BUFFER-NAME into the current buffer at point."
  (interactive "BName of buffer: ")
  (insert "#<buffer " buffer-name ">"))

(defun eshell-insert-envvar (envvar-name)
  "Insert ENVVAR-NAME into the current buffer at point."
  (interactive
   (list (read-envvar-name "Name of environment variable: " t)))
  (insert "$" envvar-name))

(defun eshell-process-interact (prompt func)
  "Interact with a process, using PROMPT if more than one, via FUNC."
  (let ((fg-proc-count 0) fg-last-proc)
    (let ((entry eshell-process-list))
      (while entry
        (if (not (nth 2 (car entry)))
            (setq fg-proc-count (1+ fg-proc-count)
                  fg-last-proc (nth 0 (car entry))))
        (setq entry (cdr entry))))
    (cond
     ((and (= fg-proc-count 1)
           (processp fg-last-proc))
      (funcall func fg-last-proc))
     ((> fg-proc-count 1)               ; how??
      (funcall func (get-process
                     (eshell-read-process-name prompt)))))))

(defun eshell-interrupt-process ()
  "Interrupt a process."
  (interactive)
  (eshell-process-interact "Process to interrupt: " 'interrupt-process)
  ;; reset the command depth, in case something has gone wrong
  (when (> eshell-command-depth 0)
    (setq eshell-command-depth 0)
    (eshell-emit-prompt)))

(defun eshell-quit-process ()
  "Send quit signal to process."
  (interactive)
  (eshell-process-interact "Process to quit: " 'quit-process)
  (when (> eshell-command-depth 0)
    (setq eshell-command-depth 0)
    (eshell-emit-prompt)))

(defun eshell-stop-process ()
  "Send STOP signal to process."
  (interactive)
  (eshell-process-interact "Process to stop: " 'stop-process))

(defun eshell-continue-process ()
  "Send CONTINUE signal to process."
  (interactive)
  (eshell-process-interact "Process to continue: " 'continue-process))

(defun eshell-send-eof-to-process ()
  "Send EOF to process."
  (interactive)
  (eshell-process-interact "Process to send EOF to: " 'process-send-eof))

;; Argument Handling Functions:

(defun eshell-regexp (directory regexp)
  "Return a list of files in the given DIRECTORY matching REGEXP."
  (directory-files (or directory default-directory)
                   directory regexp))

(defun eshell-glob-regexp (pattern)
  "Convert glob-pattern PATTERN to a regular expression."
  (let ((matched-in-pattern 0);; How many chars of PATTERN we've handled.
        regexp)
    (while (string-match "[[?*]" pattern matched-in-pattern)
      (let ((op-end (match-end 0))
            (next-op (aref pattern (match-beginning 0))))
        (setq regexp (concat regexp
                             (regexp-quote
                              (substring pattern matched-in-pattern
                                         (match-beginning 0)))))
        (cond ((= next-op ??)
               (setq regexp (concat regexp "."))
               (setq matched-in-pattern op-end))
              ((= next-op ?\[)
               ;; Fails to handle ^ yet ????
               (let* ((set-start (match-beginning 0))
                      (set-cont
                       (if (= (aref pattern (1+ set-start)) ?^)
                           (+ 3 set-start)
                         (+ 2 set-start)))
                      (set-end (string-match "]" pattern set-cont))
                      (set (substring pattern set-start (1+ set-end))))
                 (setq regexp (concat regexp set))
                 (setq matched-in-pattern (1+ set-end))))
              ((= next-op ?*)
               (setq regexp (concat regexp ".*"))
               (setq matched-in-pattern op-end)))))
    (concat "\\`"
            regexp
            (regexp-quote
             (substring pattern matched-in-pattern))
            "\\'")))

(defun eshell-glob (directory glob)
  "Return a list of files in the given DIRECTORY matching GLOB."
  (eshell-regexp directory (eshell-glob-regexp glob)))

(defun eshell-expand-file-name (filename)
  "Expand the FILENAME, but only if needed."
  (if (string-match "^\\(/?~\\|/:/\\)" filename)
      (expand-file-name filename)
    filename))

(defun eshell-convert-from-string (string)
  "Convert STRING into a more native looking Lisp object."
  (when string
    (if (and (> (length string) 0)
             (eq (aref string (1- (length string))) ?\n))
        (setq string (substring string 0 (1- (length string)))))
    (if (string-match "^[0-9]+\\(\\.[0-9]+\\)?$" string)
        (string-to-number string)
      string)))

(defun eshell-flatten-list (args)
  "Flatten any lists within ARGS, so that there are no sublists."
  (let ((new-list (list t)))
    (while args
      (if (listp (car args))
          (nconc new-list (eshell-flatten-list (car args)))
        (nconc new-list (list (car args))))
      (setq args (cdr args)))
    (cdr new-list)))

(defun eshell-stringify (object)
  "Convert OBJECT into a string value."
  (cond
   ((stringp object) object)
   ((listp object)
    (let ((string (pp-to-string object)))
      (substring string 0 (1- (length string)))))
   (t
    (pp-to-string object))))

(defun eshell-stringify-list (args)
  "Convert each element of ARGS into a string value."
  (mapcar 'eshell-stringify args))

(defun eshell-get-variable (name)
  "Get the value for the variable NAME."
  (let ((alias (cadr (assoc name eshell-variable-aliases-list))))
    (if alias
        (eval alias)
      (let* ((sym (intern-soft name))
             (value
              (if (and sym (boundp sym)
                       (or eshell-prefer-lisp-variables
                           (not (getenv name))))
                  (symbol-value sym)
                (getenv name))))
        value))))

(defun eshell-eval-variable-ref (string)
  "Eval a (virtual) variable reference at start of STRING.
Returns a cons, the car of which is the value, and the cdr of which is
the new next location in the string.

Possible options are:

    NAME          an environment or Lisp variable value
    <LONG-NAME>   disambiguates the length of the name
    {COMMAND}     result of command is variable's value
    (LISP-FORM)   result of Lisp form is variable's value"
  (cond
   ((eq (aref string 0) ?{)
    (let ((val (eshell-read-delimited-string string ?{ ?})))
      (setcar val (eshell-subcommand (car val)))
      val))
   ((eq (aref string 0) ?<)
    (let ((val (eshell-read-delimited-string string ?< ?>)))
      (setcar val (eshell-get-variable (car val)))
      val))
   ((eq (aref string 0) ?\()
    (let ((val (read-from-string string)))
      (setcar val (eval (car val)))
      val))
   ((string-match (concat "^" eshell-variable-name-regexp)
                  string)
    (let ((eom (match-end 0)))
      (cons (eshell-get-variable
             (match-string 0 string))
            eom)))))

(defun eshell-read-indices (string)
  "Parse the indices identified in STRING.
Returns a cons cell, the first item of which is the list of list of
indices, and the second of which is the end of the indices within the
string."
  (let ((end 0)
        (remainder string)
        indices ref)
    (while (setq ref (and (not (= end (length string)))
                          (eshell-read-delimited-string
                           remainder ?\[ ?\])))
      (setq indices
            (cons (eshell-eval-args
                   (eshell-split-args
                    (car ref))) indices)
            end (+ end (cdr ref))
            remainder (substring string end)))
    (cons (reverse indices) end)))

(defun eshell-apply-indices (value indices)
  "Apply to VALUE all of the given INDICES, returning the sub-result.
The format of INDICES is:

  ((INT-OR-NAME-OR-OTHER INT-OR-NAME INT-OR-NAME ...)
   ...)

Each member of INDICES represents a level of nesting.  If the first
member of a sublist is not an integer or name, and the value it's
reference is a string, that will be used as the regexp with which is
to divide the string into sub-parts.  The default is whitespace.
Otherwise, each INT-OR-NAME refers to an element of the list value.
Integers imply a direct index, and names, an associate lookup using
`assoc'.

For example, to retrieve the second element of a user's record in
/etc/passwd, the variable reference would look like:

  ${egrep johnw /etc/passwd}[: 2]"
  (if (functionp value)
      (setq value (funcall value indices))
    (while indices
      (let ((refs (car indices)))
        (when (stringp value)
          (let (separator)
            (if (not (or (not (stringp (caar indices)))
                         (string-match
                          (concat "^" eshell-variable-name-regexp "$")
                          (caar indices))))
                (setq separator (caar indices)
                      refs (cdr refs)))
            (setq value
                  (mapcar 'eshell-convert-from-string
                          (split-string value separator)))))
        (cond
         ((< (length refs) 0)
          (error "Illegal array variable index: %s"
                 (pp-to-string refs)))
         ((= (length refs) 1)
          (setq value
                (if (stringp (car refs))
                    (cdr (assoc (car refs) value))
                  (nth (car refs) value))))
         (t
          (let ((new-value (list t)))
            (while refs
              (if (stringp (car refs))
                  (nconc new-value
                         (list (cdr (assoc (car refs) value))))
                (nconc new-value (list (nth (car refs) value))))
              (setq refs (cdr refs)))
            (setq value (cdr new-value))))))
      (setq indices (cdr indices)))
    value))

(defun eshell-variable (name)
  "Return the value of variable NAME."
  (or (car (eshell-read-variable-reference name)) ""))

(defalias 'eshvar 'eshell-subcommand)

(defun eshell-read-variable-reference (string)
  "Convert the next variable reference in STRING to its value.
The variable name could refer to either an environment variable, or a
Lisp variable.  The priority order depends on the setting of
`eshell-prefer-lisp-variables'.  This function returns a cons cell,
the car of which is the value, and the cdr of which is the end of the
variable name within the string."
  (let ((index 0))
    (if (or (= index (length string))
            (eq (aref string index) ?$))
        (cons "$" index)
      (let* ((get-len (if (eq (aref string index) ?#)
                          (setq index (1+ index))))
             (remainder (substring string index))
             (value (eshell-eval-variable-ref remainder))
             indices)
        (if (not value)
            (cons nil index)
          (setq index (+ index (cdr value))
                remainder (substring string index)
                indices (eshell-read-indices remainder)
                index (+ index (cdr indices))
                value
                (if (functionp (car value))
                    (funcall (car value) (car indices) get-len)
                  (eshell-apply-indices
                   (car value) (car indices))))
          (cons (if get-len
                    (length value)
                  value)
                index))))))

(defun eshell-read-delimited-string
  (string open close &optional include-delimiter offset)
  "Starting within STRING at INDEX, use OPEN and CLOSE to parse it.
This function extracts textual data that might contain nested
delimiters.  Returns a cons with the car being the full string found,
and the cdr being the ending position, which includes the final
delimiter.  INCLUDE-DELIMITER means that the delimiter should be
included in the string result.  OFFSET means that the index reported
should be increased by that amount."
  (let ((depth 1)
        (index 0)
        (start 0)
        (len (length string)))
    (if (= (aref string 0) open)
        (setq index 1 start 1))
    (while (and (> depth 0)
                (< index len))
      (let ((c (aref string index)))
        (cond ((and (= c ?\\)
                    (< (1+ index) len)
                    (member (aref string (1+ index))
                            (list open close)))
               (setq index (1+ index)))
              ((= c open)
               (setq depth (1+ depth)))
              ((= c close)
               (setq depth (1- depth)))))
      (setq index (1+ index)))
    (and (= depth 0)
         (cons (if include-delimiter
                   (substring string 0 index)
                 (substring string start (1- index)))
               (if offset (+ index offset) index)))))

(defun eshell-read-argument (string ignored)
  "Read an argument from STRING.
The second argument to this function is always IGNORED, and exists
merely to make it convenient to call from within
`eshell-argument-syntax-list'.

At this point we know that STRING probably contains a filename or
other simple string -- but it could also be an integer, a variable
substitution, etc.  Returns a cons cell, the car of which is the
value, and the cdr of which marks the end of the value within the
string."
  (let* ((index 0)
         (result "")
         (len (length string)))
    (while (and (< index len)
                (or (eq (aref string index) ?$)
                    (not (memq (aref string index)
                               eshell-file-name-quote-list))))
      (cond
       ((eq (aref string index) ?$)
        (setq index (1+ index))
        (let* ((variable (eshell-read-variable-reference
                          (substring string index)))
               (end (+ index (cdr variable))))
          (when (car variable)
            ;; If the argument consisted solely of a variable
            ;; reference, don't convert its resultant value to a
            ;; flattened string.  Otherwise, do so since there is
            ;; concatenation going on.
            (if (and (= index 1)
                     (or (= end len)
                         (and (memq (aref string end)
                                    eshell-file-name-quote-list)
                              (not (eq (aref string (1- end)) ?\\)))))
                (if (stringp (car variable))
                    (setq result
                          (list 'quote (split-string (car variable) "\n")))
                  (setq result (list 'quote (car variable))))
              (let ((text (eshell-stringify (car variable))))
                (if (string-match "\n+" text)
                    (setq text (replace-match "" t t text)))
                (setq result (concat result text)))))
          (setq index end)))
       ((and (eq (aref string index) ?\\)
             (< (1+ index) len)
             (memq (aref string (1+ index))
                   eshell-file-name-quote-list))
        (setq result
              (concat result
                      (char-to-string (aref string (1+ index))))
              index (+ index 2)))
       (t
        (setq result
              (concat result
                      (char-to-string (aref string index)))
              index (1+ index)))))
    (cons result index)))

(defun eshell-get-next-arg (command)
  "Get the next argument from COMMAND."
  (let ((syntax eshell-argument-syntax-list)
        result)
    (while syntax
      (let (end)
        (if (functionp (caar syntax))
            (setq end (funcall (caar syntax) command))
          (if (string-match (concat "^" (caar syntax)) command)
              (setq end (match-end 0))))
        (if end
            (let* ((kind (nth 1 (car syntax)))
                   (arg (nth 2 (car syntax)))
                   (item (if (functionp arg)
                             (funcall arg command end)
                           (cons (substring command 0 end)
                                 end))))
              (setq result
                    (cons
                     (and item
                          (cons (if (functionp kind)
                                    (funcall kind (car item))
                                  kind)
                                (car item)))
                     (substring command
                                (if item (cdr item) end)))
                    syntax nil))))
      (setq syntax (cdr syntax)))
    (if result
        result
      (error "Syntax error parsing argument: %s" command))))

(defun eshell-split-args (command)
  "Split COMMAND into arguments."
  (while (string-match "[\t\n]+" command)
    (setq command (replace-match " " t t command)))
  (let ((args (list t)))
    (while (> (length command) 0)
      (let ((arg (eshell-get-next-arg command)))
        (if (car arg)
            (nconc args (list (car arg))))
        (setq command (cdr arg))))
    (cdr args)))

(defun eshell-eval-arg (arg)
  "Evaluate the argument ARG, according to its kind."
  (let ((handler (assq (car arg) eshell-argument-eval-alist)))
    (if handler
        (funcall (cdr handler) (cdr arg))
      (error "Unable to eval argument: %s" (pp-to-string arg)))))

(defun eshell-eval-args (args)
  "Evaluate the given list of ARGS."
  (mapcar 'eshell-eval-arg args))

;; Basic UNIX-like Commands:

(defun eshell-search-path (&rest names)
  "Search the environment path for NAMES, returning the first found."
  (let (file)
    (while names
      (if (setq file
                (locate-library (car names) t
                                (split-string (getenv "PATH")
                                              path-separator)))
          (setq names nil)
        (setq names (cdr names))))
    (and file
         (file-truename (expand-file-name file)))))

(defun eshell-which (name)
  "Locate the executable program named NAME."
  (interactive "sWhere is executable program: ")
  (let ((program
         (apply 'eshell-search-path
                (if (eq system-type 'windows-nt)
                    (list (concat name ".exe")
                          (concat name ".com")
                          (concat name ".bat")
                          name)
                  (list name)))))
    (when (and (interactive-p) program
               (file-executable-p program))
      (message program))
    program))

(defun eshell-echo (&rest args)
  "Concat the ARGS into a string and return it."
  (mapconcat 'eshell-stringify (eshell-flatten-list args) " "))

(defun eshell-alias (alias definition)
  "Define an ALIAS using DEFINITION."
  (if (not definition)
      (setq eshell-command-aliases-list
            (delq (assoc alias eshell-command-aliases-list)
                  eshell-command-aliases-list))
    (let ((def (assoc alias eshell-command-aliases-list))
          (alias-def
           `(,alias redef
                    (lambda (args handles)
                      (eshell-exec-command
                       (quote ,(car definition))
                       (append (quote ,(cdr definition)) args) handles)))))
          (if def
              (setq eshell-command-aliases-list
                    (delq (assoc alias eshell-command-aliases-list)
                          eshell-command-aliases-list)))
          (setq eshell-command-aliases-list
                (cons alias-def
                      eshell-command-aliases-list))))
  nil)

(defun eshell-define (var-alias definition)
  "Define an VAR-ALIAS using DEFINITION."
  (if (not definition)
      (setq eshell-variable-aliases-list
            (delq (assoc var-alias eshell-variable-aliases-list)
                  eshell-variable-aliases-list))
    (let ((def (assoc var-alias eshell-variable-aliases-list))
          (alias-def
           `(,var-alias
             (quote ,(if (= (length definition) 1)
                         (car definition)
                       definition)))))
          (if def
              (setq eshell-variable-aliases-list
                    (delq (assoc var-alias eshell-variable-aliases-list)
                          eshell-variable-aliases-list)))
          (setq eshell-variable-aliases-list
                (cons alias-def
                      eshell-variable-aliases-list))))
  nil)

(defun eshell-export (&rest sets)
  "This alias allows for \"export\" to act as bash users expect."
  (while sets
    (if (string-match "^\\([^=]+\\)=\\(.*\\)" (car sets))
        (setenv (match-string 1 (car sets))
                (match-string 2 (car sets))))
    (setq sets (cdr sets))))

(defun eshell-setq-alias (args handles)
  "Allow command-ish use of `setq'."
  (let ((final-args (list t))
        last-value)
    (while args
      (let ((sym (intern (cdar args)))
            (val (eshell-eval-arg (cadr args))))
        (nconc final-args (list sym val))
        (setq last-value (set sym val)
              args (cddr args))))
    (setq eshell-last-arguments (cdr final-args)
          eshell-command-depth (1+ eshell-command-depth))
    (eshell-printn last-value handles)
    (eshell-finish-command handles t)))

(defun eshell-if-alias (args handles)
  "Making `if' available from command line."
  (if (eshell-eval-arg (car args))
      (eshell-exec-command (cadr args) nil handles)
    (eshell-exec-command (nth 2 args) (nthcdr 3 args) handles)))

;; Don't print any output if there are no processes active.
(defun eshell-jobs-alias (args handles)
  "List processes, if there are any."
  (if (process-list)
      (eshell-insert-lisp-command 'list-processes args handles)
    (setq eshell-command-depth (1+ eshell-command-depth))
    (eshell-finish-command handles t)))

;; Convert lisp arguments to their corresponding process id, for the
;; benefit of kill.  `eshell-last-arguments' is not being set here,
;; since the arguments get all jumbled up.
(defun eshell-kill-alias (args handles)
  "Kill processes, buffers, symbol or files."
  (setq eshell-command-depth (1+ eshell-command-depth))
  (let (id-args process-args)
    (while args
      (if (or (eq 'process (caar args))
              (and (eq 'argument (caar args))
                   (string-match "^[A-Za-z/][A-Za-z0-9<>/]+$"
                                 (cdar args))))
          ;; What about when $lisp-variable is possible here?
          ;; It could very well name a process.
          (setq process-args
                (cons (get-process (cdar args))
                      process-args))
        (setq id-args (cons (car args) id-args)))
      (setq args (cdr args)))
    (setq process-args (reverse process-args)
          id-args (reverse id-args))
    (while process-args
      (eshell-funcalln handles 'kill-process (car process-args))
      (setq process-args (cdr process-args)))
    (if id-args
        (eshell-insert-disk-command
         "kill" (eshell-eval-args id-args) handles)
      (eshell-finish-command handles t))))

;; For the lisp `cd' function, no arguments means go to home.  "cd -"
;; will return to the last directory that the buffer was in.  Also,
;; don't bother echoing the result.
(defun eshell-cd-alias (args handles)
  "Alias to extend the behavior of `cd'."
  (setq eshell-last-arguments (eshell-eval-args args)
        args (car eshell-last-arguments)
        eshell-command-depth (1+ eshell-command-depth))
  (when (and (stringp args)
             (string= args "-"))
    (setq args eshell-last-cd-location))
  (setq eshell-last-cd-location default-directory
        dired-directory (or args "~"))
  (if (numberp dired-directory)
      (setq dired-directory (number-to-string dired-directory)))
  (condition-case err
      (let ((result (cd dired-directory)))
        (and result
             eshell-cd-shows-directory
             (eshell-printn result handles))
        (eshell-finish-command handles t))
    (error
     (eshell-errorn (error-message-string err) handles)
     (eshell-finish-command handles nil))))

;; "pwd" prints "Direction NAME/" in lisp.  I prefer just "NAME".
(defun eshell-pwd-alias (args handles)
  "Change output from `pwd` to be cleaner."
  (setq eshell-command-depth (1+ eshell-command-depth))
  (let ((path (pwd)))
    (if (string-match "^Directory \\(.+\\)/$" path)
        (setq path (replace-match "\\1" t nil path)))
    (if eshell-pwd-convert-function
        (setq path (funcall eshell-pwd-convert-function path)))
    (eshell-printn path handles)
    (eshell-finish-command handles t)))

(defun eshell-rm-alias (args handles)
  "Implementation of \"rm\" in Lisp, with ARGS, to HANDLES.
This is implemented to call either `delete-file', `kill-buffer',
`kill-process', or `unintern', depending on the nature of the
argument."
  (setq eshell-last-arguments (eshell-eval-args args)
        args (eshell-flatten-list eshell-last-arguments)
        eshell-command-depth (1+ eshell-command-depth))
  (if (string-match "^-"(car args))
      (eshell-insert-disk-command "rm" eshell-last-arguments handles)
    (while args
      (cond
       ((bufferp (car args))
        (eshell-funcalln handles 'kill-buffer (car args)))
       ((processp (car args))
        (eshell-funcalln handles 'kill-process (car args)))
       ((symbolp (car args))
        (eshell-funcalln handles 'unintern (car args)))
       ((not (string-match "^\\.\\.?$" (car args)))
        (eshell-funcalln handles 'delete-file (car args))))
      (setq args (cdr args)))
    (eshell-finish-command handles t)))

;; this is from GNUS, and really should be make part of Emacs some day
(defsubst eshell-time-less-p (t1 t2)
  "Say whether time T1 is less than time T2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (nth 1 t1) (nth 1 t2)))))

(defun eshell-ls-alias (args handles)
  "Implementation of \"ls\" in Lisp, passing ARGS, with output to HANDLES."
  (setq eshell-last-arguments (eshell-eval-args args)
        args (eshell-flatten-list eshell-last-arguments)
        eshell-command-depth (1+ eshell-command-depth))
  (let* (have-nondirs long-listing reverse-list (switches "")
         sort-by-modtime show-all unrecognized dir-literal)
    (while (and args
                (stringp (car args))
                (string-match "^-\\([a-zA-z]+\\)" (car args)))
      (setq switches (concat (match-string 1 (car args)) switches)
            args (cdr args)))
    (let ((index 0))
      (while (< index (length switches))
        (cond
         ((eq (aref switches index) ?l)
          (setq long-listing t))
         ((eq (aref switches index) ?r)
          (setq reverse-list t))
         ((eq (aref switches index) ?a)
          (setq show-all t))
         ((eq (aref switches index) ?t)
          (setq sort-by-modtime t))
         ((eq (aref switches index) ?d)
          (setq dir-literal t))
         (t
          (setq unrecognized t)))
      (setq index (1+ index))))
    (if unrecognized
        (eshell-insert-disk-command
         "ls" (or eshell-last-arguments '("-1")) handles)
      (if (not args)
          (setq args (list default-directory)))
      (setq args
            (sort args                  ; directories first
                  (function
                   (lambda (l r)
                     (and (file-directory-p r)
                          (not (file-directory-p l)))))))
      (while args
        (let ((files
               (if (or dir-literal
                       (not (file-directory-p (car args))))
                   (progn
                     (setq have-nondirs t)
                     (list (car args)))
                 (if (or have-nondirs (cdr args))
                     (eshell-printn
                      (concat (and have-nondirs "\n")
                              (file-name-nondirectory
                               (car args)) ":") handles))
                 (if long-listing
                     (eshell-printn
                      (concat "total "
                              (int-to-string
                               (nth 7 (file-attributes
                                       (car args))))) handles))
                 (directory-files (expand-file-name (car args)) t nil t))))
          (setq files
                (sort files
                      (function
                       (lambda (l r)
                         (let ((result
                                (if sort-by-modtime
                                    (let ((lt (nth 5 (file-attributes l)))
                                          (rt (nth 5 (file-attributes r))))
                                      (if (not (equal lt rt))
                                          (eshell-time-less-p rt lt)
                                        (string-lessp l r)))
                                  (string-lessp l r))))
                           (if reverse-list (not result) result))))))
          (while files
            (unless (and (not show-all)
                         (not (string-match "^\\."
                                            (file-name-nondirectory (car args))))
                         (string-match "^\\."
                                       (file-name-nondirectory (car files))))
              (if (not (file-exists-p (car files)))
                  (eshell-errorn
                   (concat (car files)
                           ": No such file or directory") handles)
                (if (not long-listing)
                    (eshell-printn (file-name-nondirectory (car files))
                                   handles)
                  (let ((attrs (file-attributes (car files))))
                    (eshell-printn
                     (format
                      "%s%4d %-8s %-8s %8d %s %-s%s"
                      (nth 8 attrs) (nth 1 attrs)
                      (nth 2 attrs) (nth 3 attrs)
                      (nth 7 attrs)
                      (format-time-string
                       (concat
                        "%b %e "
                        (if (not (= (nth 5 (decode-time (current-time)))
                                    (nth 5 (decode-time (nth 5 attrs)))))
                            " %Y"
                          "%H:%M")) (nth 5 attrs))
                      (file-name-nondirectory (car files))
                      (let ((symlink (file-symlink-p (car files))))
                        (if symlink
                            (concat " -> " symlink)
                          ""))) handles)))))
            (setq files (cdr files))))
        (setq args (cdr args)))
      (eshell-finish-command handles t))))

(defun eshell-history-alias (args handles)
  "List in help buffer the buffer's input history."
  (setq eshell-last-arguments (eshell-eval-args args)
        eshell-command-depth (1+ eshell-command-depth))
  (if (or (not (ring-p comint-input-ring))
	  (ring-empty-p comint-input-ring))
      (eshell-printn "No history" handles)
    (let* ((history nil)
           (index (1- (if (and eshell-last-arguments
                               (integerp (car eshell-last-arguments)))
                          (car eshell-last-arguments)
                        (ring-length comint-input-ring))))
           (ref (- (ring-length comint-input-ring) index)))
      ;; We have to build up a list ourselves from the ring vector.
      (while (>= index 0)
        (eshell-printn (format "%5d  %s" ref
                               (ring-ref comint-input-ring index))
                       handles)
        (setq index (1- index)
              ref (1+ ref)))))
  (eshell-finish-command handles t))

;; Provide:

(provide 'eshell)

(run-hooks 'eshell-load-hook)

;;; eshell.el ends here
