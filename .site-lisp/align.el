;;; align --- align text to a specific column, by regexp

;; Copyright (C) 1999 John Wiegley

;; Author: John Wiegley <johnw@oneworld.new-era.com>
;; Created: 21 Mar 1996
;; Version: 2.6
;; Keywords: convenience languages lisp
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

;; This mode allows you to align regions in a context-sensitive fashion.
;; The classic use is to align assignments:
;;
;;    int a = 1;
;;    short foo = 2;
;;    double blah = 4;
;;
;; becomes
;;
;;    int    a    = 1;
;;    short  foo  = 2;
;;    double blah = 4;

;;; Usage:

;; To use the aligner, simply call put the following in your .emacs
;; file; then mark a region and type `M-x align'.
;;
;;    (autoload 'align "align" nil t)

;; To copy the rules for an entire mode, so that they are available in
;; another mode (such as might be desired for jde-mode, which is
;; similar to java-mode), you can use this code in your .emacs file:
;;
;;    (setq align-mode-alist
;;          (cons (cons 'jde-mode
;;                      (copy-alist (cdr (assq 'java-mode
;;                                             align-mode-alist))))
;;                align-mode-alist))

;; In some programming modes, it's useful to have the aligner run only
;; after indentation is performed.  This can be done easily using
;; advice:
;;
;;    (defadvice align (before align-after-indent (beg end rules func)
;;                      activate compile)
;;      "Always indent the region before aligning it."
;;      (when (and beg end)
;;        (setq end (copy-marker end))
;;        (indent-region beg end)))

;;; History:

;; New in 2.6:

;; * Assignment operations now correctly line up, as well as
;;   right-justifying multi-character assignment operations.
;;
;;   Example:
;;       x <<= foo;
;;       yyy -= blah;
;;   becomes
;;       x   <<= foo;
;;       yyy  -= blah;
;;
;; * The lists `align-rules-list' and `align-exclude-rules-list' are
;;   new, and can be customized.  This allowed me to remove the list
;;   manipulation functions that used to be part of 2.5.
;;
;; * `align-after-indent' has been removed, to break the tie to the
;;   indent package.  Since users can set that up easily on their own,
;;   it made little sense.
;;
;; * Added `align-load-hook'.
;;
;; * If no alignment is done, the buffer is not modified.
;;
;; * Display a percentage complete figure in the modeline, for large
;;   regions (larger than `align-large-region' characters).
;;
;; * Added rules for aligning the "\=" and "\<" characters using by the
;;   LaTeX tabbing environment.
;;
;; * Added default rules for vhdl-mode.  The question is now arising:
;;   who is responsible for maintaining these?  Once align.el becomes
;;   part of the Emacs distribution, perhaps we'll see each module
;;   adding its own rules to `align-mode-alist'.
;;
;; * The command `align-current' will align the current "alignment
;;   section" that point is in.  Note that if `align-region-separate'
;;   is set to `group', each rule might have its own definition of
;;   what the current section is.
;;
;; * When giving a prefix argument to `align-regexp', and the amount
;;   of spacing given is a negative number, the absolute value of that
;;   number will be used as the column to align to.  The prompt
;;   indicates this.  You can also specify that a parenthesis group
;;   marks justification by making the group value negative.  This
;;   does not work where multiple groups are to be specified (and if
;;   things are getting that complex, you ought to consider writing a
;;   proper rule, rather than calling `align-regexp' interactively).
;;
;; * `align-highlight-rule' will set the face property of the
;;   text groups that would have been modified to the "region" face.
;;   This function is mainly for debugging.
;;
;; * The rules for aligning comments and C backslashes no longer
;;   change their behavior based on the presence of a prefix argument.
;;   Since you can easily change the comment column, this didn't
;;   appear useful enough; especially since it has the potential for
;;   conflicting with other rules that are sensitive to the prefix
;;   argument.
;;
;;   Also, when a prefix argument is not specified, it assumes that
;;   the whitespace region to be modified occurs immediately before
;;   the regexp specified.  So that aligning a series of opening
;;   parentheses would require only entering "(", instead of
;;   "\\(\\s-*\\)(".
;;
;;   When the prefix argument is specified, it inserts the typical
;;   "whitespace grouping match" pattern, assuming that the user will
;;   want it most of the time.
;;
;; * New rule attributes, or meanings of attributes:
;;
;; ** `case-fold'.  This overrides the value of `case-fold-search'
;;    when searching for `regexp'.
;;
;; ** `group' may now be a list of numbers.  This is the proper way to
;;    handle text that should be aligned both before and after a
;;    delimiter.  Otherwise, the rules might get confused.
;;
;; ** `run-if'.  This causes the rule not even to be attempted if it
;;    evaluates to nil.  Like the old "test" argument.  Differs from
;;    `valid' in that it happens before the rule ever fires.
;;
;; ** `justify'.  The way it works is this: only the initial
;;    whitespace within the parenthesis group identified by the
;;    `group' attribute will be contracted/expanded to align the
;;    alignment character.  But because there might be non-whitespace
;;    in that group, it will also be moved so that the alignment
;;    character falls into the correct column.  The end result is that
;;    any characters in that group become right-justified.
;;
;;    The practical upshot of this is that outdenting of *'s is now
;;    supported in C/C++ mode.
;;
;;    Note that I could have made computation of the justification
;;    automatic, but rather than have other rules pay for it when they
;;    never use it... (`re-search-forward' is being called enough
;;    times already).
;;
;; ** `separate'.  Each rule can specify its own section separator,
;;    which will override the value of `align-region-separate'.
;;
;; ** `regexp' can be set to a function, in which case that function
;;    will called as a direct substitute for `re-search-forward'.
;;    That is, this function is responsible for setting up the
;;    match-data (see `set-match-data' and `match-data') as if
;;    `re-search-forward' had been called.  It must return nil if
;;    nothing was found.  The `group' attribute is still used to
;;    reference the match-data set by this function.

;;; Code:

(defconst align-version "2.6"
  "This version of align.")

(defgroup align nil
  "Align text to a specific column, by regexp."
  :group 'fill)

;;; User Variables:

(defcustom align-load-hook nil
  "*Hook that gets run after the aligner has been loaded."
  :type 'hook
  :group 'align)

(defcustom align-indent-before-aligning nil
  "*If non-nil, indent the marked region before aligning it."
  :type 'boolean
  :group 'align)

(defcustom align-default-spacing 1
  "*An integer that represents the default amount of padding to use.
If `align-to-tab-stop' is non-nil, this will represent the number of
tab stops to use for alignment, rather than the number of spaces.
Each alignment rule can optionally override both this variable.  See
`align-mode-alist'."
  :type 'integer
  :group 'align)

(defcustom align-to-tab-stop 'indent-tabs-mode
  "*If non-nil, alignments will always fall on a tab boundary."
  :type '(choice integer symbol)
  :group 'align)

(defcustom align-region-heuristic 500
  "*If non-nil, used as a heuristic by `align-current'.
Since each alignment rule can possibly have its own set of alignment
sections (whenever `align-region-separate' is non-nil, and not a
string), this heuristic is used to determine how far before and after
point we should search in looking for a region separator.  Larger
values can mean slower perform in large files, although smaller values
may cause unexpected behavior at times."
  :type 'integer
  :group 'align)

(defcustom align-large-region 10000
  "*If an integer, defines what constitutes a \"large\" region.
If nil,then no messages will ever be printed to the minibuffer."
  :type 'integer
  :group 'align)

(defcustom align-dq-string-modes
  '(emacs-lisp-mode lisp-mode scheme-mode
    c++-mode c-mode java-mode
    perl-mode cperl-mode
    vhdl-mode)
  "*A list of modes where double quoted strings should be excluded."
  :type '(repeat symbol)
  :group 'align)

(defcustom align-open-comment-modes
  '(emacs-lisp-mode lisp-mode scheme-mode
    c++-mode java-mode
    perl-mode cperl-mode
    makefile-mode
    tex-mode plain-tex-mode latex-mode slitex-mode
    vhdl-mode)
  "*A list of modes with a single-line comment syntax.
These are comments as in Lisp, which have a beginning but, end with
the line (i.e., `comment-end' is an empty string)."
  :type '(repeat symbol)
  :group 'align)
  
(defcustom align-c++-modes
  '(c++-mode c-mode java-mode)
  "*A list of modes whose syntax resembles C/C++."
  :type '(repeat symbol)
  :group 'align)
  
(defcustom align-perl-modes
  '(perl-mode cperl-mode)
  "*A list of modes whose syntax resembles C/C++."
  :type '(repeat symbol)
  :group 'align)
  
(defcustom align-lisp-modes
  '(emacs-lisp-mode lisp-interaction-mode lisp-mode scheme-mode)
  "*A list of modes whose syntax resembles C/C++."
  :type '(repeat symbol)
  :group 'align)
  
(defcustom align-tex-modes
  '(tex-mode plain-tex-mode latex-mode slitex-mode)
  "*A list of modes whose syntax resembles TeX (and family)."
  :type '(repeat symbol)
  :group 'align)
  
(defcustom align-text-modes
  '(text-mode outline-mode)
  "*A list of modes whose syntax resembles TeX (and family)."
  :type '(repeat symbol)
  :group 'align)
  
(defcustom align-region-separate "^\\s-*[{}]?\\s-*$"
  "*Select the method by which alignment sections will be separated.
If this is a symbol, that symbol's value will be used.

For the sake of clarification, consider the following example, which
will be referred to in the descriptions below.

    int alpha = 1; /* one */
    double beta = 2.0;
    long gamma; /* ten */

    unsigned int delta = 1; /* one */
    long double epsilon = 3.0;
    long long omega; /* ten */

The possible settings for `align-region-separate' are:

 `entire'  The entire region being aligned will be considered as a
           single alignment section.  Assuming that comments were not
           being aligned to a particular column, the example would
           become:

             int          alpha    = 1;   /* one */
             double       beta     = 2.0;
             long         gamma;          /* ten */

             unsigned int delta    = 1;   /* one */
             long double  epsilon;
             long long    chi      = 10;  /* ten */

 `group'   Each contiguous set of lines where a specific alignment
           occurs is considered a section for that alignment rule.
           Note that each rule will may have any entirely different
           set of section divisions than another.

             int    alpha = 1; /* one */
             double beta  = 2.0;
             long   gamma; /* ten */

             unsigned int delta = 1; /* one */
             long double  epsilon;
             long long    chi = 10; /* ten */

 `largest' When contiguous rule sets overlap, the largest section
           described will be taken as the alignment section for each
           rule touched by that section.

             int    alpha = 1;   /* one */
             double beta  = 2.0;
             long   gamma;       /* ten */

             unsigned int delta    = 1;  /* one */
             long double  epsilon;
             long long    chi      = 10; /* ten */

           NOTE: This option is not supported yet, due to algorithmic
           issues which haven't been satisfactorily resolved.  There
           are ways to do it, but they're both ugly and resource
           consumptive.

 regexp    A regular expression string which defines the section
           divider.  If the mode you're in has a consistent divider
           between sections, the behavior will be very similar to
           `largest', and faster.  But if the mode does not use clear
           separators (for example, if you collapse your braces onto
           the preceding statement in C or perl), `largest' is
           probably the better alternative.

 function  A function that will be passed the beginning and ending
           locations of the region in which to look for the section
           separator.  At the very beginning of the attempt to align,
           both of these parameters will be nil, in which case the
           function should return non-nil if it wants each rule to
           define its own section, or nil if it wants the largest
           section found to be used as the common section for all rules
           that occur there.

 list      A list of markers within the buffer that represent where
           the section dividers lie.  Be certain to use markers!  For
           when the aligning begins, the ensuing contract/expanding of
           whitespace will throw off any non-marker positions.

           This method is intended for use in Lisp programs, and not
           by the user."
  :type '(choice
          (const :tag "Entire region is one section" entire)
          (const :tag "Align by contiguous groups" group)
;         (const largest)
          (regexp :tag "Regexp defines section boundaries")
          (function :tag "Function defines section boundaries"))
  :group 'align)

(put 'align-region-separate 'risky-local-variable t)

(defcustom align-rules-list
  `((second-arg
     (regexp    . "\\(^\\s-+[^( \t\n]\\|(\\(\\S-+\\)\\s-+\\)\\S-+\\(\\s-+\\)")
     (group     . 3)
     (modes     . align-lisp-modes)
     (run-if    . current-prefix-arg))

    (alist-dot
     (regexp    . "\\(\\s-*\\)\\.\\(\\s-*\\)")
     (group     . (1 2))
     (modes     . align-lisp-modes))

    (open-comment
     (regexp    . (lambda (end)
                    (re-search-forward
                     (concat "[^ \t\n\\\\]" comment-start
                             "\\(.+\\)$") end t)))
     (modes     . align-open-comment-modes))
    
    (macro-definition
     (regexp    . "^\\s-*#\\s-*define\\s-+\\S-+\\(\\s-+\\)")
     (modes     . align-c++-modes))

    (variable-declaration
     (regexp    . ,(concat "[*&0-9A-Za-z_]>?[&*]*\\(\\s-+[*&]*\\)"
                           "[A-Za-z_][0-9A-Za-z:_]*\\s-*\\(\\()\\|"
                           "=[^=\n].*\\|(.*)\\|\\(\\[.*\\]\\)*\\)?"
                           "\\s-*[;,]\\|)\\s-*$\\)"))
     (group     . 1)
     (modes     . align-c++-modes)
     (justify   . t)
     (valid     . (not
                   (or (save-excursion
                         (goto-char (match-beginning 1))
                         (backward-word 1)
                         (looking-at
                          "\\(goto\\|return\\|new\\|delete\\|throw\\)"))
                       (eq (caar (c-guess-basic-syntax)) 'c)))))

    (c++-assignment
     (regexp    . ,(concat "[^-=!^&*+<>/| \t\n]\\(\\s-*[-=!^&*+<>/|]*\\)"
                           "=\\(\\s-*\\)\\([^= \t\n]\\|$\\)"))
     (group     . (1 2))
     (modes     . align-c++-modes)
     (justify   . t)
     (tab-stop  . nil))

    (perl-assignment
     (regexp    . "[^=!^&*-+<>/| \t\n]\\(\\s-*\\)=[~>]?\\(\\s-*\\)\\([^>= \t\n]\\|$\\)")
     (group     . (1 2))
     (modes     . align-perl-modes)
     (tab-stop  . nil))

    (make-assignment
     (regexp    . "^\\s-*\\w+\\(\\s-*\\):?=\\(\\s-*\\)\\([^\t\n \\\\]\\|$\\)")
     (group     . (1 2))
     (modes     . '(makefile-mode))
     (tab-stop  . nil))

    (c++-comma-delimiter
     (regexp    . ",\\(\\s-*\\)[^/ \t\n]")
     (repeat    . t)
     (modes     . align-c++-modes)
     (valid     . (let ((syntax (caar (c-guess-basic-syntax))))
                    (memq syntax '(brace-list-intro brace-list-entry
                                   brace-entry-open)))))

    ;; With a prefix argument, comma delimiter will be aligned.  Since
    ;; perl-mode doesn't give us enough syntactic information (and we
    ;; don't do our own parsing yet), this rule is too destructive to
    ;; run normally.
    (perl-comma-delimiter
     (regexp    . ",\\(\\s-*\\)[^# \t\n]")
     (repeat    . t)
     (modes     . align-perl-modes)
     (run-if    . current-prefix-arg))

    (c++-comment
     (regexp    . "\\(\\s-*\\)\\(//.*\\|/\\*.*\\*/\\s-*\\)$")
     (modes     . align-c++-modes)
     (column    . comment-column)
     (valid     . (save-excursion
                    (goto-char (match-beginning 1))
                    (not (bolp)))))

    (macro-line-continuation
     (regexp    . "\\(\\s-*\\)\\\\$")
     (modes     . (append align-c++-modes '(makefile-mode)))
     (column    . c-backslash-column)
     (valid     . (let ((syntax (caar (c-guess-basic-syntax))))
                    (or (eq syntax 'cpp-macro)
                        (eq syntax 'cpp-macro-cont)))))

    (c++-chain-logic
     (regexp    . "\\(\\s-*\\)\\(&&\\|||\\)")
     (modes     . align-c++-modes)
     (valid     . (save-excursion
                    (goto-char (match-end 2))
                    (or (looking-at "\\s-*/[*/]")
                        (looking-at "\\s-*$")))))

    (perl-chain-logic
     (regexp    . "\\(\\s-*\\)\\(&&\\|||\\|\\<and\\>\\|\\<or\\>\\)")
     (modes     . align-perl-modes)
     (valid     . (save-excursion
                    (goto-char (match-end 2))
                    (or (looking-at "\\s-*#")
                        (looking-at "\\s-*$")))))

    (table-separator
     (regexp    . "\\(\\s-*\\)&\\(\\s-*\\)")
     (group     . (1 2))
     (modes     . align-tex-modes)
     (repeat    . t))

    (tabbing-separator
     (regexp    . "\\(\\s-*\\)\\\\[=>]\\(\\s-*\\)")
     (group     . (1 2))
     (modes     . align-tex-modes)
     (repeat    . t)
     (run-if    . (eq major-mode 'latex-mode)))

    (record-break
     (regexp    . "\\(\\s-*\\)\\\\\\\\")
     (modes     . align-tex-modes))

    ;; With a numeric prefix argument, or C-u, space delimited text
    ;; tables will be aligned.
    (text-column
     (regexp    . "\\(^\\|\\S-\\)\\(\\s-+\\)\\(\\S-\\|$\\)")
     (group     . 2)
     (modes     . align-text-modes)
     (repeat    . t)
     (run-if    . (and current-prefix-arg
                       (not (eq '- current-prefix-arg)))))

    ;; With a negative prefix argument, lists of dollar figures will
    ;; be aligned.
    (dollar-figure
     (regexp    . "\\$?\\(\\s-+[0-9]+\\)\\.")
     (modes     . align-text-modes)
     (justify   . t)
     (run-if    . (eq '- current-prefix-arg)))

    (vhdl-declaration
     (regexp    . "\\(signal\\|variable\\|constant\\)\\(\\s-+\\)\\S-")
     (group     . 2)
     (modes     . '(vhdl-mode)))

    (vhdl-case
     (regexp    . "\\(others\\|[^ \t\n=<]\\)\\(\\s-*\\)=>\\(\\s-*\\)\\S-")
     (group     . (2 3))
     (modes     . '(vhdl-mode))
     (valid     . (not (string= (downcase (match-string 1))
                                "others"))))

    (vhdl-colon
     (regexp    . "[^ \t\n:]\\(\\s-*\\):\\(\\s-*\\)[^=\n]")
     (group     . (1 2))
     (modes     . '(vhdl-mode)))

    (direction
     (regexp    . ":\\s-*\\(in\\|out\\|inout\\|buffer\\)\\(\\s-*\\)")
     (group     . 2)
     (modes     . '(vhdl-mode)))

    (sig-assign
     (regexp    . "[^ \t\n=<]\\(\\s-*\\)<=\\(\\s-*\\)\\S-")
     (group     . (1 2))
     (modes     . '(vhdl-mode)))

    (var-assign
     (regexp    . "[^ \t\n:]\\(\\s-*\\):=")
     (modes     . '(vhdl-mode)))

    (use-entity
     (regexp    . "\\(\\s-+\\)use\\s-+entity")
     (modes     . '(vhdl-mode))))
  "*An list describing all of the available alignment rules.
The format is:

   ((TITLE
     (ATTRIBUTE . VALUE) ...)
    ...)

The following attributes are meaningful:

`regexp'    This required attribute must be either a string describing
            a regular expression, or a function (described below).
            For every line within the section that this regular
            expression matches, the given rule will be applied to that
            line.  The exclusion rules denote which part(s) of the
            line should not be modified; the alignment rules cause the
            identified whitespace group to be contracted/expanded such
            that the \"alignment character\" (the character
            immediately following the identified parenthesis group),
            occurs in the same column for every line within the
            alignment section (see `align-region-separate' for a
            description of how the region is broken up into alignment
            sections).

            The `regexp' attribute describes how the text should be
            treated.  Within this regexp, there must be at least one
            group of characters (typically whitespace) identified by
            the special opening and closing parens used in regexp
            expressions (`\\\\(' and `\\\\)') (see the Emacs manual on
            the syntax of regular expressions for more info).

            If `regexp' is a function, it will be called as a
            replacement for `re-search-forward'.  This means that it
            should return nil if nothing is found to match the rule,
            or it should set the match data appropriately, move point
            to the end of the match, and return the value of point.

`group'     For exclusion rules, the group identifies the range of
            characters that should be ignored.  For alignment rules,
            these are the characters that will be deleted/expanded for
            the purposes of alignment.  The \"alignment character\" is
            always the first character immediately following this
            parenthesis group.  This attribute may also be a list of
            integer, in which case multiple alignment characters will
            be aligned, with the list of integer identifying the
            whitespace groups which precede them.  The default for
            this attribute is 1.

`modes'     The `modes' attribute, if set, should name a list of
            major modes -- or evaluate to such a value -- in which the
            rule is valid.  If not set, the rule will apply to all
            modes.

`case-fold' If `regexp' is an ordinary regular expression string
            containing alphabetic character, sometimes you may want
            the search to proceed case-insensitively (for languages
            that ignore case, such as pascal for example).  In that
            case, set `case-fold' to nil, and the regular expression
            search will ignore case.  If `regexp' is set to a
            function, that function must handle the job of ignoring
            case by itself.

`tab-stop'  If the `tab-stop' attribute is set, and non-nil, the
            alignment character will always fall on a tab stop
            (whether it uses tabs to get there or not depends on the
            value of `indent-tabs-mode').  If the `tab-stop' attribute
            is set to nil, tab stops will never be used.  Otherwise,
            the value of `align-to-tab-stop' determines whether or not
            to align to a tab stop.

`repeat'    If the `repeat' attribute is present, and non-nil, the
            rule will be applied to the line continuously until no
            further matches are found.

`valid'     If the `valid' attribute is set, it will be used to
            determine whether the rule should be invoked.  This form
            is evaluated after the regular expression match has been
            performed, so that it is possible to use the results of
            that match to determine whether the alignment should be
            performed.  The buffer should not be modified during the
            evaluation of this form.

`run-if'    Like `valid', the `run-if' attribute tests whether the
            rule should be run at all -- even before any searches are
            done to determine if the rule applies to the alignment
            region.  This can save time, since `run-if' will only be
            run once for each rule.  If it returns nil, the rule will
            not be attempted.

`column'    For alignment rules, if the `column' attribute is set --
            which must be an integer, or a symbol whose value is an
            integer -- it will be used as the column in which to align
            the alignment character.  If the text on a particular line
            happens to overrun that column, a single space character,
            or tab stop (see `align-to-tab-stop') will be added
            between the last text character and the alignment
            character.

`spacing'   Alignment rules may also override the amount of spacing
            that would normally be used by providing a `spacing'
            attribute.  This must be an integer.  See
            `align-default-spacing' for more details on spacing, tab
            stops, and how to indicate how much spacing should be
            used.  If TAB-STOP is present, it will override the value
            of `align-to-tab-stop' for that rule.

`justify'   It is possible with `regexp' and `group' to identify a
            character group that contains more than just whitespace
            characters.  By default, any non-whitespace characters in
            that group will also be deleted while aligning the
            alignment character.  However, if the `justify' attribute
            is set to a non-nil value, only the initial whitespace
            characters within that group will be deleted.  This has
            the effect of right-justifying the characters that remain,
            and can be used for outdenting or just plain old right-
            justification.

`separate'  Each rule can define its own section separator, which
            describes how to identify the separation of \"sections\"
            within the region to be aligned.  Setting the `separate'
            attribute overrides the value of `align-region-separate'
            (see the documentation of that variable for possible
            values), and any separation argument passed to `align'."
  :type '(repeat
          (cons
           :tag "Alignment rule"
           (symbol :tag "Title")
           (cons :tag "Required attributes"
                 (cons :tag "Regexp"
                       (const :tag "(Regular expression to match)" regexp)
                       (choice :value "\\(\\s-+\\)" regexp function))
                 (repeat
                  :tag "Optional attributes"
                  (choice
                   (cons :tag "Repeat"
                         (const :tag "(Repeat this rule throughout line)"
                                repeat)
                         (boolean :value t))
                   (cons :tag "Paren group"
                         (const :tag "(Parenthesis group to use)" group)
                         (choice :value 2
                                 integer (repeat integer)))
                   (cons :tag "Modes"
                         (const :tag "(Modes where this rule applies)" modes)
                         (choice :value '(text-mode) sexp))
                   (cons :tag "Case-fold"
                         (const :tag "(Should case be ignored for this rule)"
                                case-fold)
                         (boolean :value t))
                   (cons :tag "To Tab Stop"
                         (const :tag "(Should rule align to tab stops)"
                                tab-stop)
                         (boolean :value nil))
                   (cons :tag "Valid"
                         (const :tag "(Return non-nil if rule is valid)"
                                valid)
                         (sexp :value t))
                   (cons :tag "Run If"
                         (const :tag "(Return non-nil if rule should run)"
                                run-if)
                         (sexp :value t))
                   (cons :tag "Column"
                         (const :tag "(Column to fix alignment at)" column)
                         (choice :value comment-column
                                 integer symbol))
                   (cons :tag "Spacing"
                         (const :tag "(Amount of spacing to use)" spacing)
                         (integer :value 1))
                   (cons :tag "Justify"
                         (const :tag "(Should text be right justified)"
                                justify)
                         (boolean :value t))
                   ;; make sure this stays up-to-date with any changes
                   ;; in `align-region-separate'
                   (cons :tag "Separate"
                         (const :tag "(Separation to use for this rule)"
                                separate)
                         (choice :value "^\\s-*$"
                                 (const entire)
                                 (const group)
;                                (const largest)
                                 regexp function)))))))
  :group 'align)
           
(put 'align-alignment-rules-list 'risky-local-variable t)

(defcustom align-exclude-rules-list
  '((exc-dq-string
     (regexp    . "\"\\([^\"\n]+\\)\"")
     (repeat    . t)
     (modes     . align-dq-string-modes))

    (exc-sq-string
     (regexp    . "'\\([^'\n]+\\)'")
     (repeat    . t)
     (modes     . align-perl-modes))

    (exc-open-comment
     (regexp    . (lambda (end)
                    (re-search-forward
                     (concat "[^ \t\n\\\\]" comment-start
                             "\\(.+\\)$") end t)))
     (modes     . align-open-comment-modes))

    (exc-c-comment
     (regexp    . "/\\*\\(.+\\)\\*/")
     (repeat    . t)
     (modes     . align-c++-modes))
    
    (exc-func-params
     (regexp    . "(\\([^)\n]+\\))")
     (repeat    . t)
     (modes     . align-c++-modes))

    (exc-cpp-macro
     (regexp    . "^\\s-*#\\s-*\\(if\\w*\\|endif\\)\\(.*\\)$")
     (group     . 2)
     (modes     . align-c++-modes))

    (exc-vhdl-comment
     (regexp    . "--\\(\\s-*\\(in\\|out\\|inout\\|buffer\\)\\s-+\\)?\\(\\S-.+\\)$")
     (group     . 3)
     (modes     . '(vhdl-mode))))
  "*An list describing text that should be excluded from alignment.
See the documentation for `align-rules-list' for more info."
  :type '(repeat
          (cons
           :tag "Exclusion rule"
           (symbol :tag "Title")
           (cons :tag "Required attributes"
                 (cons :tag "Regexp"
                       (const :tag "(Regular expression to match)" regexp)
                       (choice :value "\\(\\s-+\\)" regexp function))
                 (repeat
                  :tag "Optional attributes"
                  (choice
                   (cons :tag "Repeat"
                         (const :tag "(Repeat this rule throughout line)"
                                repeat)
                         (boolean :value t))
                   (cons :tag "Paren group"
                         (const :tag "(Parenthesis group to use)" group)
                         (choice :value 2
                                 integer (repeat integer)))
                   (cons :tag "Modes"
                         (const :tag "(Modes where this rule applies)" modes)
                         (sexp :value (text-mode)))
                   (cons :tag "Case-fold"
                         (const :tag "(Should case be ignored for this rule)"
                                case-fold)
                         (boolean :value t)))))))
  :group 'align)

(put 'align-exclude-rules-list 'risky-local-variable t)

;;; Internal Variables:

(defvar align-highlight-overlays nil
  "The current overlays highlighting the text matched by a rule.")

;;; User Functions:

;;;###autoload
(defun align (beg end &optional separate rules exclude-rules)
  "Attempt to align a region based on a set of alignment rules.
BEG and END mark the region.  If BEG and END are specifically set to
nil (this can only be done programmatically), the beginning and end of
the current alignment section will be calculated based on the location
of point, and the value of `align-region-separate' (or possibly each
rule's `separate' attribute).

If SEPARATE is non-nil, it overrides the value of
`align-region-separate' for all rules, except those that have their
`separate' attribute set.

RULES and EXCLUDE-RULES, if either is non-nil, will replace the
default rule lists defined in `align-rules-list' and
`align-exclude-rules-list'.  See `align-rules-list' for more details
on the format of these lists."
  (interactive "r")
  (let ((separator
         (or separate
             (if (symbolp align-region-separate)
                 (symbol-value align-region-separate)
               align-region-separate)
             'entire)))
    (if (not (or ;(eq separator 'largest)
                 (and (functionp separator)
                      (not (funcall separator nil nil)))))
        (align-region beg end separator
                      (or rules align-rules-list)
                      (or exclude-rules align-exclude-rules-list))
      (let ((sec-first end)
            (sec-last beg))
        (align-region beg end
                      (or exclude-rules
                          align-exclude-rules-list) nil
                      separator
                      (function
                       (lambda (b e rule)
                         (when rule
                           (setq sec-first (min sec-first b)
                                 sec-last  (max sec-last e))))))
        (if (< sec-first sec-last)
            (align-region sec-first sec-last 'entire
                          (or rules align-rules-list)
                          (or exclude-rules
                              align-exclude-rules-list)))))))

;;;###autoload
(defun align-regexp (beg end regexp &optional group spacing repeat)
  "Align the current region using an ad-hoc rule read from the minibuffer.
BEG and END mark the limits of the region.  This function will prompt
for the REGEXP to align with.  If no prefix arg was specified, you
only need to supply the characters to be lined up and any preceding
whitespace is replaced.  If a prefix arg was specified, the full regexp
with parenthesized whitespace should be supplied; it will also prompt
for which parenthesis GROUP within REGEXP to modify, the amount of
SPACING to use, and whether or not to REPEAT the rule throughout the
line.  See `align-mode-alist' for more information about these options.

For example, let's say you had a list of phone numbers, and wanted to
align them so that the opening parentheses would line up:

    Fred (123) 456-7890
    Alice (123) 456-7890
    Mary-Anne (123) 456-7890
    Joe (123) 456-7890

There is no predefined rule to handle this, but you could easily do it
using a REGEXP like \"(\". All you would have to do is to mark the
region, call `align-regexp' and type in that regular expression."
  (interactive
   (append
    (list (min (point) (mark))
          (max (point) (mark)))
    (if current-prefix-arg
        (list (read-string "Complex align using regexp: "
                           "\\(\\s-*\\)")
              (string-to-int
               (read-string
                "Parenthesis group to modify (justify if negative): " "1"))
              (string-to-int
               (read-string "Amount of spacing (or column if negative): "
                            (number-to-string align-default-spacing)))
              (y-or-n-p "Repeat throughout line? "))
      (list (concat "\\(\\s-*\\)"
                    (read-string "Align regexp: "))
            1 align-default-spacing nil))))
  (let ((rule
         (list (list nil (cons 'regexp regexp)
                     (cons 'group (abs group))
                     (if (< group 0)
                         (cons 'justify t)
                       (cons 'bogus nil))
                     (if (>= spacing 0)
                         (cons 'spacing spacing)
                       (cons 'column (abs spacing)))
                     (cons 'repeat repeat)))))
    (align-region beg end 'entire rule nil nil)))

;;;###autoload
(defun align-entire (beg end &optional rules exclude-rules)
  "Align the selected region as if it were one alignment section.
BEG and END mark the extent of the region.  If RULES or EXCLUDE-RULES
is set to a list of rules (see `align-rules-list'), it can be used to
override the default alignment rules that would have been used to
align that section."
  (interactive "r")
  (align beg end 'entire rules exclude-rules))

;;;###autoload
(defun align-current (&optional rules exclude-rules)
  "Call `align' on the current alignment section.
This function assumes you want to align only the current section, and
so saves you from having to specify the region.  If RULES or
EXCLUDE-RULES is set to a list of rules (see `align-rules-list'), it
can be used to override the default alignment rules that would have
been used to align that section."
  (interactive)
  (align nil nil nil rules exclude-rules))

;;;###autoload
(defun align-highlight-rule (beg end title &optional rules exclude-rules)
  "Highlight the whitespace which a given rule would have modified.
BEG and END mark the extent of the region.  TITLE identifies the rule
that should be highlighted.  If RULES or EXCLUDE-RULES is set to a
list of rules (see `align-rules-list'), it can be used to override the
default alignment rules that would have been used to identify the text
to be colored."
  (interactive
   (list (min (mark) (point))
         (max (mark) (point))
         (completing-read
          "Title of rule to highlight: "
          (mapcar
           (function
            (lambda (rule)
              (list (symbol-name (car rule)))))
           (append align-rules-list
                   align-exclude-rules-list)))))
  (let (face)
    (align-unhighlight-rule)
    (align-region
     beg end 'entire
     (or rules align-rules-list)
     (or exclude-rules align-exclude-rules-list)
     (function
      (lambda (b e rule)
        (if rule
            (if (equal (symbol-name (car rule)) title)
                (setq face 'region)
              (setq face nil))
          (when face
            (let ((overlay (make-overlay b e)))
              (setq align-highlight-overlays
                    (cons overlay align-highlight-overlays))
              (overlay-put overlay 'face face)))))))))

;;;###autoload
(defun align-unhighlight-rule ()
  "Remove any highlighting that was added by `align-highlight-rule'."
  (interactive)
  (while align-highlight-overlays
    (delete-overlay (car align-highlight-overlays))
    (setq align-highlight-overlays
          (cdr align-highlight-overlays))))

;;; Internal Functions:

(defun align-new-section-p (beg end separator)
  "Is there a section divider between BEG and END?
SEPARATOR specifies how to look for the section divider.  See the
documentation for `align-region-separate' for more details."
  (cond ((or (not separator)
             (eq separator 'entire))
         nil)
        ((eq separator 'group)
         (let ((amount 2))
           (save-excursion
             (goto-char end)
             (if (bolp)
                 (setq amount 1)))
           (> (count-lines beg end) amount)))
        ((stringp separator)
         (save-excursion
           (goto-char beg)
           (re-search-forward separator end t)))
        ((functionp separator)
         (funcall separator beg end))
        ((listp separator)
         (let ((seps separator) yes)
           (while seps
             (if (and (>= (car seps) beg)
                      (<= (car seps) end))
                 (setq yes t seps nil)
             (setq seps (cdr seps))))
           yes))))

(defun align-adjust-col-for-rule (column rule)
  "Adjust COLUMN according to the given RULE."
  (let* ((rule-ts (assq 'tab-stop rule))
         (tab-stop (if rule-ts
                       (cdr rule-ts)
                     (if (symbolp align-to-tab-stop)
                         (symbol-value align-to-tab-stop)
                       align-to-tab-stop)))
         (spacing  (or (cdr (assq 'spacing rule))
                       align-default-spacing)))
    (if (<= spacing 0)
        column
      (if (not tab-stop)
          (+ column spacing)
        (let ((stops tab-stop-list))
          (while stops
            (if (and (> (car stops) column)
                     (= (setq spacing (1- spacing)) 0))
                (setq column (car stops)
                      stops nil)
              (setq stops (cdr stops)))))
        column))))

(defsubst align-column (pos)
  "Given a position in the buffer, state what column it's in.
POS is the position whose column will be taken.  Note that this
function will change the location of point."
  (goto-char pos)
  (current-column))

(defun align-regions (regions rule exclude-rules)
  "Align the regions specified in REGIONS, a list of cons cells.
RULE governs this alignment, while EXCLUDE-RULES should be a list of
exclusion rules that may prevent some of the REGIONS from being
modified.  The reason for computing the exclusion areas here is solely
efficiency."
  (let (exclude-areas excl-beg excl-end)
    (when exclude-rules
      (save-excursion
        (goto-char (caar (last (car regions))))
        (forward-line -1)
        (setq excl-beg (point))
        (goto-char (cdar (car (last regions))))
        (forward-line)
        (setq excl-end (point)))
      (align-region excl-beg excl-end 'entire exclude-rules nil
                    (function
                     (lambda (b e rule)
                       (or rule
                           (setq exclude-areas
                                 (cons (cons b e) exclude-areas))))))
      (setq exclude-areas
            (sort exclude-areas
                  (function
                   (lambda (l r)
                     (>= (car l) (car r)))))))
    (while regions
      (save-excursion
        (align-areas (car regions) rule exclude-areas))
      (setq regions (cdr regions)))))

(defun align-areas (areas rule exclude-areas)
  "Given a list of AREAS, align them according to RULE.
AREAS should be a list of cons cells containing beginning and ending
markers.  This function sweeps through all of the beginning markers,
finds out which one starts in the furthermost column, and then deletes
and inserts text such that all of the ending markers occur in the same
column.  If EXCLUDE-AREAS is non-nil, it represent a list of areas
where no changes should be made."
  (let* ((column (cdr (assq 'column rule)))
         (fixed (if (symbolp column)
                    (symbol-value column)
                  column))
         (justify (cdr (assq 'justify rule)))
         (col (or fixed 0))
         (width 0)
         ecol change look)

    ;; Determine the alignment column.
    (let ((a areas))
      (while a
        (setq look t)
        (while (and look exclude-areas
                    (< (caar a) (cdar exclude-areas)))
          (if (<= (cdar a) (caar exclude-areas))
              (setq exclude-areas (cdr exclude-areas))
            (setcar a nil)
            (setq look nil)))
        (when (car a)
          (if (not fixed)
              (setq col (max col (align-column (caar a)))))
          (unless change
            (goto-char (cdar a))
            (if ecol
                (if (not (= ecol (current-column)))
                    (setq change t))
              (setq ecol (current-column))))
          (when justify
            (goto-char (caar a))
            (if (and (re-search-forward "\\s-*" (cdar a) t)
                     (not (= (point) (cdar a))))
                (let ((bcol (current-column)))
                  (setcdr (car a) (cons (point-marker) (cdar a)))
                  (goto-char (cdr (cdar a)))
                  (setq width (max width (- (current-column) bcol)))))))
        (setq a (cdr a))))

    (or fixed
        (setq col (+ (align-adjust-col-for-rule col rule) width)))

    ;; Make all ending positions to occur in the goal column.  Since
    ;; the whitespace to be modified was already deleted by
    ;; `align-region', all we have to do here is indent.

    (if (or change (and ecol (not (= col ecol))))
        (while areas
          (let ((area (car areas))
                (gocol col) cur)
            (when area
              (if (not (and justify
                            (consp (cdr area))))
                  (goto-char (cdr area))
                (goto-char (cddr area))
                (let ((ecol (current-column)))
                  (goto-char (cadr area))
                  (setq gocol (- col (- ecol (current-column))))))
              (setq cur (current-column))
              (cond ((< gocol 0) t)     ; don't do anything
                    ((= cur gocol) t)   ; don't need to
                    ((< cur gocol)      ; just add space
                     (indent-to gocol))
                    (t
                     ;; This code works around an oddity in the
                     ;; FORCE argument of `move-to-column', which
                     ;; tends to screw up markers if there is any
                     ;; tabbing.
                     (let ((endcol (align-column
                                    (if (and justify
                                             (consp (cdr area)))
                                        (cadr area)
                                      (cdr area))))
                           (abuts (<= gocol
                                      (align-column (car area)))))
                       (if abuts
                           (goto-char (car area))
                         (move-to-column gocol t))
                       (let ((here (point)))
                         (move-to-column endcol t)
                         (delete-region here (point))
                         (if abuts
                             (indent-to (align-adjust-col-for-rule
                                         (current-column) rule)))))))))
      (setq areas (cdr areas))))))

(defun align-region (beg end separate rules exclude-rules
                         &optional func)
  "Align a region based on a given set of alignment rules.
BEG and END specify the region to be aligned.  Either may be nil, in
which case the range will stop at the nearest section division (see
`align-region-separate', and `align-region-heuristic' for more
information').

The region will be divided into separate alignment sections based on
the value of SEPARATE.

RULES and EXCLUDE-RULES are a pair of lists describing how to align
the region, and which text areas within it should be excluded from
alignment.  See the `align-mode-alist' for more information on the
required format of these two lists.

If FUNC is specified, no text will be modified.  What `align-region'
will do with the rules is to search for the alignment areas, as it
regularly would, taking account for exclusions, and then call FUNC,
first with the beginning and ending of the region to be aligned
according to that rule (this can be different for each rule, if BEG
and END were nil), and second with the beginning and ending of each
text region that the rule would have applied to.

The signature of FUNC should thus be:

 (defun my-align-function (beg end rule)
   \"If RULE is non-nil, return t if BEG to END should be searched.
If RULE is nil, BEG to END will be a region of text that matches
the rule's definition.\"
   (if rule
       t
     (message \"Would have aligned from %d to %d...\" beg end)))

This feature (of passing a FUNC) is used internally to locate the
position of exclusion areas, but could also be used for any other
purpose where you might want to know where the regions that the
aligner would have dealt with are."
  (let ((end-mark (and end (copy-marker end)))
        (real-beg beg)
        (report (and (not func) align-large-region beg end
                     (>= (- end beg) align-large-region)))
        (rule-index 1)
        (rule-count (length rules)))
    (if align-indent-before-aligning
        (indent-region real-beg end-mark nil))
    (while rules
      (let* ((rule (car rules))
             (run-if (assq 'run-if rule))
             (modes (assq 'modes rule)))
        ;; unless the `run-if' form tells us not to, look for the
        ;; rule..
        (unless (or (and modes (not (memq major-mode (eval (cdr modes)))))
                    (and run-if (not (eval (cdr run-if)))))
          (let* ((current-case-fold case-fold-search)
                 (case-fold (assq 'case-fold rule))
                 (regexp  (cdr (assq 'regexp rule)))
                 (regfunc (and (functionp regexp) regexp))
                 (rulesep (assq 'separate rule))
                 (thissep (if rulesep (cdr rulesep) separate))
                 same (eol 0)
                 group group-c
                 repeat repeat-c
                 valid valid-c
                 pos-list first
                 regions index
                 last-point b e
                 save-match-data)
            (save-excursion
              ;; if beg and end were not given, figure out what the
              ;; current alignment region should be.  Depending on the
              ;; value of `align-region-separate' it's possible for
              ;; each rule to have its own definition of what that
              ;; current alignment section is.
              (if beg
                  (goto-char beg)
                (if (or (not thissep) (eq thissep 'entire))
                    (error "Cannot determine alignment region for '%s'"
                           (symbol-name (cdr (assq 'title rule)))))
                (beginning-of-line)
                (while (and (not (eobp))
                            (looking-at "^\\s-*$"))
                  (forward-line))
                (let* ((here (point))
                       (start here))
                  (while (and here
                              (re-search-backward
                               regexp (and align-region-heuristic
                                           (- (point)
                                              align-region-heuristic))
                               t))
                    (if (align-new-section-p (point) here thissep)
                        (setq beg here
                              here nil)
                      (setq here (point))))
                  (if (not here)
                      (goto-char beg))
                  (beginning-of-line)
                  (setq beg (point))
                  (goto-char start)
                  (setq here (point))
                  (while (and here
                              (re-search-forward
                               regexp (and align-region-heuristic
                                           (+ (point)
                                              align-region-heuristic))
                               t))
                    (if (align-new-section-p here (point) thissep)
                        (setq end here
                              here nil)
                      (setq here (point))))
                  (if (not here)
                      (goto-char end))
                  (forward-line)
                  (setq end (point)
                        end-mark (copy-marker end))
                  (goto-char beg)))
              ;; If we have a region to align, and `func' is set and
              ;; reports back that the region is ok, then align it.
              (when (or (not func)
                        (funcall func beg end rule))
                (unwind-protect
                    (progn
                      ;; set `case-fold-search' according to the
                      ;; (optional) `case-fold' property
                      (and case-fold
                           (setq case-fold-search (cdr case-fold)))

                      ;; while we can find the rule in the alignment
                      ;; region..
                      (while (and (< (point) end-mark)
                                  (if regfunc
                                      (funcall regfunc end-mark)
                                    (re-search-forward regexp end-mark t)))

                        (if report
                            (message
                             "Aligning `%s' [rule %d of %d] (%d%%)..."
                             (symbol-name (car rule))
                             rule-index rule-count
                             (/ (* (- (point) real-beg) 100)
                                (- end-mark real-beg))))

                        ;; if the search ended us on the beginning of
                        ;; the next line, move back to the end of the
                        ;; previous line.
                        (if (bolp)
                            (forward-char -1))

                        ;; lookup the `group' attribute the first time
                        ;; that we need it
                        (unless group-c
                          (setq group (or (cdr (assq 'group rule)) 1))
                          (if (not (listp group))
                              (setq first group
                                    group (list group))
                            (setq first (car group)))
                          (setq group-c t))

                        ;; test whether we have found a match on the same
                        ;; line as a previous match
                        (if (> (point) eol)
                            (setq same nil
                                  eol (save-excursion
                                        (end-of-line)
                                        (point-marker))))

                        ;; lookup the `repeat' attribute the first time
                        (or repeat-c
                            (setq repeat (cdr (assq 'repeat rule))
                                  repeat-c t))

                        ;; lookup the `valid' attribute the first time
                        (or valid-c
                            (setq valid (assq 'valid rule)
                                  valid-c t))

                        ;; remember the beginning position of this rule
                        ;; match, and save the match-data, since either
                        ;; the `valid' form, or the code that searches for
                        ;; section separation, might alter it
                        (setq b (match-beginning first)
                              save-match-data (match-data))

                        ;; unless the `valid' attribute is set, and tells
                        ;; us that the rule is not valid at this point in
                        ;; the code..
                        (unless (and valid (not (eval (cdr valid))))

                          ;; look to see if this match begins a new
                          ;; section.  If so, we should align what we've
                          ;; collected so far, and then begin collecting
                          ;; anew for the next alignment section
                          (if (and
                               (not func) last-point
                               (align-new-section-p last-point b thissep))
                              (setq last-point (copy-marker b)
                                    regions (align-regions regions rule
                                                           exclude-rules))
                            (setq last-point (copy-marker b)))

                          ;; restore the match data
                          (set-match-data save-match-data)

                          ;; go through the list of parenthesis groups
                          ;; that matched whitespace text to be
                          ;; contracted/expanded (or possibly justified,
                          ;; if the `justify' attribute was set)
                          (let ((g group))
                            (while g

                              ;; we have to use markers, since
                              ;; `align-areas' may modify the buffer
                              (setq b (copy-marker (match-beginning (car g)))
                                    e (copy-marker (match-end (car g))))

                              ;; if searching for exclusion, then call
                              ;; `func'; otherwise, record this text
                              ;; region for alignment
                              (if func
                                  (funcall func b e nil)
                                (setq index (if same (1+ index) 0))
                                (if (not (nth index regions))
                                    (if regions
                                        (nconc regions
                                               (list (list (cons b e))))
                                      (setq regions
                                            (list (list (cons b e)))))
                                  (setcar (nthcdr index regions)
                                          (cons (cons b e)
                                                (nth index regions)))))

                              ;; if any further rule matches are found
                              ;; before `eol', then they are on the same
                              ;; line as this one; this can only happen if
                              ;; the `repeat' attribute is non-nil
                              (setq same t g (cdr g))))

                          ;; if `repeat' has not been set, move to the
                          ;; next line; don't bother searching anymore on
                          ;; this one
                          (if (and (not repeat) (not (bolp)))
                              (forward-line))))

                      ;; when they are no more matches for this rule,
                      ;; align whatever was left over
                      (if (and (not func) regions)
                          (align-regions regions rule exclude-rules)))

                  (setq case-fold-search current-case-fold)))))))
      (setq rules (cdr rules)
            rule-index (1+ rule-index)
            beg real-beg))
    (if report
        (message "Aligning...done"))))

;; Provide:

(provide 'align)

(run-hooks 'align-load-hook)

;;; align.el ends here
