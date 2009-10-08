*! version 1.0.0 December 15, 2003 @ 09:52:46
*! for examples of highlighting (not for Stata)
program define font_highlighting_example
version 8.2
	/* this is a comment */
	// comments use ado-comment-face which inherits the font-lock-comment-face
	
	/* strings use ado-string-face which inherits the font-lock-string-face */
	"this is a string"

	/* data-changing commands use ado-builtin-harmful-face, which inherits the font-lock-keyword-face */
	clear 

	/* statistical function use ado-builtin-harmless-face, which inherits the font-lock-builtin-face */
	summarize

	/* subcommands use ado-subcommand-face, which inherits the font-lock-type-face */
	cluster centroid foo
	
	/* macros, scalars, temporary names use ado-variable-name-face which inherits */
	/*   the font-lock-variable-name-face */
	`foo'

	/* matrices use the ado-variable-name-face plus an underline */
	matrix define foo

	/* functions use ado-function-name-face which inherits the font-lock-function-name-face */
	sin(2*c(pi))

	/* constants are rare, and inherit the font-lock-constant-face */
	c(pi)

	/* obsolete commands use ado-obsolete-face which inherits font-lock-warning-face */
	display in green "this is obsolete"

end
