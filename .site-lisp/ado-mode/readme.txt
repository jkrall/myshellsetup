Ado-mode for emacs v. 20 and higher
-----------------------------------

This is the readme file for the ado-mode major mode made for editing Stata
files nicely within emacs. It is designed for Stata 9.x, though it is
useful for any version of Stata. It works fine in emacs version 21.

Ado-mode provides the following useful features:
. Context sensitive highlighting (aka font-locking) of Stata commands and
common constructions within Stata.
. Smart indentation based on nesting of blocks. This makes the code more
readable and catches forgotten closing braces, quotes, and the like.
. Templates for documenting .ado files with Stata help files in pretty much
the same format as used by Stata Corp. for its documentation.
. Puts timestamps on files as they are saved to keep life easy for
comparing versions.

The mode is customizable both globally via emacs' customize command, as
well as buffer-locally via menus.

The installation instructions are at

http://faculty.bellarmine.edu/wrising/Stata/ado-mode_install.html

Some Extra Files:
----------------

The syntax_tester.ado and syntax_tester.dlg files are good for checking
results, intended or otherwise, of fiddling with the ado-mode.el file. They
contain pretty much all the commands which appear in the Stata
manuals. There could be commands in updates which are missing...

If You Keep Getting Errors:
--------------------------

If you install the software and keep getting errors, specifically 
"Symbol's function definition is void: line-number-at-pos"
add the following to your .emacs file:
(require 'ado-hacks)
It is a small file which includes the definition of line-number-at-pos from
emacs 22.0.50.1.

Disclaimers:
-----------

You can use this to write ado, do, and help files for versions of Stata
earlier than version 9, but it will highlight as though the files were for
Stata version 9.

I would seriously doubt that the ado-mode works with xemacs.

Use as you please, but abide by the GNU Public License (GPL).

To see what has changed and what should change in the future, see the
changes.txt file.

Enjoy!

Author:
------

Send rants or raves as well as bug reports and feature requests to 

Bill Rising <wrising@bellarmine.edu>

Plaintive Begging Statement:
---------------------------

If you use this ado-mode and find it useful to get publishable work done,
and you think of it, mentioning the software would be nice, as would
letting me you use the ado-mode. I'm not vain --- I need the publicity for
things like, uh, tenure. Thanks. 
