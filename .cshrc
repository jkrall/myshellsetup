########################################
#
#  .cshrc
#
########################################


if ($?prompt) then
    set quiet
    echo "[source ~$USER/.cshrc]"
endif

#
# Local shell variables
#
set history=100
set savehist=100
set ignoreeof
set tostop
set notify
set rmstar=on
# setting the following line to enhance
#  1) ignores case and 2) considers periods,  hyphens
#       and  underscores  (`.', `-' and `_') to be word separators
#       and hyphens and underscores to be equivalent 
set complete=enhance



#
# Environment variables
#
setenv PAGER "more"
setenv QAGER "more -c"
# Enhance vi editor
setenv EDITOR "/usr/apps/bin/vim"

#
# Source aliases
#
if -e ~/.alias then
    source ~/.alias
endif

#
# Source local cshrc file, if present.
#
if -e ~/.cshrc.$HOST then
    source ~/.cshrc.$HOST
endif

#
# Note: if you require $cdpath, please 
# set $cdpathbase also, as an identical variable.
# For an example, see $path and $pathbase above.
#

# set path = ( /software/thalia/progutils/e-linuxSuSE9.3-i386-std $path )
# set path = ( /software/thalia/progutils $path )

#
# Set the prompt.
#
switch ($shell)
    case *tcsh*:
	# Set your tcsh prompt here.  "man tcsh" and search for PROMPT FORMAT
	# to see other sequences available.
        set prompt = "%m:%B%~%b> "
        if ("$SHLVL" != 1) then # subshell
            set prompt = "${prompt}>"
        endif
        breaksw
    default:
        # csh
        set prompt = "${HOST}>"
        breaksw
endsw

#---------------------------------------
# Job environment setup
#---------------------------------------
# setjob
# put in as requested by kto (9/6/00) hawson
# moved from .login (greggo 25feb02)
# runlastjob

if ( $?job_cmd_scn ) then
    if ($job_cmd_scn == "none") then
        chdir $PWD/share
    endif
endif

#
# some personal settings that I'm adding 
#

source ~/myshellsetup
