set autolist=ambiquous
set autocorrect=1
set correct=cmd
set ignoreeof

setenv PAGER less
setenv EDITOR /Applications/Emacs.app/Contents/MacOS/Emacs

setenv PATH /usr/local/bin:${PATH}
setenv PATH /usr/local/php5/bin:${PATH}
setenv PATH ${HOME}/bin:${PATH}
setenv PATH ${PATH}:/opt/local/bin
setenv PATH ${PATH}:/opt/local/apache2/bin

setenv MONO_PATH ${MONO_PATH}:/opt/local/lib

alias tfs cd /Programming/Transfs/tfs
alias sshtfs 'ssh josh@transfs.com'

alias ritd cd /Programming/RITDFundraising.com/ritd
alias tcn cd /Programming/tcn

alias psx 'ps ax | grep '

# RH Stuff
setenv PROD /RH/usr/local/prod
setenv PATH /RH/muse/bin:/RH/usr/local/prod/bin:${PATH}
alias RH_ddr_get 'ddr_get \!:* -dest /RH/DDR '
alias RH_ddr_info 'ddr_info -description -job \!:1 -user \!:2 \!:3-*'

