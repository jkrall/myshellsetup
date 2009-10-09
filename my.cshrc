set autolist=ambiquous
set autocorrect=1
set correct=cmd
set ignoreeof

setenv PAGER less
setenv EDITOR /usr/bin/mate

setenv PATH /usr/local/bin:${PATH}
setenv PATH ${HOME}/bin:${PATH}
setenv PATH ${PATH}:/opt/local/bin

alias tfs cd /Programming/Transfs/tfs
alias sshtfs 'ssh josh@transfs.com'

alias psx 'ps ax | grep '

set prompt = "[%@] %C3 >"
# set prompt = "[%@]%n %C3 >"  This one includes the current user

setenv TERM "xterm-color"
setenv CLICOLOR "true"
# setenv LSCOLORS "exfxcxdxbxegedabagacad"
setenv LSCOLORS "dxfxcxdxbxegedabagacad"

