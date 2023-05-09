# commands common to all logins
#ulimit -n 1024
# ulimit -l 0
#ulimit -s 20480

export PY3=1

export ORACLE_HOME=/Users/chlane/instantclient_11_2/
export REQUESTS_CA_BUNDLE='/usr/local/etc/openssl/certs/combined_cacerts.pem'
export SSL_CERT_FILE='/usr/local/etc/openssl/certs/combined_cacerts.pem'

if [ "$INSIDE_EMACS" != "" ] ; then
    export EMACS=t
fi

#OktaAWSCLI
if [[ -f "$HOME/.okta/bash_functions" ]]; then
    . "$HOME/.okta/bash_functions"
fi
if [[ -d "$HOME/.okta/bin" && ":$PATH:" != *":$HOME/.okta/bin:"* ]]; then
    PATH="$HOME/.okta/bin:$PATH"
fi
export PATH=$PATH:~/homebrew/bin
export PATH=$PATH:/usr/local/texlive/2021basic/bin/universal-darwin/
export PATH=/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_14:$PATH
export PATH=/anaconda3/bin:$PATH
export PATH=/usr/local/bin:$PATH
export PATH=/usr/local/go/bin/:$PATH
export PATH=~/google-cloud-sdk/bin:$PATH
export PATH=$PATH:~/bin:/Users/chlane/Library//Python/3.7/bin
export GOPATH=~/go
export PATH=$PATH:$GOPATH/bin
export WINEARCH=win32
export WINEPREFIX="/home/lanstin/.wine"
export WINE=wine-development
export TZ=US/Pacific

if [[ `hostname -s` = "hyperlvs40" ]] ; then
    if [[ "$-" = "*i*" ]] ; then 
       echo "Skipping .bashrc"
    fi
else

export EDITOR=emacsclient
export SCRATCH=~/.scratch
export GEVENT_RESOLVER=ares
export CLASSPATH=$CLASSPATH:/opt/tomcat5/common/lib
export MANPATH=/usr/man:/usr/local/man:/usr/share/man:/usr/X11/man:/sw/share/man
export PATH="/x/opt/pp/bin:$PATH:~/bin:/usr/local/bin:."
LESS="-MM -R"

# export TZ=Asia/Calcutta
export TZ=US/Pacific
if [[ $(uname) == "Darwin" ]] ; then 
    export CC=/usr/bin/clang
fi
shopt -s histappend
export HISTSIZE=200000
#export PROMPT_COMMAND="history -a; history -c; history -r;$PROMPT_COMMAND"
export EDITOR=
export HN=`hostname` 
unset PYTHONHOME
unset PYTHONPATH
unset PYTHONBIN
umask 077
unset MYDEGREE
# export DO_DEBUG=1
if [ "$DO_DEBUG" = "" ] ; then
  unset DEBUG
  unset DEBUG_MODE
  unset SYMBOLS
  unset VERBOSE
else
  export DEBUG=1
  export DEBUG_MODE=1
  export SYMBOLS=1
  export VERBOSE=2
  export M_DEBUG=1
fi
# I had problems using 'eval tset' instead of 'TERM=', but you might want to 
# try it anyway. I think with the right /etc/termcap it would work great.
# eval `tset -sQ "$TERM"`
if [ "$TERM" = "" -o "$TERM" = "unknown" ]; then
 TERM=xterm
fi
export PAGER=/usr/bin/less
ignoreeof=10
# set up the color-ls environment variables:
if [ "$EMACS" = "t" ] || [ "$INSIDE_EMACS" != "" ] ; then
#  alias ls='ls -larthdF --color'
  export PAGER=/bin/cat
  export EMACS=t
  echo "Emacs rules."
  PS1='\W($SHLVL:\!)\$ '
else 
  eval `/usr/bin/dircolors -b`
#  alias ls='ls -F -lhartd'
  PS1='\W($SHLVL:\!)\$ '
fi
PS2='> '
export PROMPT_COMMAND=~/bin/prompt_cmd
alias vi='vim -X'
alias vim='vim -X'

alias su='su -'

alias lock='xscreensaver-command -lock'

if [ -f /sw/bin/init.sh ]; then 
    . /sw/bin/init.sh
fi

if [ $TERM = "linux" ]
then
     unicode_start
fi
export LC_ALL="en_US.UTF-8"
export LANG="en_US.UTF-8"
case "$-" in 
   *i*) 
      stty erase 
      stty start undef
      stty stop undef
      if [ "$TMUX" != "" ]; then
        echo tmux rocks
      else
        if [ "$EMACS" = "" ]; then
            if [ "$TERM" != "dumb" ]; then
	        Atmuxsession=$(tmux list-sessions | gawk ' { print $1 } ' | gawk -F \: ' { print $1 } ' | head -n1)
                if [ "$Atmuxsession" == "" ] ; then
                    tmux
                else
                    tmux attach-session -t $Atmuxsession
                fi
            fi 
        fi
     fi
esac

fi
