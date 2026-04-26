# ~/.bashrc
### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
export PATH="/Users/chlane/.rd/bin:$PATH"
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)
 export PATH=$PATH:~/.local/share/coursier/bin

# sourced in at shell startup time.
# this file is part of the ppdev environment.
# DO NOT EDIT - instead, put customizations in ~/.bashrc.custom .

export PY3=1

export CLFAGS="-g -ggdb"

if [ "$(hostname -a)" == "laptop" ] ; then
    export PATH=$PATH:~/.cargo/bin
elif [ "$(uname)" == "Darwin" ] ; then   
    export PATH=$PATH:~/bin:/Users/chlane/Library//Python/3.7/bin
    export ORACLE_HOME=/Users/chlane/instantclient_11_2/
    export REQUESTS_CA_BUNDLE='/usr/local/etc/openssl/certs/combined_cacerts.pem'
    export SSL_CERT_FILE='/usr/local/etc/openssl/certs/combined_cacerts.pem'
fi

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
export PATH=~/.local/bin:$PATH:~/homebrew/bin
export PATH=$PATH:/usr/local/texlive/2021basic/bin/universal-darwin/
export PATH=/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_14:$PATH
export PATH=/anaconda3/bin:$PATH
export PATH=/usr/local/bin:$PATH
export PATH=/usr/local/go/bin/:$PATH
export PATH=~/google-cloud-sdk/bin:$PATH
export GOPATH=~/go

export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:~/.local/bin
export WINEARCH=win32
export WINEPREFIX="/home/lanstin/.wine"
export WINE=wine-development
#export TZ=Asia/Colcutta

# WTF dashboard GitHub token (read from .netrc)
export WTF_GITHUB_TOKEN=$(awk '/machine github.com/{for(i=1;i<=NF;i++)if($i=="password")print $(i+1)}' ~/.netrc 2>/dev/null)

if [[ $(hostname -s) = "hyperlvs40" ]] ; then
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

export TZ=Asia/Calcutta
export TZ=US/Pacific
if [[ $(uname) == "Darwin" ]] ; then 
    export SSH_AUTH_SOCK="$HOME/.ssh/agent.sock"
    # Start agent if no working agent exists at the fixed path
    ssh-add -l &>/dev/null
    if [ $? -eq 2 ]; then
        eval $(ssh-agent -a "$HOME/.ssh/agent.sock") >/dev/null
    fi
    export CC=/usr/bin/clang
else
	echo Linux rocks
fi
shopt -s histappend
export HISTSIZE=200000
export PROMPT_COMMAND=". ~/bin/prompt_cmd"
export PROMPT_COMMAND="history -a; history -c; history -r;$PROMPT_COMMAND"
export EDITOR=emacsclient
export HN=$(hostname)
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
 TERM=xterm-color
fi
export PAGER=/usr/bin/less
ignoreeof=10
# set up the color-ls environment variables:
if [ "$EMACS" = "t" ] || [ "$INSIDE_EMACS" != "" ] ; then
  alias ls='ls -larthdF --color --'
  export PAGER=/bin/cat
  export EMACS=t
  echo "Emacs rules."
  PS1='\W($SHLVL:\!)\$ '
else 
  eval `/usr/bin/dircolors -b`
  alias ls='ls -F -lhartd --color -- '
  PS1='\W($SHLVL:\!)\$ '
fi
PS2='> '
alias vi='vim -X'
alias vim='vim -X'

alias su='su -'

alias lock='loginctl lock-session'

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
                    tmux attach-session -t "$Atmuxsession"
                fi
            fi 
        fi
     fi
esac

fi
. "$HOME/.cargo/env"
