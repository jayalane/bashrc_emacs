#OktaAWSCLI
if [[ -f "$HOME/.okta/bash_functions" ]]; then
    . "$HOME/.okta/bash_functions"
fi
if [[ -d "$HOME/.okta/bin" && ":$PATH:" != *":$HOME/.okta/bin:"* ]]; then
    PATH="$HOME/.okta/bin:$PATH"
fi
export PATH=/anaconda3/bin:$PATH
export PATH=/usr/local/bin:$PATH
export PATH=/usr/local/go/bin/:$PATH
export PATH=~/google-cloud-sdk/bin:$PATH
export PATH=$PATH:~/bin:/Users/c60932a/Library//Python/3.7/bin
export GOPATH=~/go
export PATH=$PATH:$GOPATH/bin
export TZ=US/Pacific

export CC=/usr/bin/clang
shopt -s histappend
export HISTSIZE=200000
export PROMPT_COMMAND="history -a; history -c; history -r;$PROMPT_COMMAND"
export EDITOR=/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_14/emacsclient
HN=`hostname` 
unset PYTHONHOME
unset PYTHONPATH
unset PYTHONBIN
umask 077

#source ~/.profile

if [ "$EMACS" = "t" ] || [ "$INSIDE_EMACS" != "" ] ; then
  export EMACS=t
  alias ls='ls -larthdF'
  export PAGER=/bin/cat
  echo "Emacs rules."
  PS1='\W($SHLVL:\!)\$ '
else 
  eval `/usr/bin/dircolors -b`
  alias ls='ls -F -lhartd'
  PS1='\W($SHLVL:\!)\$ '
fi

export PROMPT_COMMAND=~/bin/prompt_cmd
alias vi='vim -X'
alias vim='vim -X'
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
				A=$(tmux list-sessions | gawk ' { print $1 } ' | gawk -F \: ' { print $1 } ' | head -n1)
                tmux attach-session -t $A
            fi 
        else
          export PAGER=/bin/cat
        fi
     fi
esac

