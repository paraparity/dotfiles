# Source in system-wide rc settings
[ -f /etc/bashrc ] && . /etc/bashrc

# Check for other system configurations and load
[ -f /usr/share/nvm/init-nvm.sh ] && . /usr/share/nvm/init-nvm.sh

# Disable XON/XOFF to avoid collision with C-s forward bash searching
stty -ixon

# Configure Command Prompt
# See ANSI Escape Sequences for details: https://www.tldp.org/HOWTO/Bash-Prompt-HOWTO/c327.html
# See also, the ASCII Table: http://www.asciitable.com/
# And this summary: https://www.cyberciti.biz/tips/howto-linux-unix-bash-shell-setup-prompt.html
export PS1="\[\033[38;5;10m\]\u@\h\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput sgr0)\]\[\033[38;5;14m\]\w[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput sgr0)\]\[\033[38;5;14m\]|>\[$(tput sgr0)\]\[\033[38;5;15m\] \[$(tput sgr0)\]"

# Let's not put duplicate lines/lines with starting spaces in history
HISTCONTROL=ignoreboth

# Append to the history file
shopt -s histappend
export PROMPT_COMMAND="history -a; history -r; $PROMPT_COMMAND"

# Set history length
HISTSIZE=1000
HISTFILESIZE=10000

# Enable checkwinsize so that bash will check the terminal size when
# it regains control.  #65623
# http://cnswww.cns.cwru.edu/~chet/bash/FAQ (E11)
shopt -s checkwinsize

# Funfact: htps://unix.stackexchange.com/questions/32409/set-and-shopt-why-two
set -o ignoreeof
shopt -s extglob # Reg-ex globbing (e.g: 'ls ?(a*|b*)' # list files starting with a or b; Uses '?!*+@'

# Attempt to start/find ssh-agent if user has ssh directory
[ -d ~/.ssh ] && eval $(ssh-agent)

# Configure NVM if present
[ -z "$NVM_DIR" ] && export NVM_DIR="$HOME/.nvm"
[ -f /usr/share/nvm/nvm.sh ] && . /usr/share/nvm/nvm.sh
[ -f /usr/share/nvm/bash_completion ] && . /usr/share/nvm/bash_completion
[ -f /usr/share/nvm/install-nvm-exec ] && . /usr/share/nvm/install-nvm-exec

# # ex - archive extractor
# # usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1   ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

# Check if I have an alias file present; Install if so
[ -s ~/.bash_aliases ] && . ~/.bash_aliases
