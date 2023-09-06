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
shopt -s globstar # enable recursive match in globs with '**'

################################################################################
# Configure Additional Package Managers
################################################################################
# Configure Linuxbrew if present
[ -f /home/linuxbrew/.linuxbrew/bin/brew ] && eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"

################################################################################
# Configure Dev Tools
################################################################################
# Configure NVM if present
export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Configure Python and virtualenv
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/devel/projects
[ -s "./.local/bin/virtualenvwrapper.sh" ] && \. "./.local/bin/virtualenvwrapper.sh"

# Configure rust if present
[ -s "$HOME/.cargo/env" ] && \. "$HOME/.cargo/env"

################################################################################
# Path updates
################################################################################
# Set PATH so it includes .local binaries iff it exists
if [ -d "$HOME/.local/bin" ] ; then
	PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "/usr/local/go" ] ; then
	PATH="$PATH:/usr/local/go/bin"
	# TODO: Guarantee devel path structure exists
	export GOPATH=$HOME/devel/langs/go
fi

################################################################################
# Helper Functions
################################################################################
# Better ssh agent management via:
# https://vlaams-supercomputing-centrum-vscdocumentation.readthedocs-hosted.com/en/latest/access/using_ssh_agent.html
# For some background: https://rabexc.org/posts/pitfalls-of-ssh-agents
start-ssh-agent() {
    sshfile=~/.ssh-agent-environment

    if [ -n \"$SSH_AUTH_SOCK\" ]; then
        ssh-add -l &>/dev/null
        [[ $? != 2 ]] && unset sshfile && return 0
    fi

    if [ -e \"$sshfile\" ]; then
        . $sshfile &>/dev/null
        ssh-add -l &>/dev/null
        [[ $? != 2 ]] && unset sshfile && return 0
    fi

    ssh-agent -s > $sshfile && . $sshfile &>/dev/null
    unset sshfile
}

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

################################################################################
# Helper Setup Invocations
################################################################################
# Use smart ssh-agent setup
start-ssh-agent &>/dev/null

################################################################################
# Source Locals
################################################################################
# Check if I have an alias file present; Install if so
[ -s ~/.bash_aliases ] && . ~/.bash_aliases

# Load machine specific bash configurations if any
[ -s ~/.bash_machine ] && . ~/.bash_machine
