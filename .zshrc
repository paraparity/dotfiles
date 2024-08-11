# .zshrc is evaluated for login/interactive shells, but not for scripts

################################################################################
# ZSH Option Configuration
################################################################################
# Funfact: https://unix.stackexchange.com/questions/32409/set-and-shopt-why-two
setopt ignore_eof
#setopt print_exit_value # I've opted to define $? at the start of my prompt

# Globs
# BASH: shopt -s extglob # Reg-ex globbing (e.g: 'ls ?(a*|b*)' # list files starting with a or b; Uses '?!*+@'
setopt extended_glob
setopt case_glob

# History Settings
setopt append_history      # Multiple zsh sessions contribute to shared history
setopt share_history       # import and append to history file
setopt no_extended_history # I don't want timestamps, they make 'freq' difficult

# Scripts and Functions
setopt c_bases      # C standard hexadecimal output eg: 0xFF
setopt octal_zeroes # C standard octal output eg: 077
setopt pipe_fail    # Report $? of rightmost element of pipeline that was nonzero

################################################################################
# Configure Command Prompt
################################################################################
# See ANSI Escape Sequences for details: https://www.tldp.org/HOWTO/Bash-Prompt-HOWTO/c327.html
# See also, the ASCII Table: http://www.asciitable.com/
# And this summary: https://www.cyberciti.biz/tips/howto-linux-unix-bash-shell-setup-prompt.html
export PS1='%(?.%F{green}#{0}.%F{red}#{%?})$f %F{5}%n%f@%F{green}%m%f %F{14}%~%f
%(!.%F{red}||>%f .%F{green}|>%f )'

################################################################################
# General Settings
################################################################################
HISTFILESIZE=100000
HISTSIZE=10000

export EDITOR='emacsclient -t'
export VISUAL='emacsclient -c -a emacs'

export WORDCHARS='*?_-[]~.=&;!#$%^(){}<>'

export LC_ALL='en_US.UTF-8'

################################################################################
# Custom Aliases and Shortcuts
################################################################################
# some ls aliases
alias la='ls -la'  # Lists all files (including dot-files)
alias lc='ls -lU'  # List by time created
alias ll='ls -alF' # List all files with classification
alias lm='ls -lt'  # List by time modified
alias lo='ls -lut' # List by time accessed
alias ls='ls -pA'  # Adds trailing '/' to dirs; suppresses '.' and '..'
alias lz='ls -lhS' # List human readable size (sorted)

# Git Aliases
alias   ga='git add'
alias   gb='git branch'
alias   gd='git diff'
alias   gl='git log'
alias   gu='git pull' # update
alias   gp='git push'
alias  gpf='git push --force'
alias   gs='git status'
alias  gcl='git clone'
alias  gcm='git commit'
alias  gca='git commit --amend --no-edit'
alias  gco='git checkout'
alias gcod='git checkout develop'

# Lazy directory escape rope
alias .1='cd ..'
alias .2='cd ../..'
alias .3='cd ../../..'
alias .4='cd ../../../..'
alias .5='cd ../../../../..'

# NPM Shortcuts
alias i='npm i'
alias o='npm outdated'
alias r='npm run'
alias b='npm run build'
alias t='npm run test'
alias s='npm run start'

# TODO: add OS test to conditionally set:
# alias bu='brew update && brew upgrade'

# Other
alias ports="netstat -tulanp"

# Set PATH so it includes .local binaries iff it exists
if [ -d "$HOME/.local/bin" ] ; then
	PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "/usr/local/go" ] ; then
	PATH="$PATH:/usr/local/go/bin"
	# TODO: Guarantee devel path structure exists
	export GOPATH=$HOME/devel/langs/go
fi

# Identify busy commands
alias freq="awk '{print $1}' ~/.zsh_history | sort | uniq -c | sort -rn | head -n 20"

# Fun
alias pun="curl -H 'accept: text/plain' https://icanhazdadjoke.com/ -w '\n'"

################################################################################
# Custom Functions
################################################################################
# # ex - archive extractor
# # usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1     ;;
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
# Source Locals
################################################################################
# Load machine specific zsh configurations if any
[ -s ~/.zshrc_machine ] && . ~/.zshrc_machine
