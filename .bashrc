# Source in system-wide rc settings
[ -f /etc/bashrc ] && . /etc/bashrc

# Let's not put duplicate lines/lines with starting spaces in history
HISTCONTROL=ignoreboth

# Append to the history file
shopt -s histappend
export PROMPT_COMMAND="history -a; history -r; $PROMPT_COMMAND"

# Set history length
HISTSIZE=1000
HISTFILESIZE=10000

# Check window size after each command; Update LINES/COLUMNS as needed
shopt -s checkwinsize

# Check if I have an alias file present; Install if so
[ -s ~/.bash_aliases ] && . ~/.bash_aliases

# Funfact: htps://unix.stackexchange.com/questions/32409/set-and-shopt-why-two
set -o ignoreeof
shopt -s extglob # Reg-ex globbing (e.g: 'ls ?(a*|b*)' # list files starting with a or b
