################################################################################
# General Settings
################################################################################
export EDITOR='emacsclient -t'
export VISUAL='emacsclient -c -a emacs'

# locale stuff
export LC_ALL='en_US.UTF-8'

# Check for other bash configurations and load
[ -f ~/.bashrc ] && . ~/.bashrc
[ -f ~/.bash_aliases ] && . ~/.bash_aliases
