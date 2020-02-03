# some ls aliases
alias la='ls -la'  # Lists all files (including dot-files)
alias lc='ls -lU'  # List by time created
alias ll='ls -alF' # List all files with classification
alias lm='ls -lt'  # List by time modified
alias lo='ls -lut' # List by time accessed
alias ls='ls -pA'  # Adds trailing '/' to dirs; suppresses '.' and '..'
alias lz='ls -lhS' # List human readable size (sorted)
#alias l='ls -CF'  # List entries by columns with classification

# Git Aliases
alias  ga='git add'
alias  gb='git branch'
alias  gd='git diff'
alias  gl='git log'
alias  gp='git pull'
alias  gs='git status'
alias  gu='git push'
alias gcl='git clone'
alias gcm='git commit'
alias gco='git checkout'

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

# Browsers
[ -s /opt/google/chrome/chrome ] && alias chrome=/opt/google/chrome/chrome

# Other
alias ports="netstat -tulanp"

################################################################################
# Language Tools
################################################################################

# Python/Pip Aliases
# TODO conditionally set if Python/Pip installed
alias pipup='pip3 freeze -- local | grep -v "^\-e" | cut -d = -f 1 | xargs -n1 pip3 install --user -U'
export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/projects
[ -s "./.local/bin/virtualenvwrapper.sh" ] && \. "./.local/bin/virtualenvwrapper.sh"


# C++ 14 : g++ simple build
# TODO: conditionally set if g++ is installed
alias cpp14='g++ -std="c++14" -ggdb'

# NVM for easy versioning of Node
# TODO: Conditionally set if NVM installed
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

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
alias freq="aqk 'print $1' ~/.bash_history | sort | uniq -c | sort -rn | head -n 20"
