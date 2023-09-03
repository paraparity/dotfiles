# some ls aliases
alias la='ls -la'  # Lists all files (including dot-files)
alias lc='ls -lU'  # List by time created
alias ll='ls -alF' # List all files with classification
alias lm='ls -lt'  # List by time modified
alias lo='ls -lut' # List by time accessed
alias ls='ls -pA'  # Adds trailing '/' to dirs; suppresses '.' and '..'
alias lz='ls -lhS' # List human readable size (sorted)
#alias l='ls -CF'  # List entries by columns with classification

# Lazy directory escape rope
alias .1='cd ..'
alias .2='cd ../..'
alias .3='cd ../../..'
alias .4='cd ../../../..'
alias .5='cd ../../../../..'

# Directory shortcuts
alias dl='cd ~/Downloads'

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

# NPM Shortcuts
alias o='npm outdated'
alias r='npm run'
alias b='npm run build'
alias i='npm i'
alias t='npm run test'
alias s='npm run start'

# Docker Shortcuts
alias  dps="docker ps "
alias  dim="docker images"
alias  drm="docker rm"
alias dpsa="docker ps -a"

# Docker Compose Shortcuts
alias   dcup='docker-compose up -d'
alias   dcps='docker-compose ps'
alias dcstop='docker-compose stop'

# TODO: add OS test to conditionally set:
# alias bu='brew update && brew upgrade'

# Browsers
[ -s /opt/google/chrome/chrome ] && alias chrome=/opt/google/chrome/chrome

# Other
alias ports="netstat -tulanp"

################################################################################
# Language Tools
################################################################################
# Python/Pip Aliases
alias pipup='pip3 freeze -- local | grep -v "^\-e" | cut -d = -f 1 | xargs -n1 pip3 install --user -U'

# C++ 14 : g++ simple build
# TODO: conditionally set if g++ is installed
alias cpp14='g++ -std="c++14" -ggdb'

# Identify busy commands
alias freq="aqk 'print $1' ~/.bash_history | sort | uniq -c | sort -rn | head -n 20"

# Misc
alias pun="curl -H 'accept: text/plain' https://icanhazdadjoke.com/ -w '\n'"
