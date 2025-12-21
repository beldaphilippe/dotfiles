alias ll="ls -lA --color=auto"
alias l="ll"
alias bat="batcat"

# emacs
alias ec="(emacsclient -c > /dev/null) || ((emacs --daemon > /dev/null) && emacsclient -c)"
alias ek="emacsclient -e \"(kill-emacs)\""

# git
alias ga="git add"
alias gb="git branch"
alias gcmsg="git commit -m"
alias glog="git log --all --pretty --graph --oneline --decorate=auto"
alias gst="git status" 
alias gf="git fetch --prune"
