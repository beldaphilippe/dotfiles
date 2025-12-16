alias ll="ls -lA --color=auto"
alias l="ll"

# emacs
alias ec="emacsclient -c || emacs --daemon && emacsclient -c"
alias ek="emacsclient -e \"(kill-emacs)\""

# git
alias ga="git add"
alias gcmsg="git commit -m"
alias glog="git log --all --pretty --graph --oneline --decorate=auto"
alias gst="git status"
