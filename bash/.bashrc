# export PATH="$HOME/bin/:$PATH"

# PS0: after reading a command and before command executed
# PS1: primary prompt string
# PS2: secondary prompt string


PROMPT_COMMAND=__prompt_command    # Function to generate PS1 after CMDs

function parse_git_dirty {
  [[ $(git status --porcelain 2> /dev/null) ]] && echo "*"
}
function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/ (\1$(parse_git_dirty))/"
}

__prompt_command() {
    local EXITSTATUS="$?"                # This needs to be first
    PS1=""

    local RESET='\[\e[0m\]' # Reset Color

    local RED='\[\e[0;31m\]'
    local BRED='\[\e[1;31m\]'
    local GREEN='\[\e[0;32m\]'
    local BGREEN='\[\e[1;32m\]'
    local BYELLOW='\[\e[1;33m\]'
    local BBLUE='\[\e[1;34m\]'
    local PURPLE='\[\e[0;35m\]'

    if [ $EXITSTATUS != 0 ]; then
        PS1+="${BRED}X ${RESET}"        # Add red if exit code non 0
    else
        PS1+="${BYELLOW}▶ ${RESET}"        # Add red if exit code non 0
    fi

    PS1+="${BGREEN}\u@\h ${RESET}${PURPLE}\w${BBLUE}$(parse_git_branch)${BYELLOW} $ ${RESET}"
}

bind 'TAB:menu-complete'
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

# aliases
source ~/.bash_aliases
