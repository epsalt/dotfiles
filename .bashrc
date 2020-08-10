#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='\t >>> '

if [ "$(uname)" == "Darwin" ]; then
    PATH="/usr/local/bin:$PATH"
    PATH="/usr/local/sbin:$PATH"
    export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3
    export VIRTUALENVWRAPPER_VIRTUALENV=/usr/local/bin/virtualenv
    export WORKON_HOME=$HOME/.virtualenvs
    source /usr/local/bin/virtualenvwrapper.sh

    export CLICOLOR=1
    export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
    export TERM=xterm-256color

    export BASH_COMPLETION_COMPAT_DIR="/usr/local/etc/bash_completion.d"
    [[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"

elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    PATH="$HOME/.local/bin:$PATH"
    export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python
    export VIRTUALENVWRAPPER_VIRTUALENV="$HOME/.local/bin/virtualenv"
    export WORKON_HOME=$HOME/.virtualenvs
    export PATH="$HOME/.cargo/bin:$PATH"

    source "$HOME/.local/bin/virtualenvwrapper.sh"

    alias ls='ls --color=auto'
    alias grep='grep --color=auto'

    test -r "~/.dir_colors" && eval $(dircolors ~/.dir_colors)
fi

BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
        eval "$("$BASE16_SHELL/profile_helper.sh")"


alias ec="emacsclient -t -a ''"

export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"                  # $EDITOR should open in terminal
export VISUAL="emacsclient -c -a emacs"         # $VISUAL opens in GUI with non-daemon as alternate
