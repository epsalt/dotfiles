#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ec="emacsclient -c -n -a emacs"

export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"                  # $EDITOR should open in terminal
export VISUAL="emacsclient -c -a emacs"         # $VISUAL opens in GUI with non-daemon as alternate

alias ls='ls --color=auto'
PS1='\t >>> '
