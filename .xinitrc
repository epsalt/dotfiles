#!/usr/bin/env bash

[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

# startup browser
chromium &

# # startup emacs
emacsclient -c &

# screensaver
xautolock -time 30 -locker slock &

# wacom tablet config
xsetwacom set "Wacom Intuos S 2 Pen stylus" MapToOutput HEAD-0

# set cursor style
xsetroot -cursor_name left_ptr

exec xmonad
