#!/usr/bin/env bash

[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

# startup browser
chromium &

# screensaver
xautolock -time 30 -locker slock &

# wacom tablet config
xsetwacom set "Wacom Intuos S 2 Pen stylus" MapToOutput HEAD-0

# set cursor style
xsetroot -cursor_name left_ptr

# speed auto repeat rate up
xset r rate 250 60

exec xmonad
