#!/usr/bin/env bash

# startup browser
chromium-browser &

# startup emacs
emacs-snapshot &

# start dropbox
/home/epsalt/.dropbox-dist/dropboxd &

# screensaver
xautolock -time 30 -locker slock &

# flux
xflux -l 51 &

# wacom tablet config
xsetwacom set "Wacom Intuos S 2 Pen stylus" MapToOutput HEAD-0

exec xmonad
