#!/bin/sh

setxkbmap -layout 'pl,ru' -option 'ctrl:swapcaps,grp:win_space_toggle'
#cat $HOME/.Xmodmap | xmodmap -

TMP="$HOME/my/tmp/.my-xsession-temp"
dbus-launch --sh-syntax | sed 's/^/export /' > "$TMP"
. "$TMP"

my-desktop-initialize &

#exec (gnome-session ; true) || gnome-shell
exec xmonad
#exec awesome
