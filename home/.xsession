#! /bin/sh

. "$HOME/.xsession-private"
#cat $HOME/.Xmodmap | xmodmap -

TMP="$HOME/my/tmp/.my-xsession-temp"
dbus-launch --sh-syntax | sed 's/^/export /' > "$TMP"
. "$TMP"

. "$HOME/.profile"

my-desktop-initialize &

#exec (gnome-session ; true) || gnome-shell
exec xmonad
#exec awesome
