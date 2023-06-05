#! /bin/sh

. ~/.profile

TMP="/tmp/my-xsession-temp"
touch "$TMP"
dbus-launch --sh-syntax | sed 's/^/export /' > "$TMP"
. "$TMP"

# exec (gnome-session ; true) || gnome-shell
# exec xmonad
exec awesome
