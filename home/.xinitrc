#! /bin/sh

. ~/.profile

TMP="/tmp/my-xsession-temp"
touch "$TMP"
dbus-launch --sh-syntax | sed 's/^/export /' > "$TMP"
dbus-update-activation-environment --systemd DBUS_SESSION_BUS_ADDRESS DISPLAY XAUTHORITY
. "$TMP"

# exec (gnome-session ; true) || gnome-shell
exec xmonad
# exec awesome
