#! /bin/sh

set -x
set -e

. ~/.profile

TMP="/tmp/my-xsession-temp"
touch "$TMP"
dbus-launch --sh-syntax | sed 's/^/export /' > "$TMP"
dbus-update-activation-environment --systemd DBUS_SESSION_BUS_ADDRESS DISPLAY XAUTHORITY
. "$TMP"

# (gnome-session ; true) || gnome-shell
# xmonad
# awesome


if test -f ~/.guix-profile/lib/libfontconfig.so
then export LD_PRELOAD="$HOME"/.guix-profile/lib/libfontconfig.so
fi

qtile start
while true
do sleep 99999
done

