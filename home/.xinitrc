#! /bin/sh


xinitrc_body() {

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
qtile start

}

if test -z "$MY_ROOT"
then MY_ROOT="$HOME/my"
fi

mkdir -p -- "$MY_ROOT/tmp"
xinitrc_body 2>&1 > "$MY_ROOT/tmp/session-start.log"
