#! /bin/sh

# set -x

# . ~/.profile

# TMP="/tmp/my-xsession-temp"
# touch "$TMP"
# dbus-launch --sh-syntax | sed 's/^/export /' > "$TMP"
# dbus-update-activation-environment --systemd DBUS_SESSION_BUS_ADDRESS DISPLAY XAUTHORITY
# . "$TMP"

# (gnome-session ; true) || gnome-shell
# xmonad
# awesome


exec env LD_PRELOAD="$HOME"/.guix-profile/lib/libfontconfig.so qtile start
