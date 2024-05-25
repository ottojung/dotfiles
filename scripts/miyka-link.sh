#! /bin/sh

SOURCE="$1"
TARGET="$2"

if test -e "$TARGET"
then
    if test -L "$TARGET"
    then
        rm -rf "$SOURCE"
    fi
else
    ln -srf "$SOURCE" "$TARGET"
fi
