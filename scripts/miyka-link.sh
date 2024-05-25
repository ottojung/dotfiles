#! /bin/sh

SOURCE="$1"
TARGET="$2"

if test -e "$TARGET"
then
    rm -rf "$SOURCE"
else
    ln -srf "$SOURCE" "$TARGET"
fi
