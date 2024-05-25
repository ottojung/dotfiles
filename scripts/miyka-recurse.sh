#! /bin/sh

SOURCE="$1"
TARGET="$2"

if test -L "$TARGET"
then
	if ! test "$(readlink -f "$SOURCE")" = "$(readlink -f "$TARGET")"
	then
		rm -rf "$SOURCE"
		exit 1
	fi
fi
