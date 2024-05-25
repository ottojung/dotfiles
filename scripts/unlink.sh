#! /bin/sh

SOURCE="$1"
TARGET="$2"

if test "$(readlink -f "$SOURCE")" = "$(readlink -f "$TARGET")"
then rm -vf "$TARGET"
else echo "File $TARGET links to somewhere else. Ignoring it." 1>&2
fi
