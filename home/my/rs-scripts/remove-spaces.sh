#!/bin/sh
find -maxdepth 1 -exec sh -c 'mv -v "{}" $(echo {} | sed "s/ /-/g")' \;
