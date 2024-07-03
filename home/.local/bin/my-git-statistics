#!/bin/sh

echo
echo Contributors:

i=0
CONTS=$(git shortlog --summary --numbered --email | while read line; do
	i=$((i + 1))
	if test $i -le 10
	then
		EMAIL=$(echo "$line" | sed 's/.* //')
		STAT=$(git log "--author=$EMAIL" --pretty=tformat: --numstat)
		NLOC=$(echo "$STAT" | awk 'BEGIN { x = 0 } { x += $1 } END { print x }')
		NMLOC=$(echo "$STAT" | awk 'BEGIN { x = 0 } { x += $2 } END { print x }')
		PROC=$(((100 * NLOC) / (NLOC + NMLOC + 1)))
		echo "$line (+$NLOC lines, $PROC%++)"
	else
		echo "$line"
	fi
done | tac)

echo "$CONTS"

echo
echo Number of contributors: $(echo "$CONTS" | wc -l)
echo Number of commits: $(git log --oneline | wc -l)
echo First commit: $(git log --reverse | sed -n -e "s/Date: *//g ; 3,3p")
echo Last commit: $(git log -1 --format=%cd)
