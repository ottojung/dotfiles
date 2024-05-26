#! /bin/sh

firefox_cleanup() {
	echo "Cleaning up firefox dir..." 1>&2
	find -iname '*cache*' -exec rm -rf -v {} \;
	rm -r -f -v 'favicons.'*
	rm -r -f -v 'gmp-'*
	rm -r -f -v *safebrowsing*
}

chromium_cleanup() {
	echo "Cleaning up chromium dir..." 1>&2
	find -iname '*cache*' -not -path "./Default/Extensions/*" -exec rm -rf -v {} \;
}

cd -- "$MIYKA_REPO_HOME"

cd -- ".config/chromium" && chromium_cleanup

if test -d ".mozilla"
then
	cd ".mozilla"
	find -maxdepth 5 -type f -iname prefs.js | while IFS= read -r FILE
	do
		DIR="$(dirname "$FILE")"
		cd -- "$DIR" && firefox_cleanup
		cd -
	done
fi

echo "Cleanup finished!"
