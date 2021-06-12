
# This file assumes you have deployed
# all your secret config files for email

ACCOUNTS = $(shell cat $(HOME)/.mbsyncrc | grep '^Account ' | awk '{ print $2 }')

setup_email: setup_notmuch make_email_dirs

setup_notmuch:
	cd $(HOME) && printf '\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n' | notmuch setup

make_email_dirs:
	echo "$(ACCOUNTS)" | tr ' ' '\n' | xargs --verbose -I% \
		mkdir -p "$(HOME)/.mail/%"
