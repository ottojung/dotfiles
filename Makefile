
SFLAGS =

ROOT =
DIRECTORIES = $(HOME)/my $(HOME)/.config $(HOME)/.local $(HOME)/Downloads $(HOME)/Pictures

all:
	@ echo "run '$(MAKE) root' as root followed by '$(MAKE) home' as a regular user"

include programs.make
include email.make
include root.make

HOMESTOWCMD = stow --restow "--target=$(HOME)" "--verbose=2" $(SFLAGS) home/

root: root-make-all

home: check_requisites check-user initialize_home initialize_private setup_email install_programs compile_emacs

check_requisites:
	which stow || ( echo '"stow" is needed to initialize home links' ; exit 1 )
	which guile || ( echo '"guile" is needed to install programs' ; exit 1 )
	which gcc || ( echo '"gcc" is needed to install programs' ; exit 1 )
	which emacs || ( echo '"emacs" is needed to configure emacs' ; exit 1 )
	which gpg || ( echo '"gpg" is needed for private stuff' ; exit 1 )
	which notmuch || ( echo '"notmuch" is needed for emails' ; exit 1 )

initialize_home: $(DIRECTORIES)
	$(HOMESTOWCMD)

initialize_private:
	cd ../config-private/ && $(MAKE)

# Use this when initialize_home fails
unsafe_home_cleanup:
	$(HOMESTOWCMD) --simulate 2>&1 | \
		grep -e 'CONFLICT when stowing' -e 'existing target is neither a link nor a directory' | \
		sed 's/.*:\s*//' | \
		xargs -I% rm -rfv "$(HOME)/%"

	cd ../config-private/ && $(MAKE) unsafe_home_cleanup

install_programs: $(PREFIXBIN)
	PATH=$(PATH):$(PREFIXBIN) $(MAKE) install_programs_go

compile_emacs:
	cd home/.emacs.d && git clean -dfx
	emacs --batch --eval '(byte-recompile-directory "home/.emacs.d/" 0 t)' || true

$(DIRECTORIES):
	mkdir -p $@

check-user:
	@ if test 0 = $$(id --user) ; \
	then echo Must run this makefile as non-root 1>&2 ; exit 1 ; \
	else echo Ok, you are a regular user 1>&2 ; \
	fi

.PHONY: all home finalize_home initialize_home root dirs check-user
