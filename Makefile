

SFLAGS =

ROOT =
DIRECTORIES = $(HOME)/my $(HOME)/.config $(HOME)/.local/share $(HOME)/Downloads $(HOME)/Pictures $(HOME)/.config/fish

all:
	@ echo "run '$(MAKE) root' as root followed by '$(MAKE) home' as a regular user"

root: root-make-all

home: check_requisites check-user initialize_home compile_emacs

check_requisites:
	command -v emacs || ( echo '"emacs" is needed to configure emacs' ; exit 1 )

initialize_home: $(DIRECTORIES)
	./stowy $(STOWYFLAGS) run home $(HOME)

compile_emacs:
	git -C home/.emacs.d clean -x -f -- '**/*.elc'
	emacs --batch --eval '(byte-recompile-directory "home/.emacs.d/" 0 t)' || true

$(DIRECTORIES):
	mkdir -p $@

check-user:
	@ if test 0 = $$(id --user) ; \
	then echo Must run this makefile as non-root 1>&2 ; exit 1 ; \
	else echo Ok, you are a regular user 1>&2 ; \
	fi

.PHONY: all home finalize_home initialize_home root dirs check-user

include root.make

