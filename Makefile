

SFLAGS =

TARGET = $(HOME)

ROOT =
DIRECTORIES = $(TARGET)/my $(TARGET)/.config $(TARGET)/.local/share $(TARGET)/Downloads $(TARGET)/Pictures $(TARGET)/.config/fish

all:
	@ echo "run '$(MAKE) root' as root followed by '$(MAKE) home' as a regular user"

root: root-make-all

home: check_requisites check-user initialize_home compile_emacs

check_requisites:
	command -v emacs || ( echo '"emacs" is needed to configure emacs' ; exit 1 )

initialize_home: directories
	./stowy $(STOWYFLAGS) run home $(TARGET)

compile_emacs:
	git -C home/.emacs.d clean -x -f -- '**/*.elc'
	emacs --batch --eval '(byte-recompile-directory "home/.emacs.d/" 0 t)' || true

miyka-initialize:
	$(MAKE) TARGET=$(MIYKA_REPO_HOME) directories
	STOWY_LINK_CMD="scripts/miyka-link.sh" ./stowy --overwrite --unsafe run home $(MIYKA_REPO_HOME)

directories: $(DIRECTORIES)

$(DIRECTORIES):
	mkdir -p $@

check-user:
	@ if test 0 = $$(id --user) ; \
	then echo Must run this makefile as non-root 1>&2 ; exit 1 ; \
	else echo Ok, you are a regular user 1>&2 ; \
	fi

.PHONY: all home finalize_home initialize_home root dirs check-user directories

include root.make

