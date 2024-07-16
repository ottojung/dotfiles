

TARGET = $(HOME)

ROOT =
DIRECTORIES = $(TARGET)/my $(TARGET)/.config $(TARGET)/.emacs.d $(TARGET)/.local/bin $(TARGET)/.local/share $(TARGET)/.config/fish $(TARGET)/.config/miyka

all:
	@ echo "run '$(MAKE) root' as root followed by '$(MAKE) home' as a regular user"

root: root-make-all

home: check_requisites check-user initialize_home compile_emacs

check_requisites:
	command -v emacs || ( echo '"emacs" is needed to configure emacs' ; exit 1 )

initialize_home: directories
	./stowy $(STOWYFLAGS) run home $(TARGET)

uninitialize_home: directories
	STOWY_LINK_CMD="scripts/unlink.sh" ./stowy $(STOWYFLAGS) --quiet --overwrite --keep-going run home $(TARGET)

compile_emacs:
	find ~/.emacs.d -name '*.elc' -delete
	emacs --batch --eval '(byte-recompile-directory "~/.emacs.d/" 0 t)' || true

miyka-initialize:
	$(MAKE) TARGET=$(MIYKA_REPO_HOME) directories
	STOWY_RECURSE_CMD="scripts/miyka-recurse.sh" ./stowy --keep-going run home $(MIYKA_REPO_HOME)

miyka-uninitialize:
	$(MAKE) TARGET=$(MIYKA_REPO_HOME) STOWY_RECURSE_CMD="scripts/miyka-recurse.sh" uninitialize_home

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

