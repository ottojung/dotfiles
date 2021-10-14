
ROOTSLASH = $(shell if test -z $(ROOT) ; then echo "/" ; else echo $(ROOT) ; fi)

root-make-all: check-root-requisites check-root
	$(MAKE) root-stow make-hardware-configuration check-hardware-link make-root-do

check-root-requisites:
	command -v gcc || ( echo '"gcc" is needed to compile stuff' ; exit 1 )
	command -v guile || ( echo '"guile" is needed to compile stuff' ; exit 1 )
	command -v cpupower || ( echo '"cpupower" may be required to run my-root-do' ; exit 1 )

check-root:
	@ if test 0 = $$(id --user) ; \
	then echo Ok, you are root 1>&2 ; \
	else echo Must run this makefile as root 1>&2 ; exit 1 ; \
	fi
	@ if env | grep -q -E -e '^SUDO_' ; \
	then echo But you used sudo... disgusting ; exit 1 ;\
	else echo And you did not use sudo. Well played ; \
	fi

/etc/nixos:
	mkdir -p /etc/nixos

root-stow: do-root-stow /etc/nixos
	rm -f "$(ROOT)/etc/nixos/configuration.nix"
	./stowy --overwrite --readlink $(STOWYFLAGS) run root $(ROOTSLASH)
	rm -f "$(ROOT)/etc/nixos/configuration.nix"
	cp "root/etc/nixos/configuration.nix" $(ROOT)/etc/nixos/
	@ echo "Nixos config installed. To enable it, run 'sudo nixos-rebuild switch'"

make-root-do:
	cd "../code/my-root-service" && $(MAKE) all-root

check-hardware-link:
	@ test -f $(ROOT)/etc/nixos/hardware.nix || \
		( echo File "$(ROOT)/etc/nixos/hardware.nix" is not present ; \
		echo '  do # ln -sf "hardware-$$(yourchoice).nix" "hardware.nix"' ; \
		echo ' ' in "$(ROOT)/etc/nixos" directory ; \
		true )

make-hardware-configuration:
	if command -v nixos-generate-config ; then $(MAKE) $(ROOT)/etc/nixos/hardware-configuration.nix ; fi

$(ROOT)/etc/nixos/hardware-configuration.nix:
	nixos-generate-config

.PHONY: check-hardware-link root-stow do-root-stow root-make-all make-hardware-configuration
