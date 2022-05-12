
ROOTSLASH = $(shell if test -z $(ROOT) ; then echo "/" ; else echo $(ROOT) ; fi)

root-make-all: check-root-requisites check-root
	$(MAKE) root-stow make-hardware-configuration check-hardware-link

$(ROOT)/etc/nixos:
	mkdir -p /etc/nixos

check-root-requisites:
	# No dependencies

check-root:
	@ if test 0 = $$(id --user) ; \
	then echo Ok, you are root 1>&2 ; \
	else echo Must run this makefile as root 1>&2 ; exit 1 ; \
	fi

root-stow: do-root-stow $(ROOT)/etc/nixos
	rm -f "$(ROOT)/etc/nixos/configuration.nix"
	./stowy --overwrite --readlink $(STOWYFLAGS) run root $(ROOTSLASH)
	rm -f "$(ROOT)/etc/nixos/configuration.nix"
	cp "root/etc/nixos/configuration.nix" $(ROOT)/etc/nixos/
	@ echo "Nixos config installed. To enable it, run 'sudo nixos-rebuild switch'"

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
