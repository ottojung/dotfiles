
PREFIX = $(HOME)/.local
PREFIXBIN = $(PREFIX)/bin

PDC = $(MY_MEDIA)/text/code

install_programs_go: | $(PREFIXBIN) install_programs_go2

install_programs_go2: install_czempak install_gumak install_scripts install_shell_tools install_szcalc install_root_service install_tmenc

$(PREFIXBIN):
	mkdir -p $@

$(PDC):
	@ echo You should have $(PDC) directory present to install its contents ; exit 1

install_czempak: $(PDC)
	cd $(PDC)/czempak && $(MAKE) reinstall

install_gumak: $(PDC) install_czempak
	cd $(PDC)/gumak-dev/gumak && $(MAKE) reinstall

install_tmenc: $(PDC)
	cd $(PDC)/tmenc-core && $(MAKE) install

install_scripts: $(PDC)
	cd $(PDC)/scripts && $(MAKE) reinstall

install_shell_tools: $(PDC)
	cd $(PDC)/shell-tools && $(MAKE) reinstall

install_szcalc: $(PDC) install_czempak
	cd $(PDC)/szcalc && $(MAKE) reinstall

install_root_service: $(PDC)
	cd $(PDC)/my-root-service && $(MAKE) all-user || true

