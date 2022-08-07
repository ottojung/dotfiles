# Edit this configuration file to define what should be installed on
# your system.	Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
	nixpkgs.config.allowUnfree = false;

	# Set your time zone.
	time.timeZone = "Europe/Warsaw";

	# Select internationalisation properties.
	i18n.defaultLocale = "en_US.UTF-8";
	console = {
		font = "Lat2-Terminus16";
		useXkbConfig = true;
	};

	services.xserver = {
		enable = true;

		# displayManager.sddm.enable = true; # optional as one can simply use `startx'
		# displayManager.gdm.enable = true; # NOTE: need for gnome?
		displayManager.startx.enable = true;

		# # Installed by user (package xmonad-with-extras)
		# windowManager.xmonad = {
		# 	enable = true;
		# 	enableContribAndExtras = true;
		# };

		# desktopManager.gnome3.enable = true; # discouraged
		# desktopManager.plasma5.enable = true; # optional

		# Configure keymap in X11
		layout = "pl,ru";
		xkbOptions = "ctrl:swapcaps,grp:win_space_toggle"; # Swap CapsLock and Ctrl, switch language on win+space

		# Enable touchpad support (enabled default in most desktopManagers).
		libinput = {
			enable = true;
			touchpad = {
				tapping = false;
			};
		};
	};

	# Enable sound.
	sound.enable = true;
	hardware.pulseaudio.enable = true;

	# Define a user account. Don't forget to set a password with ‘passwd’.
	users.users.admin = {
		isNormalUser = true;
		extraGroups = [ "wheel" "networkmanager" "libvirtd" ]; # 'wheel' enables ‘sudo’ for the user.
		home = "/home/admin";
		shell = pkgs.dash;
	};
	users.users.u2 = {
		isNormalUser = true;
		extraGroups = [ "networkmanager" "libvirtd" ];
		home = "/home/u2";
		shell = pkgs.dash;
	};
	users.users.root = {
		shell = pkgs.dash;
	};

	# List packages installed in system profile. To search, run:
	# $ nix search wget
	environment.systemPackages = with pkgs; [
		vim dash
	];
	security.sudo.enable = false;

	# Some programs need SUID wrappers, can be configured further or are
	# started in user sessions.
	programs.mtr.enable = true;
	programs.gnupg.agent = {
		enable = true;
		enableSSHSupport = true;
	};

	# List services that you want to enable:

	# Enable the OpenSSH daemon.
	services.openssh.enable = true;

	# Enable CUPS to print documents.
	# services.printing.enable = true;

	# Don't go to sleep when lid closes
	services.logind.lidSwitch = "lock";

	virtualisation.virtualbox.host.enable = true;
	virtualisation.libvirtd.enable = true;
	users.extraGroups.vboxusers.members = [ "libvirtd" ];

	services.flatpak.enable = true;
	xdg.portal.enable = true; # ``- To use Flatpak you must enable XDG Desktop Portals with xdg.portal.enable.''
	xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-gtk ]; # ``- Setting xdg.portal.enable to true requires a portal implementation in xdg.portal.extraPortals such as xdg-desktop-portal-gtk or xdg-desktop-portal-kde.''

	# # Keybase server which is for some reason needed for the GUI client
	# services.keybase.enable = true;
	# services.kbfs.enable = true;

	# Blocking some nasty websites
	networking.extraHosts =
		''
0.0.0.0 odysee.com
0.0.0.0 www.odysee.com
0.0.0.0 twitch.tv
0.0.0.0 www.twitch.tv
0.0.0.0 youtu.be
0.0.0.0 www.youtu.be
0.0.0.0 youtube.com
0.0.0.0 www.youtube.com
0.0.0.0 yewtu.be
0.0.0.0 www.yewtu.be
0.0.0.0 puffyan.us
0.0.0.0 www.puffyan.us
0.0.0.0 snopyta.org
0.0.0.0 www.snopyta.org
0.0.0.0 kavin.rocks
0.0.0.0 www.kavin.rocks
0.0.0.0 invidious.kavin.rocks
0.0.0.0 riverside.rocks
0.0.0.0 www.riverside.rocks
0.0.0.0 invidious.osi.kr
0.0.0.0 www.invidious.osi.kr
0.0.0.0 com.sb
0.0.0.0 www.com.sb
0.0.0.0 cthd.icu
0.0.0.0 www.cthd.icu
0.0.0.0 flokinet.to
0.0.0.0 www.flokinet.to
0.0.0.0 artemislena.eu
0.0.0.0 www.artemislena.eu
0.0.0.0 mutahar.rocks
0.0.0.0 www.mutahar.rocks
0.0.0.0 se...ivacy.com
0.0.0.0 www.se...ivacy.com
0.0.0.0 invidious.tiekoetter.com
0.0.0.0 www.invidious.tiekoetter.com
0.0.0.0 inv.bp.projectsegfau.lt
0.0.0.0 www.inv.bp.projectsegfau.lt
0.0.0.0 lunar.icu
0.0.0.0 www.lunar.icu
0.0.0.0 weblibre.org
0.0.0.0 www.weblibre.org
0.0.0.0 es...elbob.xyz
0.0.0.0 www.es...elbob.xyz
0.0.0.0 076.ne.jp
0.0.0.0 www.076.ne.jp
0.0.0.0 namazso.eu
0.0.0.0 www.namazso.eu
0.0.0.0 silkky.cloud
0.0.0.0 www.silkky.cloud
0.0.0.0 tokhmi.xyz
0.0.0.0 www.tokhmi.xyz
0.0.0.0 moomoo.me
0.0.0.0 www.moomoo.me
0.0.0.0 il.ax
0.0.0.0 www.il.ax
0.0.0.0 syncpundit.com
0.0.0.0 www.syncpundit.com
0.0.0.0 mha.fi
0.0.0.0 www.mha.fi
0.0.0.0 shimul.me
0.0.0.0 www.shimul.me
0.0.0.0 mint.lgbt
0.0.0.0 www.mint.lgbt
0.0.0.0 privacy.com.de
0.0.0.0 www.privacy.com.de
0.0.0.0 notyourcomputer.net
0.0.0.0 www.notyourcomputer.net
0.0.0.0 youtube.ac
0.0.0.0 www.youtube.ac
0.0.0.0 youtube.ad
0.0.0.0 www.youtube.ad
0.0.0.0 youtube.ae
0.0.0.0 www.youtube.ae
0.0.0.0 youtube.af
0.0.0.0 www.youtube.af
0.0.0.0 youtube.ag
0.0.0.0 www.youtube.ag
0.0.0.0 youtube.ai
0.0.0.0 www.youtube.ai
0.0.0.0 youtube.al
0.0.0.0 www.youtube.al
0.0.0.0 youtube.am
0.0.0.0 www.youtube.am
0.0.0.0 youtube.ao
0.0.0.0 www.youtube.ao
0.0.0.0 youtube.aq
0.0.0.0 www.youtube.aq
0.0.0.0 youtube.ar
0.0.0.0 www.youtube.ar
0.0.0.0 youtube.as
0.0.0.0 www.youtube.as
0.0.0.0 youtube.at
0.0.0.0 www.youtube.at
0.0.0.0 youtube.au
0.0.0.0 www.youtube.au
0.0.0.0 youtube.aw
0.0.0.0 www.youtube.aw
0.0.0.0 youtube.ax
0.0.0.0 www.youtube.ax
0.0.0.0 youtube.az
0.0.0.0 www.youtube.az
0.0.0.0 youtube.ba
0.0.0.0 www.youtube.ba
0.0.0.0 youtube.bb
0.0.0.0 www.youtube.bb
0.0.0.0 youtube.bd
0.0.0.0 www.youtube.bd
0.0.0.0 youtube.be
0.0.0.0 www.youtube.be
0.0.0.0 youtube.bf
0.0.0.0 www.youtube.bf
0.0.0.0 youtube.bg
0.0.0.0 www.youtube.bg
0.0.0.0 youtube.bh
0.0.0.0 www.youtube.bh
0.0.0.0 youtube.bi
0.0.0.0 www.youtube.bi
0.0.0.0 youtube.bj
0.0.0.0 www.youtube.bj
0.0.0.0 youtube.bm
0.0.0.0 www.youtube.bm
0.0.0.0 youtube.bn
0.0.0.0 www.youtube.bn
0.0.0.0 youtube.bo
0.0.0.0 www.youtube.bo
0.0.0.0 youtube.bq
0.0.0.0 www.youtube.bq
0.0.0.0 youtube.br
0.0.0.0 www.youtube.br
0.0.0.0 youtube.bs
0.0.0.0 www.youtube.bs
0.0.0.0 youtube.bt
0.0.0.0 www.youtube.bt
0.0.0.0 youtube.bw
0.0.0.0 www.youtube.bw
0.0.0.0 youtube.by
0.0.0.0 www.youtube.by
0.0.0.0 youtube.bz
0.0.0.0 www.youtube.bz
0.0.0.0 youtube.ca
0.0.0.0 www.youtube.ca
0.0.0.0 youtube.cc
0.0.0.0 www.youtube.cc
0.0.0.0 youtube.cd
0.0.0.0 www.youtube.cd
0.0.0.0 youtube.cf
0.0.0.0 www.youtube.cf
0.0.0.0 youtube.cg
0.0.0.0 www.youtube.cg
0.0.0.0 youtube.ch
0.0.0.0 www.youtube.ch
0.0.0.0 youtube.ci
0.0.0.0 www.youtube.ci
0.0.0.0 youtube.ck
0.0.0.0 www.youtube.ck
0.0.0.0 youtube.cl
0.0.0.0 www.youtube.cl
0.0.0.0 youtube.cm
0.0.0.0 www.youtube.cm
0.0.0.0 youtube.cn
0.0.0.0 www.youtube.cn
0.0.0.0 youtube.co
0.0.0.0 www.youtube.co
0.0.0.0 youtube.cr
0.0.0.0 www.youtube.cr
0.0.0.0 youtube.cu
0.0.0.0 www.youtube.cu
0.0.0.0 youtube.cv
0.0.0.0 www.youtube.cv
0.0.0.0 youtube.cw
0.0.0.0 www.youtube.cw
0.0.0.0 youtube.cx
0.0.0.0 www.youtube.cx
0.0.0.0 youtube.cy
0.0.0.0 www.youtube.cy
0.0.0.0 youtube.cz
0.0.0.0 www.youtube.cz
0.0.0.0 youtube.de
0.0.0.0 www.youtube.de
0.0.0.0 youtube.dj
0.0.0.0 www.youtube.dj
0.0.0.0 youtube.dk
0.0.0.0 www.youtube.dk
0.0.0.0 youtube.dm
0.0.0.0 www.youtube.dm
0.0.0.0 youtube.do
0.0.0.0 www.youtube.do
0.0.0.0 youtube.dz
0.0.0.0 www.youtube.dz
0.0.0.0 youtube.ec
0.0.0.0 www.youtube.ec
0.0.0.0 youtube.ee
0.0.0.0 www.youtube.ee
0.0.0.0 youtube.eg
0.0.0.0 www.youtube.eg
0.0.0.0 youtube.eh
0.0.0.0 www.youtube.eh
0.0.0.0 youtube.er
0.0.0.0 www.youtube.er
0.0.0.0 youtube.es
0.0.0.0 www.youtube.es
0.0.0.0 youtube.et
0.0.0.0 www.youtube.et
0.0.0.0 youtube.eu
0.0.0.0 www.youtube.eu
0.0.0.0 youtube.fi
0.0.0.0 www.youtube.fi
0.0.0.0 youtube.fj
0.0.0.0 www.youtube.fj
0.0.0.0 youtube.fk
0.0.0.0 www.youtube.fk
0.0.0.0 youtube.fm
0.0.0.0 www.youtube.fm
0.0.0.0 youtube.fo
0.0.0.0 www.youtube.fo
0.0.0.0 youtube.fr
0.0.0.0 www.youtube.fr
0.0.0.0 youtube.ga
0.0.0.0 www.youtube.ga
0.0.0.0 youtube.gd
0.0.0.0 www.youtube.gd
0.0.0.0 youtube.ge
0.0.0.0 www.youtube.ge
0.0.0.0 youtube.gf
0.0.0.0 www.youtube.gf
0.0.0.0 youtube.gg
0.0.0.0 www.youtube.gg
0.0.0.0 youtube.gh
0.0.0.0 www.youtube.gh
0.0.0.0 youtube.gi
0.0.0.0 www.youtube.gi
0.0.0.0 youtube.gl
0.0.0.0 www.youtube.gl
0.0.0.0 youtube.gm
0.0.0.0 www.youtube.gm
0.0.0.0 youtube.gn
0.0.0.0 www.youtube.gn
0.0.0.0 youtube.gp
0.0.0.0 www.youtube.gp
0.0.0.0 youtube.gq
0.0.0.0 www.youtube.gq
0.0.0.0 youtube.gr
0.0.0.0 www.youtube.gr
0.0.0.0 youtube.gs
0.0.0.0 www.youtube.gs
0.0.0.0 youtube.gt
0.0.0.0 www.youtube.gt
0.0.0.0 youtube.gu
0.0.0.0 www.youtube.gu
0.0.0.0 youtube.gw
0.0.0.0 www.youtube.gw
0.0.0.0 youtube.gy
0.0.0.0 www.youtube.gy
0.0.0.0 youtube.hk
0.0.0.0 www.youtube.hk
0.0.0.0 youtube.hm
0.0.0.0 www.youtube.hm
0.0.0.0 youtube.hn
0.0.0.0 www.youtube.hn
0.0.0.0 youtube.hr
0.0.0.0 www.youtube.hr
0.0.0.0 youtube.ht
0.0.0.0 www.youtube.ht
0.0.0.0 youtube.hu
0.0.0.0 www.youtube.hu
0.0.0.0 youtube.id
0.0.0.0 www.youtube.id
0.0.0.0 youtube.ie
0.0.0.0 www.youtube.ie
0.0.0.0 youtube.il
0.0.0.0 www.youtube.il
0.0.0.0 youtube.im
0.0.0.0 www.youtube.im
0.0.0.0 youtube.in
0.0.0.0 www.youtube.in
0.0.0.0 youtube.io
0.0.0.0 www.youtube.io
0.0.0.0 youtube.iq
0.0.0.0 www.youtube.iq
0.0.0.0 youtube.ir
0.0.0.0 www.youtube.ir
0.0.0.0 youtube.is
0.0.0.0 www.youtube.is
0.0.0.0 youtube.it
0.0.0.0 www.youtube.it
0.0.0.0 youtube.je
0.0.0.0 www.youtube.je
0.0.0.0 youtube.jm
0.0.0.0 www.youtube.jm
0.0.0.0 youtube.jo
0.0.0.0 www.youtube.jo
0.0.0.0 youtube.jp
0.0.0.0 www.youtube.jp
0.0.0.0 youtube.ke
0.0.0.0 www.youtube.ke
0.0.0.0 youtube.kg
0.0.0.0 www.youtube.kg
0.0.0.0 youtube.kh
0.0.0.0 www.youtube.kh
0.0.0.0 youtube.ki
0.0.0.0 www.youtube.ki
0.0.0.0 youtube.km
0.0.0.0 www.youtube.km
0.0.0.0 youtube.kn
0.0.0.0 www.youtube.kn
0.0.0.0 youtube.kp
0.0.0.0 www.youtube.kp
0.0.0.0 youtube.kr
0.0.0.0 www.youtube.kr
0.0.0.0 youtube.kw
0.0.0.0 www.youtube.kw
0.0.0.0 youtube.ky
0.0.0.0 www.youtube.ky
0.0.0.0 youtube.kz
0.0.0.0 www.youtube.kz
0.0.0.0 youtube.la
0.0.0.0 www.youtube.la
0.0.0.0 youtube.lb
0.0.0.0 www.youtube.lb
0.0.0.0 youtube.lc
0.0.0.0 www.youtube.lc
0.0.0.0 youtube.li
0.0.0.0 www.youtube.li
0.0.0.0 youtube.lk
0.0.0.0 www.youtube.lk
0.0.0.0 youtube.lr
0.0.0.0 www.youtube.lr
0.0.0.0 youtube.ls
0.0.0.0 www.youtube.ls
0.0.0.0 youtube.lt
0.0.0.0 www.youtube.lt
0.0.0.0 youtube.lu
0.0.0.0 www.youtube.lu
0.0.0.0 youtube.lv
0.0.0.0 www.youtube.lv
0.0.0.0 youtube.ly
0.0.0.0 www.youtube.ly
0.0.0.0 youtube.ma
0.0.0.0 www.youtube.ma
0.0.0.0 youtube.mc
0.0.0.0 www.youtube.mc
0.0.0.0 youtube.md
0.0.0.0 www.youtube.md
0.0.0.0 youtube.me
0.0.0.0 www.youtube.me
0.0.0.0 youtube.mg
0.0.0.0 www.youtube.mg
0.0.0.0 youtube.mh
0.0.0.0 www.youtube.mh
0.0.0.0 youtube.mk
0.0.0.0 www.youtube.mk
0.0.0.0 youtube.ml
0.0.0.0 www.youtube.ml
0.0.0.0 youtube.mm
0.0.0.0 www.youtube.mm
0.0.0.0 youtube.mn
0.0.0.0 www.youtube.mn
0.0.0.0 youtube.mo
0.0.0.0 www.youtube.mo
0.0.0.0 youtube.mp
0.0.0.0 www.youtube.mp
0.0.0.0 youtube.mq
0.0.0.0 www.youtube.mq
0.0.0.0 youtube.mr
0.0.0.0 www.youtube.mr
0.0.0.0 youtube.ms
0.0.0.0 www.youtube.ms
0.0.0.0 youtube.mt
0.0.0.0 www.youtube.mt
0.0.0.0 youtube.mu
0.0.0.0 www.youtube.mu
0.0.0.0 youtube.mv
0.0.0.0 www.youtube.mv
0.0.0.0 youtube.mw
0.0.0.0 www.youtube.mw
0.0.0.0 youtube.mx
0.0.0.0 www.youtube.mx
0.0.0.0 youtube.my
0.0.0.0 www.youtube.my
0.0.0.0 youtube.mz
0.0.0.0 www.youtube.mz
0.0.0.0 youtube.na
0.0.0.0 www.youtube.na
0.0.0.0 youtube.nc
0.0.0.0 www.youtube.nc
0.0.0.0 youtube.ne
0.0.0.0 www.youtube.ne
0.0.0.0 youtube.nf
0.0.0.0 www.youtube.nf
0.0.0.0 youtube.ng
0.0.0.0 www.youtube.ng
0.0.0.0 youtube.ni
0.0.0.0 www.youtube.ni
0.0.0.0 youtube.nl
0.0.0.0 www.youtube.nl
0.0.0.0 youtube.no
0.0.0.0 www.youtube.no
0.0.0.0 youtube.np
0.0.0.0 www.youtube.np
0.0.0.0 youtube.nr
0.0.0.0 www.youtube.nr
0.0.0.0 youtube.nu
0.0.0.0 www.youtube.nu
0.0.0.0 youtube.nz
0.0.0.0 www.youtube.nz
0.0.0.0 youtube.om
0.0.0.0 www.youtube.om
0.0.0.0 youtube.pa
0.0.0.0 www.youtube.pa
0.0.0.0 youtube.pe
0.0.0.0 www.youtube.pe
0.0.0.0 youtube.pf
0.0.0.0 www.youtube.pf
0.0.0.0 youtube.pg
0.0.0.0 www.youtube.pg
0.0.0.0 youtube.ph
0.0.0.0 www.youtube.ph
0.0.0.0 youtube.pk
0.0.0.0 www.youtube.pk
0.0.0.0 youtube.pl
0.0.0.0 www.youtube.pl
0.0.0.0 youtube.pm
0.0.0.0 www.youtube.pm
0.0.0.0 youtube.pn
0.0.0.0 www.youtube.pn
0.0.0.0 youtube.pr
0.0.0.0 www.youtube.pr
0.0.0.0 youtube.ps
0.0.0.0 www.youtube.ps
0.0.0.0 youtube.pt
0.0.0.0 www.youtube.pt
0.0.0.0 youtube.pw
0.0.0.0 www.youtube.pw
0.0.0.0 youtube.py
0.0.0.0 www.youtube.py
0.0.0.0 youtube.qa
0.0.0.0 www.youtube.qa
0.0.0.0 youtube.re
0.0.0.0 www.youtube.re
0.0.0.0 youtube.ro
0.0.0.0 www.youtube.ro
0.0.0.0 youtube.rs
0.0.0.0 www.youtube.rs
0.0.0.0 youtube.ru
0.0.0.0 www.youtube.ru
0.0.0.0 youtube.rw
0.0.0.0 www.youtube.rw
0.0.0.0 youtube.sa
0.0.0.0 www.youtube.sa
0.0.0.0 youtube.sb
0.0.0.0 www.youtube.sb
0.0.0.0 youtube.sc
0.0.0.0 www.youtube.sc
0.0.0.0 youtube.sd
0.0.0.0 www.youtube.sd
0.0.0.0 youtube.se
0.0.0.0 www.youtube.se
0.0.0.0 youtube.sg
0.0.0.0 www.youtube.sg
0.0.0.0 youtube.sh
0.0.0.0 www.youtube.sh
0.0.0.0 youtube.si
0.0.0.0 www.youtube.si
0.0.0.0 youtube.sk
0.0.0.0 www.youtube.sk
0.0.0.0 youtube.sl
0.0.0.0 www.youtube.sl
0.0.0.0 youtube.sm
0.0.0.0 www.youtube.sm
0.0.0.0 youtube.sn
0.0.0.0 www.youtube.sn
0.0.0.0 youtube.so
0.0.0.0 www.youtube.so
0.0.0.0 youtube.sr
0.0.0.0 www.youtube.sr
0.0.0.0 youtube.ss
0.0.0.0 www.youtube.ss
0.0.0.0 youtube.st
0.0.0.0 www.youtube.st
0.0.0.0 youtube.su
0.0.0.0 www.youtube.su
0.0.0.0 youtube.sv
0.0.0.0 www.youtube.sv
0.0.0.0 youtube.sx
0.0.0.0 www.youtube.sx
0.0.0.0 youtube.sy
0.0.0.0 www.youtube.sy
0.0.0.0 youtube.sz
0.0.0.0 www.youtube.sz
0.0.0.0 youtube.tc
0.0.0.0 www.youtube.tc
0.0.0.0 youtube.td
0.0.0.0 www.youtube.td
0.0.0.0 youtube.tf
0.0.0.0 www.youtube.tf
0.0.0.0 youtube.tg
0.0.0.0 www.youtube.tg
0.0.0.0 youtube.th
0.0.0.0 www.youtube.th
0.0.0.0 youtube.tj
0.0.0.0 www.youtube.tj
0.0.0.0 youtube.tk
0.0.0.0 www.youtube.tk
0.0.0.0 youtube.tl
0.0.0.0 www.youtube.tl
0.0.0.0 youtube.tm
0.0.0.0 www.youtube.tm
0.0.0.0 youtube.tn
0.0.0.0 www.youtube.tn
0.0.0.0 youtube.to
0.0.0.0 www.youtube.to
0.0.0.0 youtube.tr
0.0.0.0 www.youtube.tr
0.0.0.0 youtube.tt
0.0.0.0 www.youtube.tt
0.0.0.0 youtube.tv
0.0.0.0 www.youtube.tv
0.0.0.0 youtube.tw
0.0.0.0 www.youtube.tw
0.0.0.0 youtube.tz
0.0.0.0 www.youtube.tz
0.0.0.0 youtube.ua
0.0.0.0 www.youtube.ua
0.0.0.0 youtube.ug
0.0.0.0 www.youtube.ug
0.0.0.0 youtube.uk
0.0.0.0 www.youtube.uk
0.0.0.0 youtube.us
0.0.0.0 www.youtube.us
0.0.0.0 youtube.uy
0.0.0.0 www.youtube.uy
0.0.0.0 youtube.uz
0.0.0.0 www.youtube.uz
0.0.0.0 youtube.va
0.0.0.0 www.youtube.va
0.0.0.0 youtube.vc
0.0.0.0 www.youtube.vc
0.0.0.0 youtube.ve
0.0.0.0 www.youtube.ve
0.0.0.0 youtube.vg
0.0.0.0 www.youtube.vg
0.0.0.0 youtube.vi
0.0.0.0 www.youtube.vi
0.0.0.0 youtube.vn
0.0.0.0 www.youtube.vn
0.0.0.0 youtube.vu
0.0.0.0 www.youtube.vu
0.0.0.0 youtube.wf
0.0.0.0 www.youtube.wf
0.0.0.0 youtube.ws
0.0.0.0 www.youtube.ws
0.0.0.0 youtube.ye
0.0.0.0 www.youtube.ye
0.0.0.0 youtube.yt
0.0.0.0 www.youtube.yt
0.0.0.0 youtube.za
0.0.0.0 www.youtube.za
0.0.0.0 youtube.zm
0.0.0.0 www.youtube.zm
0.0.0.0 youtube.zw
0.0.0.0 www.youtube.zw
0.0.0.0 youtube.xn--lgbbat1ad8j
0.0.0.0 www.youtube.xn--lgbbat1ad8j
0.0.0.0 youtube.xn--y9a3aq
0.0.0.0 www.youtube.xn--y9a3aq
0.0.0.0 youtube.xn--mgbcpq6gpa1a
0.0.0.0 www.youtube.xn--mgbcpq6gpa1a
0.0.0.0 youtube.xn--54b7fta0cc
0.0.0.0 www.youtube.xn--54b7fta0cc
0.0.0.0 youtube.xn--90ais
0.0.0.0 www.youtube.xn--90ais
0.0.0.0 youtube.xn--90ae
0.0.0.0 www.youtube.xn--90ae
0.0.0.0 youtube.xn--fiqs8s
0.0.0.0 www.youtube.xn--fiqs8s
0.0.0.0 youtube.xn--fiqz9s
0.0.0.0 www.youtube.xn--fiqz9s
0.0.0.0 youtube.xn--wgbh1c
0.0.0.0 www.youtube.xn--wgbh1c
0.0.0.0 youtube.xn--e1a4c
0.0.0.0 www.youtube.xn--e1a4c
0.0.0.0 youtube.xn--qxa6a
0.0.0.0 www.youtube.xn--qxa6a
0.0.0.0 youtube.xn--node
0.0.0.0 www.youtube.xn--node
0.0.0.0 youtube.xn--qxam
0.0.0.0 www.youtube.xn--qxam
0.0.0.0 youtube.xn--j6w193g
0.0.0.0 www.youtube.xn--j6w193g
0.0.0.0 youtube.xn--h2brj9c
0.0.0.0 www.youtube.xn--h2brj9c
0.0.0.0 youtube.xn--mgbbh1a71e
0.0.0.0 www.youtube.xn--mgbbh1a71e
0.0.0.0 youtube.xn--fpcrj9c3d
0.0.0.0 www.youtube.xn--fpcrj9c3d
0.0.0.0 youtube.xn--gecrj9c
0.0.0.0 www.youtube.xn--gecrj9c
0.0.0.0 youtube.xn--s9brj9c
0.0.0.0 www.youtube.xn--s9brj9c
0.0.0.0 youtube.xn--xkc2dl3a5ee0h
0.0.0.0 www.youtube.xn--xkc2dl3a5ee0h
0.0.0.0 youtube.xn--45brj9c
0.0.0.0 www.youtube.xn--45brj9c
0.0.0.0 youtube.xn--2scrj9c
0.0.0.0 www.youtube.xn--2scrj9c
0.0.0.0 youtube.xn--rvc1e0am3e
0.0.0.0 www.youtube.xn--rvc1e0am3e
0.0.0.0 youtube.xn--45br5cyl
0.0.0.0 www.youtube.xn--45br5cyl
0.0.0.0 youtube.xn--3hcrj9c
0.0.0.0 www.youtube.xn--3hcrj9c
0.0.0.0 youtube.xn--mgbbh1a
0.0.0.0 www.youtube.xn--mgbbh1a
0.0.0.0 youtube.xn--h2breg3eve
0.0.0.0 www.youtube.xn--h2breg3eve
0.0.0.0 youtube.xn--h2brj9c8c
0.0.0.0 www.youtube.xn--h2brj9c8c
0.0.0.0 youtube.xn--mgbgu82a
0.0.0.0 www.youtube.xn--mgbgu82a
0.0.0.0 youtube.xn--mgba3a4f16a
0.0.0.0 www.youtube.xn--mgba3a4f16a
0.0.0.0 youtube.xn--mgbtx2b
0.0.0.0 www.youtube.xn--mgbtx2b
0.0.0.0 youtube.xn--4dbrk0ce
0.0.0.0 www.youtube.xn--4dbrk0ce
0.0.0.0 youtube.xn--mgbayh7gpa
0.0.0.0 www.youtube.xn--mgbayh7gpa
0.0.0.0 youtube.xn--80ao21a
0.0.0.0 www.youtube.xn--80ao21a
0.0.0.0 youtube.xn--q7ce6a
0.0.0.0 www.youtube.xn--q7ce6a
0.0.0.0 youtube.xn--mix082f
0.0.0.0 www.youtube.xn--mix082f
0.0.0.0 youtube.xn--mix891f
0.0.0.0 www.youtube.xn--mix891f
0.0.0.0 youtube.xn--mgbx4cd0ab
0.0.0.0 www.youtube.xn--mgbx4cd0ab
0.0.0.0 youtube.xn--mgbah1a3hjkrd
0.0.0.0 www.youtube.xn--mgbah1a3hjkrd
0.0.0.0 youtube.xn--l1acc
0.0.0.0 www.youtube.xn--l1acc
0.0.0.0 youtube.xn--mgbc0a9azcg
0.0.0.0 www.youtube.xn--mgbc0a9azcg
0.0.0.0 youtube.xn--d1alf
0.0.0.0 www.youtube.xn--d1alf
0.0.0.0 youtube.xn--mgb9awbf
0.0.0.0 www.youtube.xn--mgb9awbf
0.0.0.0 youtube.xn--mgbai9azgqp6j
0.0.0.0 www.youtube.xn--mgbai9azgqp6j
0.0.0.0 youtube.xn--ygbi2ammx
0.0.0.0 www.youtube.xn--ygbi2ammx
0.0.0.0 youtube.xn--wgbl6a
0.0.0.0 www.youtube.xn--wgbl6a
0.0.0.0 youtube.xn--p1ai
0.0.0.0 www.youtube.xn--p1ai
0.0.0.0 youtube.xn--mgberp4a5d4ar
0.0.0.0 www.youtube.xn--mgberp4a5d4ar
0.0.0.0 youtube.xn--90a3ac
0.0.0.0 www.youtube.xn--90a3ac
0.0.0.0 youtube.xn--yfro4i67o
0.0.0.0 www.youtube.xn--yfro4i67o
0.0.0.0 youtube.xn--clchc0ea0b2g2a9gcd
0.0.0.0 www.youtube.xn--clchc0ea0b2g2a9gcd
0.0.0.0 youtube.xn--3e0b707e
0.0.0.0 www.youtube.xn--3e0b707e
0.0.0.0 youtube.xn--fzc2c9e2c
0.0.0.0 www.youtube.xn--fzc2c9e2c
0.0.0.0 youtube.xn--xkc2al3hye2a
0.0.0.0 www.youtube.xn--xkc2al3hye2a
0.0.0.0 youtube.xn--mgbpl2fh
0.0.0.0 www.youtube.xn--mgbpl2fh
0.0.0.0 youtube.xn--ogbpf8fl
0.0.0.0 www.youtube.xn--ogbpf8fl
0.0.0.0 youtube.xn--kprw13d
0.0.0.0 www.youtube.xn--kprw13d
0.0.0.0 youtube.xn--kpry57d
0.0.0.0 www.youtube.xn--kpry57d
0.0.0.0 youtube.xn--o3cw4h
0.0.0.0 www.youtube.xn--o3cw4h
0.0.0.0 youtube.xn--pgbs0dh
0.0.0.0 www.youtube.xn--pgbs0dh
0.0.0.0 youtube.xn--j1amh
0.0.0.0 www.youtube.xn--j1amh
0.0.0.0 youtube.xn--mgbaam7a8h
0.0.0.0 www.youtube.xn--mgbaam7a8h
0.0.0.0 youtube.xn--mgb2ddes
0.0.0.0 www.youtube.xn--mgb2ddes
0.0.0.0 youtube.ad
0.0.0.0 www.youtube.ad
0.0.0.0 youtube.as
0.0.0.0 www.youtube.as
0.0.0.0 youtube.az
0.0.0.0 www.youtube.az
0.0.0.0 youtube.bz
0.0.0.0 www.youtube.bz
0.0.0.0 youtube.cc
0.0.0.0 www.youtube.cc
0.0.0.0 youtube.cd
0.0.0.0 www.youtube.cd
0.0.0.0 youtube.co
0.0.0.0 www.youtube.co
0.0.0.0 youtube.dj
0.0.0.0 www.youtube.dj
0.0.0.0 youtube.fm
0.0.0.0 www.youtube.fm
0.0.0.0 youtube.gg
0.0.0.0 www.youtube.gg
0.0.0.0 youtube.io
0.0.0.0 www.youtube.io
0.0.0.0 youtube.la
0.0.0.0 www.youtube.la
0.0.0.0 youtube.me
0.0.0.0 www.youtube.me
0.0.0.0 youtube.ms
0.0.0.0 www.youtube.ms
0.0.0.0 youtube.nu
0.0.0.0 www.youtube.nu
0.0.0.0 youtube.NL
0.0.0.0 www.youtube.NL
0.0.0.0 youtube.sc
0.0.0.0 www.youtube.sc
0.0.0.0 youtube.tf
0.0.0.0 www.youtube.tf
0.0.0.0 youtube.tv
0.0.0.0 www.youtube.tv
0.0.0.0 youtube.ws
0.0.0.0 www.youtube.ws
		'';

	# This value determines the NixOS release from which the default
	# settings for stateful data, like file locations and database versions
	# on your system were taken. It‘s perfectly fine and recommended to leave
	# this value at the release version of the first install of this system.
	# Before changing this value read the documentation for this option
	# (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
	system.stateVersion = "20.09"; # Did you read the comment?

}

