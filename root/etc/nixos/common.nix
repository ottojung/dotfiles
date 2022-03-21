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

	users.users.root.shell = pkgs.dash;

	# List packages installed in system profile. To search, run:
	# $ nix search wget
	environment.systemPackages = with pkgs; [
		wget vim dash
	];

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

	# # Keybase server which is for some reason needed for the GUI client
	# services.keybase.enable = true;
	# services.kbfs.enable = true;

	# This value determines the NixOS release from which the default
	# settings for stateful data, like file locations and database versions
	# on your system were taken. It‘s perfectly fine and recommended to leave
	# this value at the release version of the first install of this system.
	# Before changing this value read the documentation for this option
	# (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
	system.stateVersion = "20.09"; # Did you read the comment?

}

