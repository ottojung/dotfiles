# Edit this configuration file to define hardware configuration.
# Then make a link hardware.nix -> this.file and use the link in configuration.nix

{ config, pkgs, ... }:

{
	# Use the GRUB 2 boot loader.
	boot.loader = {
		# If using bios:
		grub.enable = true;
		grub.version = 2;

		# # If using UEFI:
		# grub.efiSupport = true;
		# grub.efiInstallAsRemovable = true;
		# efi.efiSysMountPoint = "/boot/efi";

		# Define on which hard drive you want to install Grub.
		grub.device = "/dev/sda"; # or "nodev" for efi only
	};

	networking = {
		hostName = "kek"; # Machine hostname.

		# wireless.enable = true; # Enables wireless support via wpa_supplicant.
		networkmanager.enable = true; # default of gnome3 and others is "true", which conflicts with networking.wireless, unless using the hack below...
		# # NOTE: this is a hack that allows networkmanager to work with networking.wireless
		# networkmanager.unmanaged = [
		# 	"*"
		# ];

		# The global useDHCP flag is deprecated, therefore explicitly set to false here.
		# Per-interface useDHCP will be mandatory in the future, so this generated config
		# replicates the default behaviour.
		useDHCP = false;
		interfaces.enp0s25.useDHCP = true; # NOTE: hardware dependent
		interfaces.wlp2s0.useDHCP = true; # NOTE: hardware dependent

		# Configure network proxy if necessary
		# proxy.default = "http://user:password@proxy:port/";
		# proxy.noProxy = "127.0.0.1,localhost,internal.domain";

		# Open ports in the firewall.
		# firewall.allowedTCPPorts = [ ... ];
		# firewall.allowedUDPPorts = [ ... ];
		# Or disable the firewall altogether.
		firewall.enable = false;
	};

	# bluetooth is the devil
	hardware.bluetooth.enable = false;
	# but if it is enabled, enable this as well, for pairing:
	services.blueman.enable = false;
	## INFO: https://nixos.wiki/wiki/Bluetooth

}
