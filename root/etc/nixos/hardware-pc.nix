# Edit this configuration file to define hardware configuration.
# Then make a link hardware.nix -> this.file and use the link in configuration.nix

{ config, pkgs, ... }:

{
	# Use the systemd-boot EFI boot loader.
	boot.loader.systemd-boot.enable = true;
	boot.loader.efi.canTouchEfiVariables = true;

	networking = {
		hostName = "pc"; # Machine hostname.

		wireless.enable = true; # Enables wireless support via wpa_supplicant.
		# networkmanager.enable = true; # default of gnome3 and others is "true", which conflicts with networking.wireless, unless using the hack below...
		# # NOTE: this is a hack that allows networkmanager to work with networking.wireless
		# networkmanager.unmanaged = [
		# 	"*"
		# ];

		# The global useDHCP flag is deprecated, therefore explicitly set to false here.
		# Per-interface useDHCP will be mandatory in the future, so this generated config
		# replicates the default behaviour.
		useDHCP = false;
		interfaces.eno1.useDHCP = true;
		interfaces.wlo1.useDHCP = true;

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
