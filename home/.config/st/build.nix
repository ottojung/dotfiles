with import <nixpkgs> {}; {
	stEnv = stdenv.mkDerivation {
		name = "stEnv";
		buildInputs = [ gnutar gzip gnumake gcc binutils-unwrapped coreutils gawk gnused gnugrep patchelf findutils pkg-config xorg.libX11 xorg.libXft ];
	};
}