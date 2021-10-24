{
allowUnfree = false;
packageOverrides = pkgs: with pkgs; {
myPackages = pkgs.buildEnv {
ignoreCollisions = true;
name = "my-packages";
paths =
	let unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
	in [

	#############
	# CLI Tools #
	#############
	time # measure time taken by programs
	pv # measures progress of the unix pipe
	man-db # default manual
	stow # a tool to deploy dotfiles
	# bc # very old calculator, provides bc and dc. Use awk instead
	fzf # command line fuzzy finder
	openssl # protocols & encryption stuff
	recode # Charset converter tool and library
	jq # JSON query language. Preprocessor
	pup # HTML processor, like jq but for HTML instead of JSON
	rlwrap # provides `readline` functionality for CLI programs
	tree # like `ls` but recursive
	entr # runs command when file is updated
	wget # downloads stuff from the internet, like curl
	curl # downloads stuff from the internet, like wget
	libnotify # provides `notify-send'
	sox # provides `play' command
	figlet # transforms text into nice drawings
	cowsay # transforms text into nice drawings
	ncdu # program for exploring filesystem

	################
	# CLI Programs #
	################
	vim # text editor
	git # version control system
	fish # shell
	htop # process monitor
	screen # terminal multiplexer a la tmux
	tmux # terminal multiplexer a la screen
	cmus # terminal music player
	# cordless # discord CLI client
	# neofetch # to look cool on the internet
	p7zip # .7z archive manager
	unzip # .zip archive extractor
	zip # .zip archive creator
	# unrar # .rar archive manager, unfree :(
	# megacmd # mega.nz command line interface, unfree :(
	# ffmpeg # video coverter
	pandoc # conversion between formats, like markdown to HTML
	scc # cloc alternative
	nitrogen # changes wallpapers
	linuxPackages.cpupower # `cpupower' utility
	lm_sensors # CPU diagnostics tool, provides `sensors' program and others
	acpi # battery diagnostics tool
	rsync # copies files between remote computers
	unison # like rsync, but for syncing
	youtube-dl # need newest version
	texlive.combined.scheme-full # distribution of TeX programs

	#############
	# Compilers #
	#############
	gcc binutils # gcc and linker and stuff
	# nodejs
	gnumake
	# guile
	# python3
	# racket
	# chez # provides `scheme' - r6rs compiler
	# chibi # provides `chibi-scheme' - r7rs interpreter
	# libck # instead of `cyclone-scheme` which is broken
	# dotnet-sdk # provides `dotnet` - C#, F# and VB.NET compilers
	# R libjpeg libpng # provides `R` - R-language interpreter
	# ghc # provides `ghci` - haskell language interpreter
	# coq # Coq language compiler
	# maxima # mathematica-like computer algebra system
	swiProlog # rich prolog implementation

	############
	# X Server #
	############
	xsel # copy-paste from terminal
	xmonad-with-packages # main xmonad program and libraries
	xmobar # panel
	mime-types # for xdg-open
	shared-mime-info # for xdg-open
	arandr # managing multiple screens
	xorg.xrandr # managing resolution and stuff
	xorg.xmodmap # managing shortcuts
	xorg.xev # captures keyboard keycodes
	compton # allows transparency and screen sharing
	scrot # making screenshots
	gromit-mpx # drawing on the screen
	# dunst # notify manager
	stalonetray # tray manager
	# trayer # tray manager

	#############
	# GUI/Small #
	#############
	pcmanfm # gui file manager
	unstable.emacs # operating system
	# lilyterm # terminal emulator
	# mlterm # terminal emulator
	alacritty # terminal emulator
	rofi # application launcher
	mpv # simple video player
	pavucontrol # audio mixer/controller
	sxiv # simple X image viewer
	transmission-gtk # torrent client
	# wicd # wifi manager, not working?

	###########
	# GUI/Big #
	###########
	### big apps
	audacity # audio editor
	tdesktop # Telegram desktop client
	# virtualbox # virtual machine (some parts are propriatary)
	virt-manager # virtual machine helper
	ungoogled-chromium # chromium without google services
	firefox # fuck mozilla
	# unstable.torbrowser # breaks
	# nyxt # doesn't work
	qutebrowser # python-based browser
	# teams # unfree :(
	obs-studio
	# flatpak
	okular # pdf-reader
	zathura # pdf-reader
	vlc # video-player. Use mpv instead
	libreoffice
	krita # paint program
	gimp # image editor
	mypaint # simple paint program

	#########
	# Fonts #
	#########
	fira-code
	fira-code-symbols

	##########
	# E-Mail #
	##########
	notmuch # mail indexer
	isync # also known as `mbsync' and uses `~/.mbsyncrc'
	msmtp # for sending mails, uses `~/.msmtprc'

	###########
	# Drivers #
	###########
	xf86_input_wacom ### wacom tablet driver
	libwacom ### wacom tablet library
	# wacomtablet # KDE thingy

	];
};
};
}
