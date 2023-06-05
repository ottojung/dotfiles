
# This file is primarily for Guix installations

SRC=$(wildcard $(HOME)/.xmonad/*.hs)

$(HOME)/.xmonad/build-x86_64-linux/Main.o: $(HOME)/.xmonad/xmonad.hs
	guix environment --ad-hoc gcc-toolchain 'gcc-toolchain:static' ghc ghc-hostname ghc-xmonad-contrib -- xmonad --recompile

xmonad: $(HOME)/.xmonad/xmonad.o

.PHONY: xmonad
