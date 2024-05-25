
# This file is primarily for Guix installations

SRC=$(wildcard $(TARGET)/.xmonad/*.hs)

$(TARGET)/.xmonad/build-x86_64-linux/Main.o: $(TARGET)/.xmonad/xmonad.hs
	guix environment --ad-hoc gcc-toolchain 'gcc-toolchain:static' ghc ghc-hostname ghc-xmonad-contrib -- xmonad --recompile

xmonad: $(TARGET)/.xmonad/xmonad.o

.PHONY: xmonad
