
case $IN_NIX_SHELL in
	pure)
		# export PATH=$(cat "$HOME/.my-initial-env")
		return
esac

if command -v guix 1>/dev/null 2>/dev/null
then
	if test -z "$GUIX_ENVIRONMENT"
	then
		if test -z "$GUIX_PROFILE" || ! test -d "$GUIX_PROFILE"
		then
			if test -d "$HOME/.config/guix/current"
			then export GUIX_PROFILE="$HOME/.config/guix/current"
			else
				if test -d "$HOME/.guix-profile"
				then export GUIX_PROFILE="$HOME/.guix-profile"
				fi
			fi
			. "$GUIX_PROFILE/etc/profile"
			export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"
		fi
	fi
fi

###########
## PATHS ##
###########

test -d "$HOME/.cabal/bin" && PATH="${HOME}/.cabal/bin:${PATH}"
test -d "$HOME/.cargo/bin" && PATH="${HOME}/.cargo/bin:${PATH}"
test -d "$HOME/.local/nodejs/bin" && PATH="${HOME}/.local/nodejs/bin:${PATH}"
test -d "/usr/local/go/bin" && PATH="/usr/local/go/bin:${PATH}"
test -d "$HOME/go/bin" && PATH="$HOME/go/bin:${PATH}"

# Some XDG hack to make flatpak unbug itself
export XDG_DATA_DIRS="/usr/share/gnome:/usr/local/share/:/usr/share/:$XDG_DATA_DIRS"

# Read nix env and fix archive directory (https://github.com/nix-community/home-manager/issues/354)
if test -d "$HOME/.nix-profile"
then
	test -e "$HOME/.nix-profile/etc/profile.d/nix.sh" && \
		. "$HOME/.nix-profile/etc/profile.d/nix.sh"
	test -e '$HOME/.nix-profile/etc/profile.d/nix-daemon.sh' && \
		. '$HOME/.nix-profile/etc/profile.d/nix-daemon.sh'
	test -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' && \
		. '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
	test -e '/nix/var/nix/profiles/default/etc/profile.d/nix.sh' && \
		. '/nix/var/nix/profiles/default/etc/profile.d/nix.sh'
	export LOCALE_ARCHIVE=$(nix-build --no-out-link '<nixpkgs>' -A glibcLocales 2>/dev/null)/lib/locale/locale-archive
fi

export NVM_DIR="$HOME/.nvm"
test -s "$NVM_DIR/nvm.sh" && \. "$NVM_DIR/nvm.sh"  # This loads nvm

## Remove duplicates (disabled because bugged on nixos)
# PATH=$(echo "$PATH" | awk -v 'RS=:' -v 'ORS=:' '!($0 in a) {a[$0]; print}')

# prepend ~.local/bin to PATH
PATH="${HOME}/.local/bin:${PATH}"

export PATH

# Chibi scheme load path
if test -z "$CHIBI_MODULE_PATH"
then export CHIBI_MODULE_PATH="$HOME/.local/share/chibi:$HOME/.local/lib/chibi"
else export CHIBI_MODULE_PATH="$HOME/.local/share/chibi:$HOME/.local/lib/chibi"
fi

###############
## VARIABLES ##
###############

if command -v hostname 1>/dev/null 2>/dev/null
then
	export MY_HOSTNAME="$(hostname)"
else
	if test -f /etc/hostname
	then export MY_HOSTNAME="$(cat /etc/hostname)"
	fi
fi

export MY_ROOT="$HOME/my"
export MY_MEDIA="$MY_ROOT/media"
export MY_SESSIONS_ROOT="$MY_MEDIA/text/other/sessions"

if test -f "$MY_ROOT/var/private-profile.sh"
then . "$MY_ROOT/var/private-profile.sh"
fi

# Export locale so terminals and tmux are fancy
# export LC_ALL='en_US.UTF-8'

if command -v ec 1>/dev/null 2>/dev/null && command -v emacs 1>/dev/null 2>/dev/null
then export EDITOR=ec
else export EDITOR=vi
fi

export PAGER=less
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

export SHELL=$(command -v bash 2>/dev/null)

if test -z "$FISH_SHELL"
then export FISH_SHELL=$(command -v fish 2>/dev/null)
fi

if test -n "$FISH_SHELL"
then
	export SHELL="$FISH_SHELL"
	case "$-" in
		*i*)
			if test -n "$SSH_CONNECTION"
			then exec fish
			fi
			;;
		*)
			;;
	esac
fi
