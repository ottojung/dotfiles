
if [[ -z "$MY_SHELL_INITIALIZED" ]]
then . ~/.profile
fi

case $IN_NIX_SHELL in
	pure)
		export PATH=$(cat "$HOME/.my-initial-env")
		return
esac

if [[ $- != *i* ]]
then
	# We are being invoked from a non-interactive shell.  If this
	# is an SSH session (as in "ssh host command"), source
	# /etc/profile so we get PATH and other essential variables.
	[[ -n "$SSH_CLIENT" ]] && source /etc/profile

	# Don't do anything else.
	return
fi

# Source the system-wide file.
test -f /etc/bashrc && source /etc/bashrc

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
export HISTCONTROL=ignoreboth

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
export HISTSIZE=100000
export HISTFILESIZE=20000

# append to the history file, don't overwrite it
shopt -s histappend

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
	. /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
	. /etc/bash_completion
  fi
fi

# change PS1
if [ -n "$color_prompt" ]
then PS1="\n\[\033[01;34m\]\W\[\033[00m\] \[\033[01;32m\]▶\[\033[00m\] "
else PS1="\n\W > "
fi

if [ -n "$TASKS_PRINT_AT_START" ] && command -v task 2>/dev/null 1>/dev/null
then
	task minimal
	export TASKS_PRINT_AT_START=0
fi

#############
## ALIASES ##
#############

if command -v my-safe-rm 1>/dev/null 2>/dev/null
then alias rm="my-safe-rm"
fi

# LPWD=$PWD

# # on change cd - also change gnome-terminal title
# # https://unix.stackexchange.com/questions/116955/where-is-cd-located
# # https://askubuntu.com/questions/22413/how-to-change-gnome-terminal-title
# cd() {
# 	[ "$LWD" == "$PWD" ] || LPWD=$PWD
# 	builtin cd # "$@" && __change_terminal_title
# }

__change_terminal_title_real() {
		wd="$PWD"
		if  [[ $PWD == ${HOME}* ]] ; then
				homelen=${#HOME}
				wd="~${PWD:$homelen}"
		fi
		echo -ne '\033]0;' "$wd" '\007'
}

case "$TERM" in
	xterm-256color)
		__change_terminal_title_wrap=__change_terminal_title_real
		;;
	*)
		__change_terminal_title_wrap=
		;;
esac

__change_terminal_title() {
	$__change_terminal_title_wrap
}

__change_terminal_title

mkcd() {
	mkdir -p "$@" && cd "$@"
}

mvcd() {
	dest="${@:$#:1}"

	# Making directory using last argument
	mkdir -p "$dest"

	# Moving everything to directory just made
	for i in $(seq 1 $(($# - 1)))
	do
		mv "${@:$i:1}" "$dest"
	done

	# CD into created directory
	cd "$dest"
}


