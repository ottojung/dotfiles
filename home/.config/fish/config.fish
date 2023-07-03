
if command -v my-safe-rm 1>/dev/null 2>/dev/null
	alias rm "my-safe-rm -v"
end

# Fix for ESHELL prompt
function fish_title
end

function fish_greeting
end

if [ -z "$MY_HOSTNAME" ]
	set MY_HOSTNAME "$(cat /etc/hostname)"
end

function get_prompt_header
	set_color purple
	printf "\n%s" "$PWD" | sed "s#^$HOME#~#"

	if [ -n "$SSH_CONNECTION" ]
		set_color red
		printf "%s" " @$MY_HOSTNAME"
	end

	set_color blue
	printf "%s" " \$$USER"

	if [ -n "$GUIX_ENVIRONMENT" ]
		set_color yellow
		printf "%s" " [env]"
	end

	set -l display_status $status
	if [ "$display_status" = 0 ]
		set_color green
	else
		set_color red
	end

	printf '\n> '
	set_color normal
end

set PPROM

function recalculate_prompt
	set PPROM "$(get_prompt_header)"
end

recalculate_prompt

function fish_prompt
	printf "%s" "$PPROM"
end

function mkcd
	mkdir -p "$argv"; and cd "$argv"
end

function my-timed
	time sh -c "$argv"
	my-notify "DONE"
end

function mvcd
	set len (count $argv)
	set last_index (math $len - 1)
	set dest "$argv[$len]"

	# Making directory using last argument
	mkdir -p "$dest" ; or return 1

	# Moving everything to directory just made
	for i in (seq 1 $last_index)
		mv -v -- "$argv[$i]" "$dest"
	end

	# CD into created directory
	cd "$dest"
end

function cpcd
	set len (count $argv)
	set last_index (math $len - 1)
	set dest "$argv[$len]"

	# Making directory using last argument
	mkdir -p "$dest" ; or return 1

	# Moving everything to directory just made
	for i in (seq 1 $last_index)
		cp -v -- "$argv[$i]" "$dest"
	end

	# CD into created directory
	cd "$dest"
end

function mvtemp
	cd (my-move-to-temporary $argv) && ls
end

set LPWD "$PWD"

function cd
	set PPWD "$PWD"

	if [ "$argv" = "" ]
		if [ "$HOME" = "" ]
			echo "BAD HOME"
			return 1
		else
			cd "$HOME"
			recalculate_prompt
		end
	else if [ "$argv" = "-" ]
		if [ "$LPWD" = "-" ]
			echo "BAD LPWD"
			return 1
		else
			cd "$LPWD"
			recalculate_prompt
		end
	else if builtin cd "$argv"
		if [ "$LPWD" = "$PPWD" ]
			true
		else
			set LPWD $PPWD
			recalculate_prompt
			true
		end
	end
end

# emacs dir tracking
if [ -n "$INSIDE_EMACS" ]
	function prompt_AnSiT -e fish_prompt
		printf "\eAnSiTc %s\n" "$PWD"
	end
	printf "\eAnSiTu %s\n" "$USER"
end

# # SSH prompt
# if [ -n "$SSH_CONNECTION" ] && [ -z "$MY_INSIDE_SSH" ]
# 	export MY_INSIDE_SSH="@$MY_HOSTNAME"
# 	export SHELL=(command -v fish)
# 	screen
# end
