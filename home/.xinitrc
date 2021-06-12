##########
## KEYS ##
##########

# keycode 62 - Shift_R
# keycode 64 - Alt_L
# keycode 37 - Control_L
# keycode 66 - Caps_Lock

xmodmap -e "keycode 62 = Alt_L"
xmodmap -e "remove shift = Alt_L"
xmodmap -e "add shift = Shift_R"

# does not work :(
# xmodemap -e "keycode 37 = Caps_Lock NoSymbol Caps_Lock"
# xmodemap -e "keycode 66 = Control_L NoSymbol Control_L"

#################
## WIN MANAGER ##
#################

sh ~/.xsession
