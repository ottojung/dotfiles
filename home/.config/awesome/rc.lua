-- Standard awesome library
gears = require("gears")
awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
wibox = require("wibox")
-- Theme handling library
beautiful = require("beautiful")
-- Notification library
naughty = require("naughty")

--------------------------
-- Definitions          --
--------------------------

function debug_notify(object)
	naughty.notify({
		 preset = naughty.config.presets.critical,
		 text = gears.debug.dump_return(object) })
end

local terminal = "x-terminal-emulator"
local editor = os.getenv("EDITOR") or "vi"
local editor_cmd = terminal .. " -e " .. editor
local modkey = "Mod4" -- Mod4 is "Windows" key
local temp_group_name = "t"

local globalkeys = {} -- mutable
local clientkeys = {} -- mutable
theme = {} -- mutable
local tags_table = {} -- mutable, 2 dimensional, key1 is screen index, key2 is tag name
local screens_table = {} -- mutable array

local current_tag = nil -- mutable
local last_tag = nil	-- mutable

function table_range_inclusive(start, finish)
	local ret = {}
	for i = start, finish do
		ret[i] = i
	end
	return ret
end

function table_get_length(t)
	local count = 0
	for _ in pairs(t) do count = count + 1 end
	return count
end

function table_map(t, fn)
	local ret = {}
	for i, v in pairs(t) do
		ret[i] = fn(v)
	end
	return ret
end

function table_find_index(t, element)
	local ret = {}
	for i, v in pairs(t) do
		if v == element then
			return i
		end
	end
	return nil
end

function list_table_init(table_that_is_also_a_list)
	local ret = {}
	local last = nil
	local started = false
	for _, v in pairs(table_that_is_also_a_list)  do
		if started
		then table.insert(ret, last)
		else started = true
		end
		last = v
	end
	return ret
end

function list_table_last(table_that_is_also_a_list)
	local last = nil
	for _, v in pairs(table_that_is_also_a_list) do
		last = v
	end
	return last
end

function join_two_tables(a, b)
	local ret = {}
	for i, v in pairs(a) do
		table.insert(ret, v)
	end
	for i, v in pairs(b) do
		table.insert(ret, v)
	end
	return ret
end

function make_default_modifiers(mods)
	if type(mods) == "table"
	then return join_two_tables({modkey}, mods)
	else return {modkey, mods}
	end
end

function make_key(key)

	local key_code = key[1]
	local modifiers = nil
	local actual_key = nil

	if type(key_code) == "string"
	then
		actual_key = key_code
		modifiers = { modkey, nil }
	else
		actual_key = list_table_last(key_code)
		modifiers = make_default_modifiers(list_table_init(key_code))
	end

	local awfulkey = awful.key(modifiers, actual_key, key[2], key[3])
	local descriptor = {
		modifiers = modifiers,
		actual_key = actual_key,
		fn = key[2],
		desc = key[3],
		awfulkey = awfulkey,
	}

	return descriptor
end

function table_any(t, predicate)
	for i, v in pairs(t) do
		if predicate(v) then return true end
	end
	return false
end

function table_equals1(a, b)
	if a == nil or b == nil
	then return a == b
	end

	for i, v in pairs(a) do
		if b[i] ~= v then return false end
	end
	for i, v in pairs(b) do
		if a[i] ~= v then return false end
	end
	return true
end

function key_exists_in(t, pre_raw, c)
	local pre = make_default_modifiers(pre_raw)

	function predicate(val)
		return (table_equals1(val.modifiers, pre) and (val.actual_key == c))
	end

	return table_any(t, predicate)
end

function append_table_key(t, key)
	table.insert(t, make_key(key))
	return t
end

function append_table_keys(t, keys)
	for i, k in pairs(keys) do
		append_table_key(t, k)
	end
	return t
end

function append_global_key(key)
	return append_table_key(globalkeys, key)
end

function append_global_keys(keys)
	return append_table_keys(globalkeys, keys)
end

function client_toggle_fullscreen(c)
	c.fullscreen = not c.fullscreen
	c:raise()
end

function key_table_to_awful_key_table(t)
	function get_awful(v)
		return v.awfulkey
	end

	-- Note: below doesn't work on some versions of awesome. Don't wanna know why
	-- return gears.table.join(unpack(table_map(t, get_awful)))

	local ret = {}
	for _, k in pairs(t) do
		local a = get_awful(k)
		for _, v in pairs(a) do
			table.insert(ret, v)
		end
	end

	return ret
end

function minimize_client(c)
	c.minimized = true
	adjust_border_sizes()
end

function toggle_floating_client(c)
	awful.client.floating.toggle(c)
	adjust_border_sizes()
end

local digits = { "1", "2", "3", "4", "5", "6", "7", "8", "9", "0" }
local alphabet = { "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z" }
local alphanum = join_two_tables(digits, alphabet)
local sub_desktop_names = { "1", "2", "3", "4", "5", "6", "7", "8", "9" }
local group_names = alphabet

local special_keys =
	{ "equal", "minus", "Up", "Down", "Left", "Right", "KP_Add", "KP_Subtract", "Delete", "Print", "Return" }

local groups_table =
	(function ()
		local ret = {}
		for i, a in pairs(group_names) do
			local obj = {
				name = a,
				last_subdesktop = "1",
			}
			ret[a] = obj
		end
		return ret
	end)()

local taglist =
	(function ()
		local ret = {}
		for i, a in pairs(group_names) do
			for k, v in pairs(sub_desktop_names) do
				if v == "1" then
					table.insert(ret, a)
				else
					table.insert(ret, a .. "/" .. v)
				end
			end
		end
		return ret
	end)()

function get_current_screen()
	return awful.screen.focused()
end

function hide_panel()
	local screen = get_current_screen()
	local wib = screen.mywibox
	wib.visible = false
end

function show_panel()
	local screen = get_current_screen()
	local wib = screen.mywibox
	wib.visible = true
end

function toggle_panel()
	local screen = get_current_screen()
	local wib = screen.mywibox
	wib.visible = not wib.visible
end

function on_screen_history_update()
	local screen = get_current_screen()
	local new = screen.selected_tag

	if not (current_tag == new) then
		last_tag = current_tag
		current_tag = new
	end

	local name = get_current_tag_name()
	local group_name, desktop_name = destructure_tag_name(name)
	local g = get_group_by_name(group_name)
	g.last_subdesktop = desktop_name
end

function next_screen(s)
	local len = table_get_length(screens_table)
	local current_index = table_find_index(screens_table, s)
	if current_index >= len
	then return screens_table[1]
	else return screens_table[current_index + 1]
	end
end

function focus_next_screen()
	awful.screen.focus(next_screen(get_current_screen()))
end

function move_to_next_screen(c)
	local s = next_screen(get_current_screen())
	local focused_tag = s.selected_tag
	client_move_to_tag(c, focused_tag)
	awful.screen.focus(s)
end

function get_foreach_screen_fn()
	local index = 1

	return function(s)
		table.insert(screens_table, s)

		local mytextclock = wibox.widget.textclock()

		awful.tag(taglist, s, awful.layout.layouts[1])

		init_tags_table_on_screen(s)

		s:connect_signal("tag::history::update", on_screen_history_update)

		-- Create a taglist widget
		s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.noempty, taglist_buttons)

		-- Create a tasklist widget
		s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.focused, tasklist_buttons)

		-- Create the wibox
		s.mywibox = awful.wibar({ position = "top", screen = s })

		-- Add widgets to the wibox
		s.mywibox:setup {
			layout = wibox.layout.align.horizontal,
			{ -- Left widgets
				layout = wibox.layout.fixed.horizontal,
				s.mytaglist,
			},
			s.mytasklist, -- Middle widget
			{ -- Right widgets
				layout = wibox.layout.fixed.horizontal,
				wibox.widget.systray(),
				mytextclock,
			},
		}
	end
end

function spawn_check(ret, onerror)
	if onerror == nil then
		onerror = function()
		naughty.notify({ preset = naughty.config.presets.critical,
						  title = "Spawn error",
						  text = ret })
		end
	end

	local t = tostring(type(ret))
	if t == 'number' then
		return true
	elseif t == 'string' then
		onerror(ret)
		return false
	else
		naughty.notify({ preset = naughty.config.presets.critical,
						title = "Spawn error: unknown return type",
						text = t })
		return false
	end
end

function spawn_normal(command)
	spawn_check(awful.spawn(command))
end

function spawn_with_shell_cb(stdout, stderr, exitreason, exitcode)
	if exitcode ~= 0 then
		naughty.notify({ preset = naughty.config.presets.critical,
						title = "Shell spawn failed with: " .. tostring(exitcode),
						text = stderr })
	end
end

function spawn_with_shell(command)
	spawn_check(awful.spawn.easy_async_with_shell(command, spawn_with_shell_cb))
end

function append_dynamic_keys()
	function onerror (ret)
		naughty.notify({ preset = naughty.config.presets.low,
						title = "Spawn dynamic error",
						text = ret })
	end

	function spawn_dynamic(pre, c)
		local prefix = ""
		if pre ~= nil
		then prefix = pre .. "-"
		end

		local cmd = "awesomewm-key-" .. prefix .. c
		local lower = cmd:lower()

		return function()
			spawn_check(awful.spawn(lower, { focus = true }), onerror)
		end
	end

	local keys = join_two_tables(alphanum, special_keys)

	function check(pre, c)
		return not (key_exists_in(globalkeys, pre, c) or key_exists_in(clientkeys, pre, c))
	end

	function add(k, pre, c)
		if check(pre, c) then
			append_global_key({k, spawn_dynamic(pre, c)})
		end
	end

	for _,c in pairs(keys) do
		add(c, nil, c)
		add({"Control", c}, "Control", c)
		add({"Shift", c}, "Shift", c)
	end
end

function on_geometry_hook(client)
	client.maximized = false
end

function append_sub_desktop_keys()
	local r = table_range_inclusive(1, 9)

	function sw(i)
		function switcher()
			switch_to_sub_desktop(i)
		end
		return {tostring(i), switcher}
	end
	local smap = table_map(r, sw)
	append_global_keys(smap)

	function mv(i)
		function mover(c)
			move_to_sub_desktop(c, i)
		end
		return {{"Shift", tostring(i)}, mover}
	end
	local mmap = table_map(r, mv)
	append_table_keys(clientkeys, mmap)
end

function append_groups_keys()
	local r = alphabet

	function sw(i)
		function switcher()
			switch_to_group(i)
		end
		return {{"Control", i}, switcher}
	end
	local gmap = table_map(r, sw)
	append_global_keys(gmap)

	function mv(i)
		function mover(c)
			move_to_group(c, i)
		end
		return {{"Control", "Shift", i}, mover}
	end
	local cmap = table_map(r, mv)
	append_table_keys(clientkeys, cmap)
end

function on_unmanage_hook(t)
	local cur = get_current_tag()
	adjust_border_sizes()
end

function on_manage_hook(c)
	adjust_border_sizes()

	-- Set the windows at the slave,
	-- i.e. put it at the end of others instead of setting it master.
	-- if not awesome.startup then awful.client.setslave(c) end
	if awesome.startup and
		not c.size_hints.user_position
	and not c.size_hints.program_position then
		-- Prevent clients from being unreachable after screen count changes.
		awful.placement.no_offscreen(c)
	end
end

function on_mouse_enter_hook(c)
	if (awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
		and awful.client.focus.filter(c)) then
		client.focus = c
	end
end

function count_visible_clients_on(t) -- t is tag
	local cl = t:clients()
	local re = 0
	local list = {}
	if cl ~= nil then
		for i, c in pairs(cl) do
			if not c.minimized and not c.floating then
				re = re + 1
				table.insert(list, c)
			end
		end
	end
	return re, list
end

function should_be_borderless(t) -- t is tag
	if t.layout == awful.layout.suit.max
	then return true, (t:clients())
	else
		local count, list = count_visible_clients_on(t)
		return (count <= 1), list
	end
end

function client_remove_borders(c)
	c.border_width = 0
end

function client_add_borders(c)
	c.border_width = beautiful.border_width
end

function adjust_border_sizes()
	local should_be, list = should_be_borderless(get_current_tag())
	if should_be
	then
		for i, c in pairs(list) do
			client_remove_borders(c)
		end
	else
		for i, c in pairs(list) do
			client_add_borders(c)
		end
	end
end

function check_floating(c)
	if (not c.ontop) and c.floating and c.first_tag ~= nil and c.first_tag.layout ~= awful.layout.suit.floating then
		c.ontop = true
	else
		c.ontop = false
	end
end

function on_focus_hook(c)
	c.border_color = beautiful.border_focus
	check_floating(c)
end

function on_unfocus_hook(c)
	c.border_color = beautiful.border_normal
	check_floating(c)
end

function restore_minimized()
	local c = awful.client.restore()
	-- Focus restored client
	if c then
		client.focus = c
		c:raise()
	end

	adjust_border_sizes()
end

function tag_get_left_name(tag)
	local name = tag.name
	local group_name, desktop_name = destructure_tag_name(name)
	local index = tonumber(desktop_name)
	local left_index = index - 1
	if left_index < 1
	then left_index = 9
	end

	local left = tostring(left_index)
	return group_make_desktop_fullname(group_name, left)
end

function tag_get_left(tag)
	local name = tag_get_left_name(tag)
	local tag = get_tag_by_name(name)
	return tag
end

function tag_get_right_name(tag)
	local name = tag.name
	local group_name, desktop_name = destructure_tag_name(name)
	local index = tonumber(desktop_name)
	local right_index = index + 1
	if right_index > 9
	then right_index = 1
	end

	local right = tostring(right_index)
	return group_make_desktop_fullname(group_name, right)
end

function tag_get_right(tag)
	local name = tag_get_right_name(tag)
	local tag = get_tag_by_name(name)
	return tag
end

function client_move_to_tag(c, target)
	c:move_to_tag(target)
	adjust_border_sizes()
end

function client_move_left(c)
	local t = c.first_tag or nil
	if t == nil then return end
	local tag = tag_get_left(t)
	client_move_to_tag(c, tag)
	switch_to_tag(tag)
end

function client_move_right(c)
	local t = c.first_tag or nil
	if t == nil then return end
	local tag = tag_get_right(t)
	client_move_to_tag(c, tag)
	switch_to_tag(tag)
end

function view_left_tag()
	local t = get_current_tag()
	local tag = tag_get_left(t)
	switch_to_tag(tag)
end

function view_right_tag()
	local t = get_current_tag()
	local tag = tag_get_right(t)
	switch_to_tag(tag)
end

function switch_to_last_tag()
	switch_to_tag(last_tag)
end

function switch_to_last_window()
	awful.client.focus.history.previous()
	if client.focus then
		client.focus:raise()
	end
end

function destructure_tag_name(tag_name)
	local len = string.len(tag_name)
	local group = string.sub(tag_name, 1, 1)
	local sub_desktop = "1"

	if len == 3
	then sub_desktop = string.sub(tag_name, 3, 3)
	end

	return group, sub_desktop
end

function get_current_tag()
	return current_tag
end

function get_current_tag_name()
	return get_current_tag().name
end

function get_tag_by_name(name)
	local index = get_current_screen().index
	local tag_obj = tags_table[index][name]
	return tag_obj.body
end

function get_current_group_name()
	local tag_name = get_current_tag_name()
	local group, sub_desktop = destructure_tag_name(tag_name)
	return group
end

function group_last_sub_fullname(group)
	return group_make_desktop_fullname(group.name, group.last_subdesktop)
end

function group_last_tag(group)
	local fullname = group_last_sub_fullname(group)
	return get_tag_by_name(fullname)
end

function group_get_subdesktops_as_tags(group)
	local group_name = group.name
	local ret = {}
	for i, d in pairs(sub_desktop_names) do
		local fullname = group_make_desktop_fullname(group_name, d)
		local tag = get_tag_by_name(fullname)
		ret[i] = tag
	end
	return ret
end

function get_group_by_name(group_name)
	return groups_table[group_name]
end

function group_get_free_tag(group)
	local all_tags = group_get_subdesktops_as_tags(group)
	for i, t in pairs(all_tags) do
		if #t:clients() == 0 then
			return t
		end
	end
	return nil
end

function switch_to_tag(tag)
	tag:view_only()
	adjust_border_sizes()
end

function switch_to_tag_name(tag_name)
	local tag = get_tag_by_name(tag_name)
	switch_to_tag(tag)
end

function switch_to_sub_desktop(i)
	local group_name   = get_current_group_name()
	local desktop_name = tostring(i)
	local fullname     = group_make_desktop_fullname(group_name, desktop_name)
	switch_to_tag_name(fullname)
end

function move_to_sub_desktop(client, i)
	local group_name   = get_current_group_name()
	local desktop_name = tostring(i)
	local fullname     = group_make_desktop_fullname(group_name, desktop_name)
	local tag          = get_tag_by_name(fullname)
	client_move_to_tag(client, tag)
	switch_to_tag(tag)
end

function group_make_desktop_fullname(group_name, desktop_name)
	if desktop_name == "1"
	then return group_name
	else return (group_name .. "/" .. desktop_name)
	end
end

function group_get_last_dekstop_fullname(group)
	local name = group.last_subdesktop
	return group_make_desktop_fullname(group.name, name)
end

function group_name_get_last_dekstop_fullname(name)
	local g = get_group_by_name(name)
	return group_get_last_dekstop_fullname(g)
end

function switch_to_group(name)
	local last_fullname = group_name_get_last_dekstop_fullname(name)
	switch_to_tag_name(last_fullname)
end

function move_to_group_noswitch(client, group_name)
	local last_fullname = group_name_get_last_dekstop_fullname(group_name)
	local tag = get_tag_by_name(last_fullname)
	client_move_to_tag(client, tag)
	return tag
end

function move_to_group(client, group_name)
	local tag = move_to_group_noswitch(client, group_name)
	return switch_to_tag(tag)
end

function reset_theme_defaults()
	theme.font = "monospace 11"

	-- Note: breaks notifications on some awesome versions
	-- theme.bg_normal	 = "#000000"

	theme.bg_focus      = theme.bg_normal
	theme.bg_urgent     = "#FF0000"
	theme.bg_minimize   = "#000055"
	theme.bg_minimize   = "#FFFFFF"
	theme.bg_systray    = theme.bg_normal

	theme.taglist_bg_normal     = theme.bg_normal
	theme.taglist_bg_focus      = theme.bg_normal
	theme.taglist_bg_occupied   = theme.taglist_bg_normal
	theme.taglist_fg_focus      = "#FFFF00"
	theme.taglist_fg_normal     = theme.bg_normal
	theme.taglist_fg_occupied   = "#FFFFFF"

	theme.tasklist_fg_normal = "#007700"
	theme.tasklist_fg_focus  = theme.tasklist_fg_normal

	theme.fg_normal   = "#aaaaaa"
	theme.fg_focus    = "#ffffff"
	theme.fg_urgent   = "#ffffff"
	theme.fg_minimize = "#999999"

	theme.useless_gap   = 0
	theme.border_width  = 3
	theme.border_normal = "#000000"
	theme.border_focus  = "#0000FF"
	theme.border_marked = "#91231c"
end

function reset_theme()
	theme = {}
	reset_theme_defaults()
end

function init_theme()
	beautiful.init(theme)
end

function reinit_theme()
	reset_theme()
	init_theme()
end

-- depends on screen connect
function init_tags_table_on_screen(screen)
	local tags = screen.tags
	local index = screen.index

	tags_table[index] = {}

	for i, t in pairs(tags) do
		local name = t.name
		local group_name, desktop_name = destructure_tag_name(name)
		local group = get_group_by_name(group_name)
		local obj = {
			name	= name,
			body	= t,
			group  = group,
			screen = screen,
			screen_index = screen_index,
		}
		tags_table[index][name] = obj
	end
end

local clientbuttons = awful.util.table.join(
	awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
	awful.button({ modkey }, 1, awful.mouse.client.move),
	awful.button({ modkey }, 3, awful.mouse.client.resize))

function get_temporary_group()
	return get_group_by_name(temp_group_name)
end

function switch_to_last_temporary_tag()
	local temp_group        = get_temporary_group()
	local tag               = group_last_tag(temp_group)
	switch_to_tag(tag)
end

function switch_to_new_temporary_tag()
	local temp_group        = get_temporary_group()
	local free_tag          = group_get_free_tag(temp_group)
	switch_to_tag(free_tag)
end

function move_to_new_temporary_tag(client)
	local temp_group        = get_temporary_group()
	local tag               = group_get_free_tag(temp_group)
	client_move_to_tag(client, tag)
	switch_to_tag(tag)
end

function toggle_keep_on_top(client)
	client.ontop = not client.ontop
end

function kill_client(client)
	client:kill()
end

function kill_awesome()
	awesome.quit()
end

function scroll_layout()
	awful.layout.inc(1)
	adjust_border_sizes()
end

--------------------------
-- Procedural           --
--------------------------

append_table_keys(clientkeys,
	{
		{{"Shift", "w"}, kill_client},
		{"F11", client_toggle_fullscreen},
		{{"Shift", "f"}, toggle_floating_client},
		{"m", minimize_client},
		{{"Shift", "h"}, client_move_left},
		{{"Shift", "l"}, client_move_right},
		{"u", toggle_keep_on_top},
		{{"Shift", "a"}, move_to_new_temporary_tag},
		{{"Shift", "Tab"}, move_to_next_screen},
	}
)

append_global_keys({
	{"t", function () awful.spawn(terminal) end},
	{{"Shift", "t"}, function () switch_to_new_temporary_tag() ; awful.spawn(terminal) end},
	{"x", scroll_layout},
	{"j", function () awful.client.focus.byidx( 1) end},
	{"k", function () awful.client.focus.byidx(-1) end},
	{{"Shift", "j"}, function () awful.client.swap.byidx( 1) end},
	{{"Shift", "k"}, function () awful.client.swap.byidx(-1) end},
	{"h", view_left_tag},
	{"l", view_right_tag},
	{"Tab", focus_next_screen},
	{"]", function () awful.tag.incmwfact( 0.05) end},
	{"[", function () awful.tag.incmwfact(-0.05) end},
	{{"Shift", "m"}, restore_minimized},
	{"a", switch_to_new_temporary_tag},
	{"F2", toggle_panel},
	{{"Shift", "F12"}, kill_awesome},
})

append_sub_desktop_keys()
append_groups_keys()
append_dynamic_keys()

--------------------------
-- Effects              --
--------------------------

reinit_theme()

local awful_clientkeys = key_table_to_awful_key_table(clientkeys)

-- All clients will match this rule.
awful.rules.rules = {
	{ rule = { },
		properties = {
			titlebars_enabled = false,
			border_width	  = 2,
			border_color	  = beautiful.border_normal,
			size_hints_honor  = false, -- fixes empty space at bottom of the screen
			keys			  = awful_clientkeys,
			buttons			= clientbuttons,
			screen			= awful.screen.preferred,
			placement		 = awful.placement.no_overlap + awful.placement.no_offscreen,
		},
	},
}

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
	awful.layout.suit.tile,
	awful.layout.suit.max,
}

awful.screen.connect_for_each_screen(get_foreach_screen_fn())

root.keys(key_table_to_awful_key_table(globalkeys));

client.connect_signal("manage", on_manage_hook)
client.connect_signal("unmanage", on_unmanage_hook)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", on_mouse_enter_hook)

client.connect_signal("property::geometry", on_geometry_hook)

client.connect_signal("focus", on_focus_hook)
client.connect_signal("unfocus", on_unfocus_hook)

