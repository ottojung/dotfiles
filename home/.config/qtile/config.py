# ~/.config/qtile/config.py
#
# A small Qtile translation of:
# https://github.com/ottojung/dotfiles/blob/master/home/.xmonad/xmonad.hs
#
# The important bit for your Chrome/F11 behavior is:
#     auto_fullscreen = False

from html import escape
from string import ascii_lowercase
from typing import Any, cast

from libqtile import bar, hook, layout, qtile, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy


mod = "mod4"
terminal = "xterm"

# Small, readable palette. Adjust these freely if you want a different vibe.
COLORS = {
    "bg": "#1e1e2e",
    "bg_alt": "#313244",
    "fg": "#cdd6f4",
    "muted": "#a6adc8",
    "accent": "#89b4fa",
    "accent_2": "#a6e3a1",
    "warning": "#f38ba8",
    "border_focus": "#f38ba8",
    "border_normal": "#585b70",
    "gap": "#11111b",
}

MAIN_GROUPS = range(1, 10)
SECONDARIES = tuple(ascii_lowercase)
DEFAULT_MAIN = 1
DEFAULT_SECONDARY = "t"

# XMonad remembered the last letter workspace visited within each numeric
# workspace. This is the small Qtile equivalent. It is intentionally not
# persisted across restarts.
_last_secondary_by_main = {main: DEFAULT_SECONDARY for main in MAIN_GROUPS}

# Global focus history for Mod-Tab. Qtile has its own internal focus handling,
# but this gives us something close to the XMonad GroupNavigation behavior.
_focus_history: list[Any] = []
_MAX_FOCUS_HISTORY = 64


def group_name(main: int, secondary: str) -> str:
    # Match the XMonad naming: "a".."z" for main 1, "2/a".."9/z" otherwise.
    if main == DEFAULT_MAIN:
        return secondary
    return f"{main}/{secondary}"


def parse_group_name(name: str) -> tuple[int, str]:
    if "/" in name:
        left, right = name.split("/", 1)
        return int(left), right[0]
    if len(name) == 1 and name in SECONDARIES:
        return DEFAULT_MAIN, name
    # Fallback for ad-hoc groups.
    return DEFAULT_MAIN, DEFAULT_SECONDARY


def current_parts(qtile_obj) -> tuple[int, str]:
    try:
        return parse_group_name(qtile_obj.current_group.name)
    except Exception:
        return DEFAULT_MAIN, DEFAULT_SECONDARY


def switch_to_group(qtile_obj, name: str) -> None:
    group = qtile_obj.groups_map.get(name)
    if group is not None:
        qtile_obj.current_screen.set_group(group)


def move_window_to_group(qtile_obj, name: str, switch: bool = True) -> None:
    if qtile_obj.current_window is not None:
        qtile_obj.current_window.togroup(name)
    if switch:
        switch_to_group(qtile_obj, name)


def remember_current_subworkspace() -> None:
    main, secondary = current_parts(qtile)
    if main in _last_secondary_by_main:
        _last_secondary_by_main[main] = secondary


# Register imperatively rather than with @hook.subscribe.setgroup.
# This avoids a qtile-check/mypy stubgen issue where decorated hook functions
# can be emitted as `FunctionType` without importing it in config.pyi.
hook.subscribe.setgroup(remember_current_subworkspace)


def remember_focused_window(window: Any) -> None:
    global _focus_history

    if window is None:
        return

    # Keep most-recent-first and remove stale/dead windows opportunistically.
    alive = []
    for old_window in _focus_history:
        if old_window is window:
            continue
        if getattr(old_window, "group", None) is not None:
            alive.append(old_window)

    _focus_history = [window] + alive[: _MAX_FOCUS_HISTORY - 1]


# Register imperatively for the same reason as setgroup above.
hook.subscribe.client_focus(remember_focused_window)


def pango_span(text: str, fg: str, bg: str | None = None, weight: str | None = None) -> str:
    attrs = [f'foreground="{fg}"']
    if bg is not None:
        attrs.append(f'background="{bg}"')
    if weight is not None:
        attrs.append(f'weight="{weight}"')
    return f"<span {' '.join(attrs)}> {escape(text)} </span>"


class ScreenStatus(widget.GenPollText):
    """Compact, coloured screen/workspace status.

    State colours:
      [0:t]    focused monitor/workspace
      (1:2/a)  visible workspace on another monitor, with windows
       1:2/a   visible workspace on another monitor, currently empty/muted
       3/b     hidden workspace that contains windows
    """

    def __init__(self, **config: Any) -> None:
        super().__init__(func=self.poll_status, update_interval=0.25, markup=True, **config)

    def poll_status(self) -> str:
        try:
            qtile_any = getattr(self, "qtile", None) or cast(Any, qtile)
            current_group = getattr(qtile_any, "current_group", None)

            visible_group_to_screen = {}
            for index, screen in enumerate(getattr(qtile_any, "screens", [])):
                group = getattr(screen, "group", None)
                if group is not None:
                    visible_group_to_screen[group.name] = index

            parts = []
            for group in getattr(qtile_any, "groups", []):
                windows = getattr(group, "windows", []) or []
                screen_index = visible_group_to_screen.get(group.name)
                is_visible = screen_index is not None
                has_windows = bool(windows)

                # Always show monitor workspaces, even if empty, so that the
                # bar tells you which physical display is focused. Also show
                # hidden workspaces if they contain windows.
                if not is_visible and not has_windows:
                    continue

                if current_group is not None and group.name == current_group.name:
                    parts.append(
                        pango_span(
                            f"[{screen_index}:{group.name}]",
                            COLORS["bg"],
                            COLORS["accent"],
                            "bold",
                        )
                    )
                elif is_visible and has_windows:
                    parts.append(
                        pango_span(
                            f"({screen_index}:{group.name})",
                            COLORS["bg"],
                            COLORS["accent_2"],
                            "bold",
                        )
                    )
                elif is_visible:
                    parts.append(
                        pango_span(
                            f" {screen_index}:{group.name} ",
                            COLORS["muted"],
                            COLORS["bg"],
                        )
                    )
                else:
                    parts.append(pango_span(group.name, COLORS["muted"], COLORS["bg_alt"]))

            return "  ".join(parts)
        except Exception:
            return ""


def focus_previous_window(qtile_obj: Any) -> None:
    """Focus the previously focused live window.

    If the target window is visible on another monitor, swap that monitor's
    workspace with the currently focused monitor's workspace first. This keeps
    the useful XMonad behavior: Mod-Tab brings the target window to the display
    you are already looking at, rather than moving your keyboard focus away to a
    different physical display.
    """
    current = qtile_obj.current_window
    target = None
    cleaned = []

    for window in _focus_history:
        group = getattr(window, "group", None)
        if group is None:
            continue
        cleaned.append(window)
        if window is not current and target is None:
            target = window

    _focus_history[:] = cleaned[:_MAX_FOCUS_HISTORY]

    if target is None:
        return

    target_group = getattr(target, "group", None)
    if target_group is None:
        return

    current_screen = qtile_obj.current_screen
    current_group = getattr(current_screen, "group", None)
    target_screen = getattr(target_group, "screen", None)

    if target_screen is not None and target_screen is not current_screen:
        # Swap the two visible workspaces. Do the other screen first so the
        # target group is free to land on the current screen.
        if current_group is not None:
            target_screen.set_group(current_group)
        current_screen.set_group(target_group)
    elif target_screen is None:
        # Hidden target workspace: pull it onto the current monitor.
        current_screen.set_group(target_group)

    # Keep keyboard focus on the physical monitor that initiated Mod-Tab.
    try:
        qtile_obj.focus_screen(qtile_obj.screens.index(current_screen))
    except Exception:
        pass

    try:
        target_group.focus(target, warp=False)
    except TypeError:
        target_group.focus(target)

    try:
        target.focus(warp=False)
    except TypeError:
        target.focus(False)
    except Exception:
        pass


def go_to_main(qtile_obj, main: int, move_window: bool = False) -> None:
    secondary = _last_secondary_by_main.get(main, DEFAULT_SECONDARY)
    target = group_name(main, secondary)
    if move_window:
        move_window_to_group(qtile_obj, target, switch=True)
    else:
        switch_to_group(qtile_obj, target)


def go_to_secondary(qtile_obj, secondary: str, move_window: bool = False) -> None:
    main, _ = current_parts(qtile_obj)
    target = group_name(main, secondary)
    if move_window:
        move_window_to_group(qtile_obj, target, switch=True)
    else:
        switch_to_group(qtile_obj, target)


def go_to_secondary_delta(qtile_obj, delta: int, move_window: bool = False) -> None:
    main, secondary = current_parts(qtile_obj)
    index = SECONDARIES.index(secondary) if secondary in SECONDARIES else 0
    target_secondary = SECONDARIES[(index + delta) % len(SECONDARIES)]
    target = group_name(main, target_secondary)
    if move_window:
        move_window_to_group(qtile_obj, target, switch=True)
    else:
        switch_to_group(qtile_obj, target)


groups = [
    Group(group_name(main, secondary))
    for main in MAIN_GROUPS
    for secondary in SECONDARIES
]


keys = [
    # Panel / qtile control.
    Key([mod], "F2", lazy.hide_show_bar("top"), desc="Toggle top bar"),
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload Qtile"),
    Key([mod, "shift"], "F12", lazy.shutdown(), desc="Quit Qtile"),

    # Window focus and swapping.
    Key([mod], "j", lazy.layout.down(), desc="Focus next window"),
    Key([mod], "k", lazy.layout.up(), desc="Focus previous window"),
    Key([mod], "Tab", lazy.function(focus_previous_window), desc="Focus previously focused window/screen"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Swap down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Swap up"),

    # Tall-layout resizing. This is close to XMonad Shrink/Expand.
    Key([mod], "bracketleft", lazy.layout.shrink(), desc="Shrink focused pane / vertical size"),
    Key([mod], "bracketright", lazy.layout.grow(), desc="Grow focused pane / vertical size"),
    Key([mod, "shift"], "asciicircum", lazy.layout.grow(), desc="Grow focused pane / vertical size"),
    Key([mod, "shift"], "6", lazy.layout.grow(), desc="Grow focused pane / vertical size fallback"),

    # Horizontal/master ratio. The brace variants cover keyboards where
    # Shift+[ / Shift+] produce braceleft/braceright keysyms.
    Key([mod, "shift"], "bracketleft", lazy.layout.shrink_main(), desc="Decrease horizontal/master size"),
    Key([mod, "shift"], "bracketright", lazy.layout.grow_main(), desc="Increase horizontal/master size"),
    Key([mod, "shift"], "braceleft", lazy.layout.shrink_main(), desc="Decrease horizontal/master size fallback"),
    Key([mod, "shift"], "braceright", lazy.layout.grow_main(), desc="Increase horizontal/master size fallback"),
    Key([mod], "n", lazy.layout.normalize(), desc="Normalize layout"),

    # Layout / window state.
    Key([mod], "x", lazy.next_layout(), desc="Next layout"),
    Key([mod, "shift"], "w", lazy.window.kill(), desc="Kill focused window"),
    Key([mod], "m", lazy.window.toggle_minimize(), desc="Minimize/unminimize window"),
    Key([mod], "f", lazy.window.toggle_floating(), desc="Toggle floating"),
    Key([mod, "shift"], "f", lazy.window.toggle_floating(), desc="Toggle floating"),

    # Fullscreen is manual only because auto_fullscreen=False below.
    Key([mod], "F11", lazy.window.toggle_fullscreen(), desc="Explicitly fullscreen focused window"),

    # XMonad's grouped workspace movement.
    Key([mod], "h", lazy.function(go_to_secondary_delta, -1), desc="Previous letter workspace in current main group"),
    Key([mod], "l", lazy.function(go_to_secondary_delta, 1), desc="Next letter workspace in current main group"),
    Key([mod, "shift"], "h", lazy.function(go_to_secondary_delta, -1, True), desc="Move window to previous letter workspace"),
    Key([mod, "shift"], "l", lazy.function(go_to_secondary_delta, 1, True), desc="Move window to next letter workspace"),

    # Temporary/default desktop: the XMonad config's default secondary is 't'.
    Key([mod], "a", lazy.function(go_to_secondary, DEFAULT_SECONDARY), desc="Go to temporary/default letter workspace"),
    Key([mod, "shift"], "a", lazy.function(go_to_secondary, DEFAULT_SECONDARY, True), desc="Move window to temporary/default workspace"),
]

# Numeric main workspaces: Mod-1..9. Preserve last visited letter per number.
for main in MAIN_GROUPS:
    keys.extend(
        [
            Key([mod], str(main), lazy.function(go_to_main, main), desc=f"Go to main workspace {main}"),
            Key([mod, "shift"], str(main), lazy.function(go_to_main, main, True), desc=f"Move window to main workspace {main}"),
        ]
    )

# Letter sub-workspaces: Mod-Control-a..z.
for secondary in SECONDARIES:
    keys.extend(
        [
            Key([mod, "control"], secondary, lazy.function(go_to_secondary, secondary), desc=f"Go to {secondary} in current main workspace"),
            Key(
                [mod, "control", "shift"],
                secondary,
                lazy.function(go_to_secondary, secondary, True),
                desc=f"Move window to {secondary} in current main workspace",
            ),
        ]
    )

# Your XMonad config delegates many Mod-* bindings to shell commands named
# awesomewm-key-* / awesomewm-key-shift-*.
#
# Keep this enabled if those scripts are still part of your workflow. Keys
# explicitly used above are skipped so Qtile does not bind the same chord twice.
ENABLE_AWESOMEWM_KEY_SCRIPTS = True
QTILE_USED_LETTER_KEYS = {"a", "f", "h", "j", "k", "l", "m", "n", "w", "x"}
QTILE_USED_SPECIAL_KEYS = {"Tab"}
if ENABLE_AWESOMEWM_KEY_SCRIPTS:
    for letter in SECONDARIES:
        if letter not in QTILE_USED_LETTER_KEYS:
            keys.append(Key([mod], letter, lazy.spawn(f"awesomewm-key-{letter}")))
            keys.append(Key([mod, "shift"], letter, lazy.spawn(f"awesomewm-key-shift-{letter}")))

    for key_name, script_name in [
        ("BackSpace", "backspace"),
        ("Tab", "tab"),
        ("Return", "return"),
        ("Print", "print"),
        ("Escape", "escape"),
        ("Delete", "delete"),
        ("Home", "home"),
        ("Left", "left"),
        ("Up", "up"),
        ("Right", "right"),
        ("Down", "down"),
        ("Page_Up", "page_up"),
        ("Page_Down", "page_down"),
        ("End", "end"),
        ("Insert", "insert"),
        ("space", "space"),
    ]:
        if key_name not in QTILE_USED_SPECIAL_KEYS:
            keys.append(Key([mod], key_name, lazy.spawn(f"awesomewm-key-{script_name}")))
            keys.append(Key([mod, "shift"], key_name, lazy.spawn(f"awesomewm-key-shift-{script_name}")))


layouts = [
    # Full ||| Tall equivalent. Max is not automatic fullscreen; it is just
    # "one tiled window fills the available Qtile screen rectangle".
    layout.Max(
        border_width=0,
        margin=8,
    ),
    layout.MonadTall(
        ratio=0.5,
        change_ratio=0.03,
        border_width=3,
        margin=8,
        border_focus=COLORS["border_focus"],
        border_normal=COLORS["border_normal"],
    ),
]


widget_defaults = dict(
    font="DejaVu Sans",
    fontsize=14,
    padding=8,
    foreground=COLORS["fg"],
    background=COLORS["bg"],
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.TextBox(
                    " 󰍹 ",
                    fontsize=18,
                    foreground=COLORS["bg"],
                    background=COLORS["accent"],
                    padding=10,
                ),
                ScreenStatus(
                    foreground=COLORS["fg"],
                    background=COLORS["bg"],
                    padding=6,
                ),
                widget.Sep(linewidth=0, padding=6, background=COLORS["bg"]),
                widget.WindowName(
                    foreground=COLORS["fg"],
                    background=COLORS["bg"],
                    empty_group_string="",
                    padding=10,
                ),
                widget.Systray(background=COLORS["bg"], padding=8),
                widget.Clock(
                    format="%Y-%m-%d  %a  %H:%M",
                    foreground=COLORS["bg"],
                    background=COLORS["accent"],
                    padding=10,
                ),
            ],
            32,
            background=COLORS["bg"],
            margin=[6, 8, 0, 8],
        )
    ),
]


floating_layout = layout.Floating(
    border_width=3,
    border_focus=COLORS["border_focus"],
    border_normal=COLORS["border_normal"],
    float_rules=[
        *layout.Floating.default_float_rules,
        Match(wm_class="file-roller"),
        Match(wm_class="nitrogen"),
        Match(wm_class="ncmpcpp"),
        Match(wm_class="weechat"),
        Match(wm_class="mplayer"),
        Match(wm_class="cmus"),
    ],
)

mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]


# This is the key setting for the F11 behavior:
#
# When Chrome/browser asks for fullscreen, Qtile will not automatically enlarge
# the client to the whole monitor. The browser may still hide its own UI, but
# the Qtile-managed window keeps its current tiled geometry.
auto_fullscreen = False

# Close to your xmonad config / Qtile defaults.
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
focus_on_window_activation = "smart"
reconfigure_screens = True
auto_minimize = True
wmname = "LG3D"
