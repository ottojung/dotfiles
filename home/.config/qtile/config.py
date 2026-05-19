# ~/.config/qtile/config.py
#
# A compact Qtile translation of your XMonad config.
#
# Important behavior:
#   - application-requested fullscreen is disabled: auto_fullscreen = False
#   - group names are always main/letter, e.g. 1/a, 2/t
#   - screen numbers are only display decoration in the bar, never group names

from html import escape
from string import ascii_lowercase
from typing import Any, cast

from libqtile import bar, hook, layout, qtile, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy


mod = "mod4"
terminal = "xterm"

COLORS = {
    # Flat xmobar-like bar: mostly text on one background, not widget boxes.
    "bg": "#10121a",
    "fg": "#d8dee9",
    "title": "#c0caf5",
    "muted": "#7f849c",
    "subtle": "#4c566a",
    "dim": "#5f667a",
    "accent": "#8bd5ff",        # focused workspace
    "visible": "#d8dee9",       # workspace visible on another monitor
    "active_hidden": "#a6e3a1", # hidden workspace with windows
    "clock": "#f9e2af",
    "warning": "#f38ba8",
    "border_focus": "#f38ba8",
    "border_normal": "#585b70",
}

MAIN_GROUPS = range(1, 10)
SECONDARIES = tuple(ascii_lowercase)
DEFAULT_MAIN = 1
DEFAULT_SECONDARY = "t"

# XMonad remembered the last letter workspace visited within each numeric
# workspace. This is the small Qtile equivalent. It is intentionally not
# persisted across restarts.
_last_secondary_by_main: dict[int, str] = {main: DEFAULT_SECONDARY for main in MAIN_GROUPS}

# Global focus history for Mod-Tab.
_focus_history: list[Any] = []
_MAX_FOCUS_HISTORY = 64


def group_name(main: int, secondary: str) -> str:
    # The only separator used in actual Qtile group names.
    return f"{main}/{secondary}"


def parse_group_name(name: str) -> tuple[int, str]:
    # Be forgiving when reloading from older experimental names, but never
    # produce those old names again.
    normalized = name.replace(":", "/")

    parts = normalized.split("/")
    if len(parts) >= 2:
        try:
            return int(parts[0]), parts[1][0]
        except (ValueError, IndexError):
            pass

    if len(normalized) == 1 and normalized in SECONDARIES:
        return DEFAULT_MAIN, normalized

    return DEFAULT_MAIN, DEFAULT_SECONDARY


def current_parts(qtile_obj: Any) -> tuple[int, str]:
    group = getattr(qtile_obj, "current_group", None)
    if group is None:
        return DEFAULT_MAIN, DEFAULT_SECONDARY
    return parse_group_name(group.name)


def switch_to_group(qtile_obj: Any, name: str) -> None:
    group = qtile_obj.groups_map.get(name)
    if group is not None:
        qtile_obj.current_screen.set_group(group)


def move_window_to_group(qtile_obj: Any, name: str, switch: bool = True) -> None:
    if qtile_obj.current_window is not None:
        qtile_obj.current_window.togroup(name)
    if switch:
        switch_to_group(qtile_obj, name)


def go_to_main(qtile_obj: Any, main: int, move_window: bool = False) -> None:
    secondary = _last_secondary_by_main.get(main, DEFAULT_SECONDARY)
    target = group_name(main, secondary)
    if move_window:
        move_window_to_group(qtile_obj, target, switch=True)
    else:
        switch_to_group(qtile_obj, target)


def go_to_secondary(qtile_obj: Any, secondary: str, move_window: bool = False) -> None:
    main, _ = current_parts(qtile_obj)
    if main not in MAIN_GROUPS:
        main = DEFAULT_MAIN
    target = group_name(main, secondary)
    if move_window:
        move_window_to_group(qtile_obj, target, switch=True)
    else:
        switch_to_group(qtile_obj, target)


def go_to_secondary_delta(qtile_obj: Any, delta: int, move_window: bool = False) -> None:
    main, secondary = current_parts(qtile_obj)
    if main not in MAIN_GROUPS:
        main = DEFAULT_MAIN
    index = SECONDARIES.index(secondary) if secondary in SECONDARIES else 0
    target_secondary = SECONDARIES[(index + delta) % len(SECONDARIES)]
    target = group_name(main, target_secondary)
    if move_window:
        move_window_to_group(qtile_obj, target, switch=True)
    else:
        switch_to_group(qtile_obj, target)


def remember_current_subworkspace() -> None:
    qtile_any = cast(Any, qtile)
    main, secondary = current_parts(qtile_any)
    if main in _last_secondary_by_main:
        _last_secondary_by_main[main] = secondary


def remember_focused_window(window: Any) -> None:
    global _focus_history

    if window is None:
        return

    alive = []
    for old_window in _focus_history:
        if old_window is window:
            continue
        if getattr(old_window, "group", None) is not None:
            alive.append(old_window)

    _focus_history = [window] + alive[: _MAX_FOCUS_HISTORY - 1]


def assign_starting_workspaces() -> None:
    """Start monitors on 1/a, 1/b, ..., 1/$n.

    This is only about which groups are initially visible. Group names remain
    plain main/letter names and never receive screen prefixes.
    """
    qtile_any = cast(Any, qtile)

    for index, screen in enumerate(getattr(qtile_any, "screens", [])):
        if index >= len(SECONDARIES):
            break
        target = qtile_any.groups_map.get(group_name(DEFAULT_MAIN, SECONDARIES[index]))
        if target is not None:
            screen.set_group(target)


def swap_screen_groups(screen_a: Any, screen_b: Any) -> None:
    group_a = getattr(screen_a, "group", None)
    group_b = getattr(screen_b, "group", None)

    if group_a is None or group_b is None or group_a is group_b:
        return

    # In practice this gives the desired XMonad-like result: the target window's
    # workspace appears on the monitor you were using, and the previous content
    # of that monitor moves to the other display.
    screen_a.set_group(group_b)
    screen_b.set_group(group_a)


def focus_previous_window(qtile_obj: Any) -> None:
    """Focus the previously focused live window.

    If the target window is visible on another physical display, swap that
    display's group with the currently focused display first. This mirrors the
    useful part of the old XMonad focusLastWindow behavior: the target appears
    on the monitor you were already looking at.
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

    current_screen = getattr(qtile_obj, "current_screen", None)
    target_screen = getattr(target_group, "screen", None)

    if current_screen is not None and target_screen is not None and target_screen is not current_screen:
        swap_screen_groups(current_screen, target_screen)
    elif current_screen is not None and target_screen is None:
        current_screen.set_group(target_group)

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


def pango_span(text: str, foreground: str, background: str | None = None, weight: str | None = None) -> str:
    attrs = [f'foreground="{escape(foreground)}"']
    if background is not None:
        attrs.append(f'background="{escape(background)}"')
    if weight is not None:
        attrs.append(f'weight="{escape(weight)}"')
    return f'<span {" ".join(attrs)}>{escape(text)}</span>'


def status_group_name(name: str) -> str:
    """Display-only group name.

    Actual Qtile groups stay named 1/a, 2/a, etc. In the bar, 1/a is shown as
    just a, while 2/a and above keep their numeric prefix.
    """
    main, secondary = parse_group_name(name)
    if main == DEFAULT_MAIN:
        return secondary
    return group_name(main, secondary)


def screen_status_text() -> str:
    """Coloured screen/workspace status.

    Deliberately flat and xmobar-like: brackets/parens carry meaning, colour
    carries state, and no token gets a coloured background box.

      [a]    focused workspace 1/a
      (b)    visible workspace 1/b on another monitor, with windows
       c     visible but empty workspace 1/c
      2/t    hidden workspace 2/t with windows
    """
    try:
        qtile_any = cast(Any, qtile)
        current_group = getattr(qtile_any, "current_group", None)

        visible_group_names: set[str] = set()
        for screen in getattr(qtile_any, "screens", []):
            group = getattr(screen, "group", None)
            if group is not None:
                visible_group_names.add(group.name)

        parts = []
        for group in getattr(qtile_any, "groups", []):
            windows = getattr(group, "windows", []) or []
            has_windows = bool(windows)
            is_visible = group.name in visible_group_names

            if not is_visible and not has_windows:
                continue

            name = status_group_name(group.name)
            is_current = current_group is not None and group.name == current_group.name

            if is_current:
                parts.append(
                    pango_span("[", COLORS["subtle"])
                    + pango_span(name, COLORS["accent"], weight="bold")
                    + pango_span("]", COLORS["subtle"])
                )
            elif is_visible and has_windows:
                parts.append(
                    pango_span("(", COLORS["subtle"])
                    + pango_span(name, COLORS["visible"], weight="bold")
                    + pango_span(")", COLORS["subtle"])
                )
            elif is_visible:
                parts.append(pango_span(name, COLORS["dim"]))
            else:
                parts.append(pango_span(name, COLORS["active_hidden"]))

        return pango_span("  ", COLORS["subtle"]).join(parts)
    except Exception:
        return ""


class ScreenStatus(widget.GenPollText):
    """Custom coloured widget for visible and active workspaces."""

    def __init__(self, **config: Any) -> None:
        super().__init__(
            func=screen_status_text,
            update_interval=0.25,
            markup=True,
            **config,
        )


# Register hooks imperatively. This avoids qtile check/stub issues that can
# show up with decorated lazy functions in config files.
hook.subscribe.setgroup(remember_current_subworkspace)
hook.subscribe.client_focus(remember_focused_window)
hook.subscribe.startup_once(assign_starting_workspaces)


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
    Key([mod], "Tab", lazy.function(focus_previous_window), desc="Focus previous window/screen"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Swap down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Swap up"),

    # Size changes.
    Key([mod], "bracketleft", lazy.layout.shrink(), desc="Shrink focused pane / vertical size"),
    Key([mod], "bracketright", lazy.layout.grow(), desc="Grow focused pane / vertical size"),
    Key([mod, "control", "shift"], "6", lazy.layout.grow(), desc="Grow focused pane / vertical size"),

    Key([mod, "shift"], "bracketleft", lazy.layout.shrink_main(), desc="Decrease horizontal/master size"),
    Key([mod, "shift"], "bracketright", lazy.layout.grow_main(), desc="Increase horizontal/master size"),
    Key([mod, "shift"], "braceleft", lazy.layout.shrink_main(), desc="Decrease horizontal/master size fallback"),
    Key([mod, "shift"], "braceright", lazy.layout.grow_main(), desc="Increase horizontal/master size fallback"),

    # Layout / window state.
    Key([mod], "x", lazy.next_layout(), desc="Next layout"),
    Key([mod, "shift"], "w", lazy.window.kill(), desc="Kill focused window"),
    Key([mod], "m", lazy.window.toggle_minimize(), desc="Minimize/unminimize window"),
    Key([mod], "f", lazy.window.toggle_floating(), desc="Toggle floating"),
    Key([mod, "shift"], "f", lazy.window.toggle_floating(), desc="Toggle floating"),
    Key([mod], "F11", lazy.window.toggle_fullscreen(), desc="Explicitly fullscreen focused window"),

    # XMonad's grouped workspace movement.
    Key([mod], "h", lazy.function(go_to_secondary_delta, -1), desc="Previous letter workspace in current main group"),
    Key([mod], "l", lazy.function(go_to_secondary_delta, 1), desc="Next letter workspace in current main group"),
    Key([mod, "shift"], "h", lazy.function(go_to_secondary_delta, -1, True), desc="Move window to previous letter workspace"),
    Key([mod, "shift"], "l", lazy.function(go_to_secondary_delta, 1, True), desc="Move window to next letter workspace"),

    # Temporary/default desktop: the XMonad config's default secondary was 't'.
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

# Your XMonad config delegated many Mod-* bindings to shell commands named
# awesomewm-key-* / awesomewm-key-shift-*.
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
    font="DejaVu Sans Mono",
    fontsize=17,
    padding=6,
    foreground=COLORS["fg"],
    background=COLORS["bg"],
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.TextBox(
                    " qtile ",
                    foreground=COLORS["subtle"],
                    padding=8,
                ),
                ScreenStatus(
                    fontsize=18,
                    foreground=COLORS["fg"],
                    background=COLORS["bg"],
                    padding=8,
                ),
                widget.TextBox(" │ ", foreground=COLORS["subtle"], padding=2),
                widget.WindowName(
                    fontsize=16,
                    foreground=COLORS["title"],
                    background=COLORS["bg"],
                    empty_group_string="",
                    padding=8,
                ),
                widget.Systray(background=COLORS["bg"], padding=8),
                widget.TextBox(" │ ", foreground=COLORS["subtle"], padding=2),
                widget.Clock(
                    format="%a %b %-d  %H:%M",
                    fontsize=17,
                    foreground=COLORS["clock"],
                    background=COLORS["bg"],
                    padding=10,
                ),
            ],
            36,
            background=COLORS["bg"],
            margin=[4, 8, 0, 8],
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
# browser/app fullscreen requests do not automatically take the whole monitor.
auto_fullscreen = False

follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
focus_on_window_activation = "smart"
reconfigure_screens = True
auto_minimize = True
wmname = "LG3D"
