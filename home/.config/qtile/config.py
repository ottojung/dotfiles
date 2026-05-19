# ~/.config/qtile/config.py
#
# A compact Qtile translation of your XMonad config, extended with a global
# major-workspace invariant:
#
#   If any display is on 3/k, all displays must be on 3/<something>.
#
# Important behavior:
#   - application-requested fullscreen is disabled: auto_fullscreen = False
#   - group names are always main/letter, e.g. 1/a, 2/t
#   - the bar displays 1/a as just a, but keeps 2/a, 3/a, etc.
#   - major workspace switches move all displays together
#   - each major workspace remembers its own per-display minor layout

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

    # Status-bar hierarchy. Active things are saturated; inactive things are
    # intentionally grey and low-contrast.
    "focused": "#ffcc66",        # current workspace
    "active_visible": "#a6e3a1", # visible on another display and has windows
    "active_hidden": "#7dcfff",  # hidden but has windows
    "inactive": "#545c73",       # visible but empty
    "subtle": "#3f4658",         # brackets, separators, label text
    "muted": "#7f849c",

    "clock": "#f9e2af",
    "warning": "#f38ba8",
    "border_focus": "#f38ba8",
    "border_normal": "#585b70",
}

MAIN_GROUPS = range(1, 10)
SECONDARIES = tuple(ascii_lowercase)
DEFAULT_MAIN = 1
DEFAULT_SECONDARY = "t"

# Richer replacement for the old XMonad-style "last secondary per main" idea.
#
# For each major workspace, remember which minor/secondary letter each physical
# display last showed. Example:
#   1 -> {0: "a", 1: "b", 2: "t"}
#   2 -> {0: "k", 1: "m", 2: "l"}
_minor_config_by_major: dict[int, dict[int, str]] = {main: {} for main in MAIN_GROUPS}

# Global focus history for Mod-Tab.
_focus_history: list[Any] = []
_MAX_FOCUS_HISTORY = 64

# Per-workspace bar visibility. True means: hide the bar whenever this group is
# visible on a screen. This makes Mod-F2 behave like a per-workspace layout bit.
_bar_hidden_by_group: dict[str, bool] = {}

# Last-minimized-first history, so Mod-m can only minimize and Mod-Shift-m can
# restore the most recent minimized window on the current workspace.
_minimized_history: list[Any] = []


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


def current_screen_index(qtile_obj: Any) -> int:
    current_screen = getattr(qtile_obj, "current_screen", None)
    screens = list(getattr(qtile_obj, "screens", []))
    if current_screen in screens:
        return screens.index(current_screen)
    return 0


def fallback_secondary_for_screen(index: int) -> str:
    if 0 <= index < len(SECONDARIES):
        return SECONDARIES[index]
    return DEFAULT_SECONDARY


def first_unused_secondary(used: set[str]) -> str:
    for secondary in SECONDARIES:
        if secondary not in used:
            return secondary
    return DEFAULT_SECONDARY


def normalize_minor_config(main: int, screen_count: int, overrides: dict[int, str] | None = None) -> list[str]:
    """Return a unique secondary letter for each screen for a major group."""
    saved = dict(_minor_config_by_major.get(main, {}))
    if overrides:
        saved.update(overrides)

    letters: list[str] = []
    used: set[str] = set()

    for index in range(screen_count):
        secondary = saved.get(index, fallback_secondary_for_screen(index))
        if secondary not in SECONDARIES or secondary in used:
            secondary = first_unused_secondary(used)
        letters.append(secondary)
        used.add(secondary)

    _minor_config_by_major[main] = {index: secondary for index, secondary in enumerate(letters)}
    return letters


def save_visible_minor_config(qtile_obj: Any) -> None:
    """Remember the visible screen -> secondary layout for every visible major."""
    for index, screen in enumerate(getattr(qtile_obj, "screens", [])):
        group = getattr(screen, "group", None)
        if group is None:
            continue

        main, secondary = parse_group_name(group.name)
        if main not in MAIN_GROUPS or secondary not in SECONDARIES:
            continue

        _minor_config_by_major.setdefault(main, {})[index] = secondary


def visible_major(qtile_obj: Any) -> int:
    main, _ = current_parts(qtile_obj)
    if main in MAIN_GROUPS:
        return main
    return DEFAULT_MAIN


def set_screen_to_group(screen: Any, group: Any) -> None:
    if group is not None:
        screen.set_group(group)


def switch_major_on_all_screens(
    qtile_obj: Any,
    main: int,
    preferred_screen_index: int | None = None,
    preferred_secondary: str | None = None,
) -> None:
    """Switch every display to the requested major workspace.

    The target major's last per-screen minor layout is restored. If a preferred
    screen/secondary is supplied, that screen is forced to that secondary; this
    is used by Mod-Tab so the target window appears on the currently focused
    physical display.
    """
    if main not in MAIN_GROUPS:
        return

    screens = list(getattr(qtile_obj, "screens", []))
    overrides: dict[int, str] = {}
    if preferred_screen_index is not None and preferred_secondary in SECONDARIES:
        overrides[preferred_screen_index] = cast(str, preferred_secondary)

    letters = normalize_minor_config(main, len(screens), overrides)

    for index, screen in enumerate(screens):
        target = qtile_obj.groups_map.get(group_name(main, letters[index]))
        set_screen_to_group(screen, target)


def switch_to_group(qtile_obj: Any, name: str) -> None:
    """Compatibility helper: switch current screen to a group if it exists."""
    group = qtile_obj.groups_map.get(name)
    if group is not None:
        qtile_obj.current_screen.set_group(group)


def move_window_to_group(qtile_obj: Any, name: str, switch: bool = True) -> None:
    if qtile_obj.current_window is not None:
        qtile_obj.current_window.togroup(name)
    if switch:
        switch_to_group(qtile_obj, name)


def swap_screen_groups(screen_a: Any, screen_b: Any) -> None:
    group_a = getattr(screen_a, "group", None)
    group_b = getattr(screen_b, "group", None)

    if group_a is None or group_b is None or group_a is group_b:
        return

    screen_a.set_group(group_b)
    screen_b.set_group(group_a)


def go_to_main(qtile_obj: Any, main: int, move_window: bool = False) -> None:
    """Switch all displays to a major group, restoring its minor layout."""
    if main not in MAIN_GROUPS:
        return

    save_visible_minor_config(qtile_obj)

    screen_index = current_screen_index(qtile_obj)
    screens = list(getattr(qtile_obj, "screens", []))
    target_letters = normalize_minor_config(main, len(screens))
    target_secondary = target_letters[screen_index] if screen_index < len(target_letters) else DEFAULT_SECONDARY
    target_name = group_name(main, target_secondary)

    if move_window and qtile_obj.current_window is not None:
        qtile_obj.current_window.togroup(target_name)

    switch_major_on_all_screens(qtile_obj, main)


def go_to_secondary(qtile_obj: Any, secondary: str, move_window: bool = False) -> None:
    """Switch only the current display's minor letter, keeping the major global."""
    if secondary not in SECONDARIES:
        return

    save_visible_minor_config(qtile_obj)

    main = visible_major(qtile_obj)
    target_name = group_name(main, secondary)
    target_group = qtile_obj.groups_map.get(target_name)
    current_screen = getattr(qtile_obj, "current_screen", None)
    target_screen = getattr(target_group, "screen", None)

    if target_group is None or current_screen is None:
        return

    if move_window and qtile_obj.current_window is not None:
        qtile_obj.current_window.togroup(target_name)

    if target_screen is not None and target_screen is not current_screen:
        # If another monitor already shows the requested minor workspace,
        # swap displays rather than duplicating the group or violating Qtile's
        # one-group-per-screen model.
        swap_screen_groups(current_screen, target_screen)
    else:
        current_screen.set_group(target_group)

    save_visible_minor_config(qtile_obj)


def go_to_secondary_delta(qtile_obj: Any, delta: int, move_window: bool = False) -> None:
    _, secondary = current_parts(qtile_obj)
    index = SECONDARIES.index(secondary) if secondary in SECONDARIES else 0
    target_secondary = SECONDARIES[(index + delta) % len(SECONDARIES)]
    go_to_secondary(qtile_obj, target_secondary, move_window)


def remember_current_subworkspace() -> None:
    qtile_any = cast(Any, qtile)
    save_visible_minor_config(qtile_any)
    apply_bar_visibility(qtile_any)


def get_screen_bars(screen: Any) -> list[Any]:
    bars = []
    for position in ("top", "bottom", "left", "right"):
        bar_obj = getattr(screen, position, None)
        if bar_obj is not None:
            bars.append(bar_obj)
    return bars


def set_bar_visible(bar_obj: Any, visible: bool) -> None:
    show = getattr(bar_obj, "show", None)
    if callable(show):
        try:
            show(visible)
            return
        except TypeError:
            pass

    # Fallback for future/older Qtile API drift. If this does not work on a
    # given version, it should fail harmlessly rather than breaking the config.
    window = getattr(bar_obj, "window", None)
    if window is not None:
        try:
            if visible:
                window.unhide()
            else:
                window.hide()
        except Exception:
            pass


def apply_bar_visibility(qtile_obj: Any) -> None:
    for screen in getattr(qtile_obj, "screens", []):
        group = getattr(screen, "group", None)
        group_name_value = getattr(group, "name", "")
        visible = not _bar_hidden_by_group.get(group_name_value, False)
        for bar_obj in get_screen_bars(screen):
            set_bar_visible(bar_obj, visible)


def toggle_bar_for_current_group(qtile_obj: Any) -> None:
    group = getattr(qtile_obj, "current_group", None)
    group_name_value = getattr(group, "name", None)
    if group_name_value is None:
        return

    _bar_hidden_by_group[group_name_value] = not _bar_hidden_by_group.get(group_name_value, False)
    apply_bar_visibility(qtile_obj)


def call_window_toggle_minimize(window: Any) -> None:
    toggle = getattr(window, "toggle_minimize", None)
    if callable(toggle):
        toggle()
        return

    cmd_toggle = getattr(window, "cmd_toggle_minimize", None)
    if callable(cmd_toggle):
        cmd_toggle()


def focus_window_object(group: Any, window: Any) -> None:
    try:
        group.focus(window, warp=False)
    except TypeError:
        group.focus(window)

    try:
        window.focus(warp=False)
    except TypeError:
        window.focus(False)
    except Exception:
        pass


def find_focus_replacement(group: Any, minimized_window: Any) -> Any | None:
    windows = list(getattr(group, "windows", []) or [])
    if not windows:
        return None

    if minimized_window in windows:
        index = windows.index(minimized_window)
        ordered_windows = windows[index + 1 :] + windows[:index]
    else:
        ordered_windows = windows

    for window in ordered_windows:
        if window is minimized_window:
            continue
        if getattr(window, "minimized", False):
            continue
        return window

    return None


def minimize_current_window(qtile_obj: Any) -> None:
    global _minimized_history

    window = getattr(qtile_obj, "current_window", None)
    if window is None:
        return

    if getattr(window, "minimized", False):
        return

    group = getattr(window, "group", None) or getattr(qtile_obj, "current_group", None)
    replacement = find_focus_replacement(group, window) if group is not None else None

    _minimized_history = [window] + [old for old in _minimized_history if old is not window]
    call_window_toggle_minimize(window)

    if replacement is not None and group is not None:
        focus_window_object(group, replacement)


def unminimize_last_window(qtile_obj: Any) -> None:
    global _minimized_history

    group = getattr(qtile_obj, "current_group", None)
    if group is None:
        return

    candidates = []
    candidates.extend(_minimized_history)
    candidates.extend(reversed(getattr(group, "windows", []) or []))

    seen = set()
    for window in candidates:
        marker = id(window)
        if marker in seen:
            continue
        seen.add(marker)

        if getattr(window, "group", None) is not group:
            continue
        if not getattr(window, "minimized", False):
            continue

        call_window_toggle_minimize(window)
        focus_window_object(group, window)
        _minimized_history = [old for old in _minimized_history if old is not window]
        return


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
    """Start monitors on 1/a, 1/b, ..., 1/$n."""
    qtile_any = cast(Any, qtile)
    screens = list(getattr(qtile_any, "screens", []))
    letters = normalize_minor_config(DEFAULT_MAIN, len(screens))

    for index, screen in enumerate(screens):
        target = qtile_any.groups_map.get(group_name(DEFAULT_MAIN, letters[index]))
        if target is not None:
            screen.set_group(target)

    save_visible_minor_config(qtile_any)
    apply_bar_visibility(qtile_any)


def focus_previous_window(qtile_obj: Any) -> None:
    """Focus the previously focused live window.

    If the target is in a different major group, all displays travel to that
    major group. The target window's minor workspace is forced onto the current
    physical display, and the rest of that major's saved per-display layout is
    restored around it.
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

    save_visible_minor_config(qtile_obj)

    target_main, target_secondary = parse_group_name(target_group.name)
    current_main = visible_major(qtile_obj)
    current_screen = getattr(qtile_obj, "current_screen", None)
    current_index = current_screen_index(qtile_obj)

    if target_main != current_main:
        switch_major_on_all_screens(
            qtile_obj,
            target_main,
            preferred_screen_index=current_index,
            preferred_secondary=target_secondary,
        )
    else:
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

    save_visible_minor_config(qtile_obj)


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

      [a]    focused workspace 1/a                  bright yellow
      (b)    visible workspace 1/b with windows      green
       c     visible but empty workspace 1/c          dim grey
      2/t    hidden workspace 2/t with windows        blue
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
                    + pango_span(name, COLORS["focused"], weight="bold")
                    + pango_span("]", COLORS["subtle"])
                )
            elif is_visible and has_windows:
                parts.append(
                    pango_span("(", COLORS["subtle"])
                    + pango_span(name, COLORS["clock"], weight="bold")
                    + pango_span(")", COLORS["subtle"])
                )
            elif is_visible:
                parts.append(pango_span(name, COLORS["clock"]))
            else:
                parts.append(pango_span(name, COLORS["clock"]))

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
    Key([mod], "F2", lazy.function(toggle_bar_for_current_group), desc="Toggle top bar for this workspace"),
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
    Key([mod], "m", lazy.function(minimize_current_window), desc="Minimize focused window"),
    Key([mod, "shift"], "m", lazy.function(unminimize_last_window), desc="Unminimize last minimized window"),
    Key([mod], "f", lazy.window.toggle_floating(), desc="Toggle floating"),
    Key([mod, "shift"], "f", lazy.window.toggle_floating(), desc="Toggle floating"),
    Key([mod], "F11", lazy.window.toggle_fullscreen(), desc="Explicitly fullscreen focused window"),

    # Minor workspace movement within the current global major group.
    Key([mod], "h", lazy.function(go_to_secondary_delta, -1), desc="Previous letter workspace on this display"),
    Key([mod], "l", lazy.function(go_to_secondary_delta, 1), desc="Next letter workspace on this display"),
    Key([mod, "shift"], "h", lazy.function(go_to_secondary_delta, -1, True), desc="Move window to previous letter workspace"),
    Key([mod, "shift"], "l", lazy.function(go_to_secondary_delta, 1, True), desc="Move window to next letter workspace"),

    # Temporary/default minor workspace inside the current global major group.
    Key([mod], "a", lazy.function(go_to_secondary, DEFAULT_SECONDARY), desc="Go to temporary/default letter workspace"),
    Key([mod, "shift"], "a", lazy.function(go_to_secondary, DEFAULT_SECONDARY, True), desc="Move window to temporary/default workspace"),
]

# Numeric major workspaces: Mod-1..9. These switch all displays together.
for main in MAIN_GROUPS:
    keys.extend(
        [
            Key([mod], str(main), lazy.function(go_to_main, main), desc=f"Go to major workspace {main}"),
            Key([mod, "shift"], str(main), lazy.function(go_to_main, main, True), desc=f"Move window and go to major workspace {main}"),
        ]
    )

# Letter minor workspaces: Mod-Control-a..z.
for secondary in SECONDARIES:
    keys.extend(
        [
            Key([mod, "control"], secondary, lazy.function(go_to_secondary, secondary), desc=f"Go to {secondary} on this display"),
            Key(
                [mod, "control", "shift"],
                secondary,
                lazy.function(go_to_secondary, secondary, True),
                desc=f"Move window to {secondary} on this display",
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
                    foreground=COLORS["active_visible"],
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
