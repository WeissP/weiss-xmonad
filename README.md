# weiss-xmonad
[![weiss-xmonad on hackage](https://img.shields.io/hackage/v/weiss-xmonad)](http://hackage.haskell.org/package/weiss-xmonad)
[![weiss-xmonad on Stackage Nightly](https://stackage.org/package/weiss-xmonad/badge/nightly)](https://stackage.org/nightly/package/weiss-xmonad)

Generated with [template-haskell](https://github.com/jonascarpay/template-haskell)

XMonad runs independently of the status bar. Start Taffybar separately or through
your desktop services. `Mod-b` toggles the space reserved for docks and bars.

## Workspaces

[WorkspaceFamily.hs](src/WorkspaceFamily.hs) defines nine numbered families
(`1`–`9`) and labeled families. Each has nine members: numbered workspaces use
names such as `1.1` and `1.2`; labeled workspaces use `览`, `览2`, …, `览9`.
Each family remembers its last selected member across XMonad restarts.

All workspace shortcuts are key sequences starting with `<XF86Launch7>`.
Member/family numbers `1`–`9` map to `m , . j k l u i o`, respectively.

| Keys after `<XF86Launch7>` | Destination |
| --- | --- |
| `Return`, number key | Numbered family, at its remembered member |
| `End`, number key | Member of the current family |
| `Down` / `h` / `Up` / `y` / `n` / `-` / `0` | Labeled family `览` / `邮` / `媒` / `聊` / `设` / `记` / `NSP` (scratchpads) |
| `End`, labeled-family key, number key | Specific member of a labeled family |
| `m , . j k l` | Members 1–6 of the first ordered numbered family |
| `u i o` | Members 1–3 of the second ordered numbered family |

Numbered-family order starts at `1`–`9`. Selecting a numbered family with
`Return` exchanges its position with the current numbered family (family `1`
is the fallback when on a labeled workspace). This order also persists.

Insert an action key before the destination sequence: `Escape` moves the
focused window, `Space` moves it and follows it (focusing its screen if already
visible), and `Home` swaps the destination workspace with the current one.
Without an action key, the sequence switches workspaces, bringing the
destination onto the current screen if needed. For example,
`<XF86Launch7> Escape End ,` moves the focused window to member 2 of the
current family.

Before the action/destination keys, optionally press `<XF86Launch8>` once to
toggle focus between screens 0 and 1, or twice for screens 0 and 2.

## Taffybar

Run the independent bar from the repository root, inside `nix develop`:

```sh
cabal run weiss-xmonad:exe:taffybar
```

Stop the foreground process before rebuilding/restarting it. The desktop
session needs `status-notifier-watcher` for the tray; use `xembedsniproxy` for
legacy XEmbed icons.

The Nix output `packages.${system}.weiss-taffybar` installs `bin/taffybar`, so it
can be used directly with Home Manager's service. For a flake input named
`weiss-xmonad`:

```nix
services.taffybar = {
  enable = true;
  package = inputs.weiss-xmonad.packages.${pkgs.system}.weiss-taffybar;
};
```

Home Manager can use its standard `${cfg.package}/bin/taffybar` command.

The bar shows CPU history, RAM and swap used/total in GiB, root filesystem usage,
temperature, network transfer rates, tray icons, and media controls. Memory
values update every two seconds; its tooltip includes exact KiB values.
Hover memory or disk usage for details; click disk usage to refresh it. Click
the clock to open its calendar. Optional widgets hide when space is limited.

Each monitor chooses its layout from its geometry. Portrait screens and screens
narrower than 1600 logical pixels show a compact bar with CPU, memory, disk
percentage, time, tray, and shorter media titles. Wider landscape screens also
show memory history, temperature, network rates, available disk space, and the date.
The `playerctld` media proxy is hidden because its real player is already shown.

The full bar also shows weather for Kaiserslautern from wttr.in, with conditions
and Celsius temperature but no city name. It refreshes every ten minutes and
shows wind in the tooltip. Positive temperatures omit the plus sign. Weather
text is blue at 0°C or below, white below 20°C, amber below 30°C, and red at
30°C or above. Click to refresh manually.
The compact bar does not include weather.

To arrange widgets, edit `fullBar` and `compactBar` in
`weiss-taffybar/taffybar.hs`. Each layout has plain `startWidgets`,
`centerWidgets`, and `endWidgets` lists. End widgets are ordered from the right
edge inward. For example:

```haskell
startWidgets = [workspacesCompact, cpuCompact, memoryCompact, diskCompact]
endWidgets = [tray, mediaCompact]
```

Configure the named widgets in `weiss-taffybar/Widgets.hs`. Widgets with two
styles expose both names, such as `cpu` and `cpuCompact`; shared widgets such as
`tray` and `network` work in either layout. The lists choose the style explicitly,
so a full widget can also be used in the compact bar. Widget definitions use
`withPriority` to mark optional widgets when space is limited. Shared bar appearance is in `defBar`;
monitor geometry selection remains separate from the widget lists.

Workspace names appear at the left, without window icons. The wide bar shows
nonempty workspaces; the compact bar shows active, visible, and urgent
workspaces. Scratchpad workspaces are hidden. Click a name to switch to it.
Numbered families such as `1.1` and `1.2` share a tightly spaced group;
different families have larger gaps. All groups use the same neutral background.
When a numbered Chinese member is shown, its family gets a shared heading and
member buttons, for example `览 [1] [2]` for `览` and `览2`. If only `览2` is
shown, it appears as `览 [2]`. Chinese families without visible numbered members
keep their existing plain labels and shared group.

Workspace styling follows one rule: **group top edges show shortcut order;
member buttons show workspace state**.

| Element                                         | Style                                                |
|-------------------------------------------------|------------------------------------------------------|
| First ordered numbered family group             | Solid lavender top edge                                 |
| Second ordered numbered family group            | Dotted lavender top edge                                |
| All family headings                             | Pale lavender text on muted plum badges              |
| Inactive member                                 | White text, transparent background                   |
| Focused member                                  | Soft teal fill, dark bold text                           |
| Member visible on another screen                | Soft teal outline, no fill                               |
| Member containing a window requesting attention | Amber underline, preserving focus/visibility styling |

Family headings are bold and slightly larger than member labels, with the same
muted plum background badge for every leading number and no borders. The same member-state rules apply to Chinese workspaces.
The top-edge markers span the whole family group and follow XMonad's persistent
shortcut order. Every group reserves the same two-pixel top border, transparent
when unmarked, so order changes never change its width or height. All family
headings use pale lavender text. Restart the rebuilt XMonad once to publish the
initial order; without it, all groups remain unmarked.

XMonad's `pagerHints` supplies visibility on the other screen; rebuild and
restart XMonad as well as Taffybar when first enabling these hints.

The dark theme in `weiss-taffybar/assets/taffybar.css` is installed with the executable.
During development, edits to this file reload automatically. Personal overrides
can go in `$XDG_CONFIG_HOME/taffybar/taffybar.css` (usually
`~/.config/taffybar/taffybar.css`), which loads after the bundled theme and also
reloads on changes. Installed Nix store files cannot be edited; use the personal
stylesheet to tune an installed bar. Prefix the launch command with
`GTK_DEBUG=interactive` to inspect widgets and try styles.
