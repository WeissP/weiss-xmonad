# weiss-xmonad
[![weiss-xmonad on hackage](https://img.shields.io/hackage/v/weiss-xmonad)](http://hackage.haskell.org/package/weiss-xmonad)
[![weiss-xmonad on Stackage Nightly](https://stackage.org/package/weiss-xmonad/badge/nightly)](https://stackage.org/nightly/package/weiss-xmonad)

Generated with [template-haskell](https://github.com/jonascarpay/template-haskell)

XMonad runs independently of the status bar. Start Taffybar separately or through
your desktop services. `Mod-b` toggles the space reserved for docks and bars.

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
Numbered families such as `1.1` and `1.2` share a tightly spaced, tinted group;
different families have larger gaps.
The focused workspace has a blue fill; workspaces visible on another screen
have a teal outline and background.
XMonad's `pagerHints` supplies visibility on the other screen; rebuild and
restart XMonad as well as Taffybar when first enabling these hints.

The dark theme in `weiss-taffybar/taffybar.css` is installed with the executable.
During development, edits to this file reload automatically. Personal overrides
can go in `$XDG_CONFIG_HOME/taffybar/taffybar.css` (usually
`~/.config/taffybar/taffybar.css`), which loads after the bundled theme and also
reloads on changes. Installed Nix store files cannot be edited; use the personal
stylesheet to tune an installed bar. Prefix the launch command with
`GTK_DEBUG=interactive` to inspect widgets and try styles.
