# Repository guidance

Personal Haskell configuration for the XMonad X11 window manager, with a separate
Taffybar status bar executable. Keep changes focused on the requested desktop
behavior and preserve existing personal bindings and machine-specific settings.

The owner normally compiles and deploys this through the full Nix configuration
switch. Taffybar runs independently; XMonad does not launch a status bar.
Iterate on Taffybar independently during development, then integrate the
working result into the Nix configuration.

## Structure

- `app/Main.hs`: XMonad entry point.
- `src/WeissXMonad.hs`: layouts, keybindings, manage/event/startup hooks, and
  XMonad startup. `src/Config.hs` holds shared appearance and terminal settings.
- `src/WorkspaceFamily.hs`: workspace families and navigation keys.
- `src/WeissScratchpad.hs`, `WeissWindowOperations.hs`, `TreeActions.hs`, and
  `WeissPromptPass.hs`: scratchpads, focus/swap behavior, action menus, and
  password prompts. `Utils.hs` provides shared helpers.
- `weiss-taffybar/taffybar.hs`: independent Taffybar entry point and widgets.
- `vendor/xmonad-contrib/` and `vendor/taffybar/`: local dependency sources
  included by `cabal.project`; changes here affect the application build.
- `weiss-xmonad.cabal`: library/executable components, dependencies, extensions,
  and warnings. `flake.nix`/`flake.lock` define Nix builds and the development
  shell; `hie.yaml` maps source directories to Cabal components.

## Build and validation

Run from the repository root:

```sh
nix develop                         # Cabal, GHC, Fourmolu, HLint, HLS
nix build .#                        # XMonad executable; matches CI
nix build .#weiss-taffybar           # Taffybar executable
```

Within the development shell:

```sh
cabal build weiss-xmonad:lib:weiss-xmonad
cabal build weiss-xmonad:exe:weiss-xmonad-exe
cabal build weiss-xmonad:exe:taffybar
fourmolu --mode check path/to/Changed.hs
hlint path/to/Changed.hs
git diff --check
```

- The flake selects GHC 9.8 (`ghc98`); Cabal's `tested-with` still lists 9.6.6.
  Prefer the flake environment when reproducing the local build.
- The main package defines no test suite, despite `tests: True` in
  `cabal.project`. Compilation is the automated baseline; CI only builds the
  default Nix package. Formatter and lint commands are additional local checks.
- For dependency changes, the vendored suites can be targeted with
  `cabal test xmonad-contrib:tests` or `cabal test taffybar:unit`.
- UI behavior needs an X11 session: check affected keybindings, layouts,
  workspace navigation, scratchpads, or bar widgets. Report checks performed
  and any runtime behavior left unverified. The full Nix configuration and its
  exact switch command are outside this repository.

## Taffybar development

- Keep Taffybar's optional WirePlumber support disabled via
  `package taffybar` / `flags: -wireplumber` in the root `cabal.project`.
- From `nix develop`, use
  `cabal run weiss-xmonad:exe:taffybar` in the desktop session to
  iterate without switching the entire Nix configuration. Stop the foreground
  process before rerunning. Haskell changes require rebuilding/restarting:
  this executable uses `startTaffybar`, not Dyre.
- Prefix that command with `GTK_DEBUG=interactive` to inspect widgets and try
  CSS. User CSS defaults to `$XDG_CONFIG_HOME/taffybar/taffybar.css` (normally
  `~/.config/taffybar/taffybar.css`) and reloads on changes. Create the containing
  directory before launching; `/nix/store` CSS directories are not watched.
- The custom executable does not implement the upstream `--log-level` flag.
  For Haskell logging, configure `System.Log.Logger` in its entry point and add
  `hslogger` to that executable's direct Cabal dependencies.
- Workspace names appear in `startWidgets`, grouped by numbered family.
  XMonad enables `ewmh`, `pagerHints`, `docks`, and `avoidStruts` for workspace
  visibility and dock support. `Mod-b` toggles the reserved dock space.
- `sniTrayNew` needs a running `status-notifier-watcher`; legacy XEmbed icons
  need a bridge such as `xembedsniproxy`. Both commands are available in the
  development shell. See `vendor/taffybar/doc/run.md` and `doc/custom.md`.

## Editing conventions

- Follow `fourmolu.yaml` (two-space indentation, leading commas) and
  `.hlint.yaml`; format touched files without unrelated cleanup.
- Reuse the Cabal common extensions and existing module organization. Register
  new library modules in `weiss-xmonad.cabal`.
- Check Cabal component membership: `src/WeissEasyMotion.hs` and
  `src/Utils/WindowOps.hs` currently are not listed in the library. Main XMonad
  code imports `XMonad.Actions.EasyMotion` from the vendored dependency.
- Preserve workspace identifiers, including Chinese labels, and their links to
  manage hooks and bar output when changing workspace behavior.
- Keep vendor edits limited to tasks that require dependency changes. Respect
  existing untracked local files and keep generated build/editor artifacts out
  of commits.
- Runtime commands depend on external applications, personal scripts, and
  systemd user services. Building alone does not validate those integrations;
  launching XMonad also runs its startup hook.
