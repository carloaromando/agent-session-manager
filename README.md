# agent-session-manager

An ibuffer-style list of [agent-shell](https://github.com/xenodium/agent-shell)
sessions, grouped by project.

Open sessions come from `agent-shell-buffers`. Closed sessions come from a
per-agent backend; today only `pi` is registered, reading the session map kept
by the pi-acp adapter. Both lists are merged on the ACP session id, and the
open side wins. That means a live session already shows its dates and file
path while the agent is still working.

## Requirements

- Emacs 29.1 or newer
- `agent-shell` 0.60.0 or newer
- For the `pi` backend: the pi-acp adapter, with its session map at
  `~/.pi/pi-acp/session-map.json`

## Installation

### Nix

The flake exports an overlay:

```nix
{
  inputs.agent-session-manager.url = "path:/path/to/agent-session-manager";

  outputs = { nixpkgs, agent-session-manager, ... }: {
    nixpkgs.overlays = [ agent-session-manager.overlays.default ];
  };
}
```

This makes `pkgs.agent-session-manager` available.

### Manually

Put the file on your `load-path` and require it:

```elisp
(add-to-list 'load-path "/path/to/agent-session-manager")
(require 'agent-session-manager)
```

`agent-shell` must be installed and loadable.

### use-package

```elisp
(use-package agent-session-manager
  :ensure nil
  :bind ("C-c a" . agent-session-manager))
```

## Usage

Run `M-x agent-session-manager` to open the list. The buffer is grouped by
project root: projectile is used when it is active, otherwise the built-in
project.el. Sessions that belong to no project land in the `Free` group.

| Key     | Command                                         |
|---------|-------------------------------------------------|
| `RET`   | Open a live session, or resume a closed one     |
| `M-RET` | Resume a session, picking the agent yourself    |
| `TAB`   | Fold or unfold the project heading at point     |
| `n`     | Start a session on a file with your preferred agent |
| `M-n`   | Start a session on a file, picking the agent    |
| `r`     | Rename the session at point                     |
| `d`     | Mark the session for deletion                   |
| `u`     | Remove the mark                                 |
| `x`     | Delete every marked session, after confirmation |
| `k`     | Kill the buffer without touching disk           |
| `w`     | Copy the ACP session id                         |
| `g`     | Refresh the list                                |
| `q`     | Close the list                                  |

The columns are Status, Name, Agent, Created and Updated. Status is one of
`ready`, `busy`, `blocked`, `starting`, `closed` or `missing`. A `missing`
session has a map entry but no file on disk, so `RET` refuses to open it.

Names you give sessions are stored in
`agent-session-manager-names-file`. Nothing is ever written into the
agent's own data. A name given to a session that has no id yet is kept in
memory and saved once the id appears; if the buffer is closed before that, the
name is lost.

Deleting a session moves its transcript to the trash first, then removes its
entry from the pi session map. Deleting is a two-step operation: mark with `d`,
then confirm with `x`.

## Customization

| Variable | Default | Purpose |
|----------|---------|---------|
| `agent-session-manager-names-file` | `~/.emacs.d/agent-session-manager.eld` | Where session names are saved |
| `agent-session-manager-pi-map-file` | `~/.pi/pi-acp/session-map.json` | Session map written by pi-acp |
| `agent-session-manager-free-group-name` | `"Free"` | Heading for sessions with no project |
| `agent-session-manager-name-max-width` | `48` | Width at which derived names are truncated |

## Caveats

The package reads a few internals of `agent-shell` that have no public
accessor, such as the session title and the preferred agent hook. These are
deliberate break points: if upstream renames them, the package fails loudly
instead of showing wrong data.

Closed sessions are only available for backends that register themselves.
Only the `pi` backend is included at the moment; other agents still appear
while their shell buffer is live.

Columns are not sortable. Sorting would scatter the project headings, which
are entries like any other.

## License

BSD 3-Clause. See [LICENSE](LICENSE).
