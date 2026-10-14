# Emacs config

Personal GNU Emacs configuration: fast startup, Vertico/Consult/Corfu for
completion, Eglot (LSP), Tree-sitter, Lisp languages (Common Lisp, Racket,
Clojure), Magit, vterm, and [Claude Code](https://claude.com/claude-code)
integration.

Packages are installed automatically on first launch via `use-package`.

## Contents

| File            | Purpose                                                          |
|-----------------|------------------------------------------------------------------|
| `early-init.el` | Startup optimization (GC, file handlers), `package-quickstart`, UI without bars |
| `init.el`       | Main configuration                                               |
| `custom.el`     | Customize variables (kept separate from `init.el`)               |

Main packages: `vertico`, `orderless`, `marginalia`, `consult`, `embark`,
`corfu`, `cape`, `treesit-auto`, `eglot`, `magit`, `paredit`, `slime`,
`racket-mode`, `cider`, `clojure-mode`, `yasnippet`, `vterm`, `which-key`,
`gcmh`, `abyss-theme`, `exec-path-from-shell`, `claude-code-ide`.

## Requirements

- **GNU Emacs 30+** (needed for the `:vc` keyword in `use-package`).
- `git` — to clone the config and to install `claude-code-ide` via `package-vc`.
- A C compiler, `cmake`, `libtool`, `libvterm` — to build the `vterm` module
  and Tree-sitter grammars.
- Optional language tooling (see below).

## Installation

### 1. System packages

**Fedora**

```bash
sudo dnf install emacs git gcc make cmake libtool libvterm-devel
```

**Debian 13+ / Ubuntu**

```bash
sudo apt install emacs git build-essential cmake libtool-bin libvterm-dev
```

> Ubuntu 24.04 and older ship Emacs 29, which is not enough.
> Install Emacs 30 via snap instead: `sudo snap install emacs --classic`.

**Arch Linux**

```bash
sudo pacman -S emacs git base-devel cmake libtool libvterm
```

**macOS (Homebrew)**

```bash
brew install --cask emacs
brew install git cmake libtool libvterm
```

Check the version:

```bash
emacs --version   # must be 30.x or newer
```

### 2. Clone the configuration

If you already have an Emacs config, back it up first — `~/.emacs` takes
precedence over `~/.emacs.d/init.el` and will prevent this config from loading:

```bash
[ -e ~/.emacs ]          && mv ~/.emacs ~/.emacs.bak
[ -e ~/.emacs.el ]       && mv ~/.emacs.el ~/.emacs.el.bak
[ -d ~/.emacs.d ]        && mv ~/.emacs.d ~/.emacs.d.bak
[ -d ~/.config/emacs ]   && mv ~/.config/emacs ~/.config/emacs.bak
```

Clone the repository:

```bash
git clone https://github.com/sokol-sa/.emacs.d.git ~/.emacs.d
```

### 3. First launch

```bash
emacs
```

On first launch Emacs will:

1. Fetch package lists from GNU ELPA, NonGNU ELPA and MELPA.
2. Install all packages (takes a few minutes; native compilation then
   continues in the background).
3. Clone `claude-code-ide` from GitHub.

You may be prompted during setup:

- **vterm** — on the first `M-x vterm` it offers to build `vterm-module`.
  Answer `yes` (requires `cmake`, `libtool`, `libvterm` from step 1).
- **treesit-auto** — when opening a file in a new language it offers to
  download and build the Tree-sitter grammar. Answer `y`.

Restart Emacs once installation is finished.

### 4. Language tooling (as needed)

Install only what you use.

**LSP servers for Eglot**

| Language | Server                 | Install                                     |
|----------|------------------------|---------------------------------------------|
| Python   | `pylsp`                | `pipx install python-lsp-server`            |
| Ruby     | `ruby-lsp`             | `gem install ruby-lsp`                      |
| YAML     | `yaml-language-server` | `npm install -g yaml-language-server`       |
| Markdown | `marksman`             | binary from [GitHub Releases](https://github.com/artempyanykh/marksman/releases) into `~/.local/bin` |

**Lisp languages**

| Language    | Requirement                | Fedora                    | Debian/Ubuntu             |
|-------------|----------------------------|---------------------------|---------------------------|
| Common Lisp | `sbcl` (for SLIME)         | `sudo dnf install sbcl`   | `sudo apt install sbcl`   |
| Racket      | `racket`                   | `sudo dnf install racket` | `sudo apt install racket` |
| Clojure     | JDK + Clojure CLI or Leiningen (for CIDER) | see [clojure.org/guides/install_clojure](https://clojure.org/guides/install_clojure) | same |

### 5. Claude Code (optional)

The `claude-code-ide` integration uses the `claude` CLI and a Claude
subscription (no API key required).

```bash
curl -fsSL https://claude.ai/install.sh | bash
claude   # sign in once
```

The CLI is installed into `~/.local/bin`. The config adds this directory to
`PATH` automatically, and `exec-path-from-shell` imports your shell's `PATH`
into GUI Emacs and daemon mode.

In Emacs: `C-c C-'` opens the Claude Code menu.

## Key bindings

| Keys            | Action                                      |
|-----------------|---------------------------------------------|
| `C-s`           | Search lines in buffer (`consult-line`)     |
| `C-x b`         | Switch buffer (`consult-buffer`)            |
| `C-x C-g`       | Recent files with preview                   |
| `M-y`           | Kill-ring history                           |
| `C-.`           | `embark-act` — actions on thing at point    |
| `C-;`           | `embark-dwim`                               |
| `C-h B`         | List bindings (`embark-bindings`)           |
| `C-c p`         | Cape prefix (manual completion sources)     |
| `C-<tab>`       | Indent region or insert a literal TAB       |
| `C-c a c`       | Align comments in region                    |
| `S-<arrows>`    | Move between windows (`windmove`)           |
| `C-S-<arrows>`  | Resize windows (`windsize`)                 |
| `C-c C-'`       | Claude Code menu                            |

## Defaults

- Session (buffers, windows, frames) is saved and restored via `desktop-save-mode`.
- Files are auto-saved (`auto-save-visited-mode`); versioned backups go to
  `~/.emacs.d/backups/`.
- Enabled: `tab-bar-mode`, `repeat-mode`, `delete-selection-mode`,
  `electric-pair-mode` (replaced by `paredit` in Lisp modes).
- Text modes auto-pair typographic quotes: «», „“, ‘’.

## Updating

```bash
cd ~/.emacs.d && git pull
```

Update packages from within Emacs: `M-x package-upgrade-all`.
After manual changes to the package list, run `M-x package-quickstart-refresh`.

## Troubleshooting

- **Errors on first launch** — usually network-related. Restart Emacs;
  installation resumes where it stopped. If that doesn't help, run
  `M-x package-refresh-contents` and restart.
- **A package is broken** — delete its directory under `~/.emacs.d/elpa/`
  and restart Emacs.
- **Full package reset** — `rm -rf ~/.emacs.d/elpa ~/.emacs.d/eln-cache`
  and restart.
- **Eglot can't find a server / Emacs can't see `claude`** — check
  `M-: (getenv "PATH")` and make sure the binary is in one of those directories.

## License

See [LICENSE](LICENSE).
