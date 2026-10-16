# Dotfiles

My personal configuration files managed with GNU Stow.

## Contents

- **zsh**: Zsh shell configuration
- **git**: Git configuration
- **alacritty**: Alacritty terminal emulator
- **ghostty**: Ghostty terminal emulator
- **nvim**: Neovim (LazyVim) configuration
- **zellij**: Zellij terminal multiplexer
- **lazygit**: LazyGit TUI configuration
- **lazydocker**: LazyDocker TUI configuration
- **starship**: Starship prompt configuration
- **cosmic**: Dark violet COSMIC theme, always-visible translucent panel/dock, nebula wallpaper, and FiraCode Nerd Font terminal palette
- **niri**: Niri scrollable-tiling Wayland compositor configuration (with `swaybg` overview backdrop)
- **waybar**: Waybar status bar configuration and styling for Niri
- **wpaperd**: Wallpaper daemon configuration for rotating workspace backgrounds
- **fuzzel**: Fuzzel application launcher configuration
- **swaylock**: Swaylock screen locker configuration and `~/.local/bin/lock-screen` helper

## Installation

1. Clone this repository:
   ```bash
   git clone <your-repo-url> ~/dotfiles
   ```

2. Install GNU Stow:
   ```bash
   sudo pacman -S stow
   ```

3. Stow the packages you want:
   ```bash
   cd ~/dotfiles
   stow zsh git alacritty ghostty nvim zellij lazygit lazydocker starship niri waybar wpaperd fuzzel swaylock
   ```

   Or stow individual packages:
   ```bash
   stow zsh
   stow nvim
   # etc...
   ```

### Windows

Requires Windows PowerShell 5.1 or newer, [WinGet/App Installer](https://learn.microsoft.com/windows/package-manager/winget/), and Windows Developer Mode (or an elevated shell) for native symlinks. Clone or download this repository, keep it in a permanent location, then run from its directory:

```powershell
powershell.exe -NoProfile -ExecutionPolicy Bypass -File .\install-windows.ps1
```

The script checks that tools actually run, so broken shims and Windows Store execution aliases do not count as installed tools. Missing tools are installed with WinGet; existing usable tools are kept, not upgraded. Installers may request elevation. Run the script as the account receiving the dotfiles: elevating under different credentials changes the profile and user environment it targets.

| Configuration | Tools installed when missing | WinGet IDs |
| --- | --- | --- |
| All packages | Git for Windows (bundled Perl for Stow) | `Git.Git` |
| Git / LazyGit | Delta, Difftastic | `dandavison.delta`, `Wilfred.difftastic` |
| Neovim | Neovim, Python 3.13, ripgrep, fd, GCC and bundled GNU Make | `Neovim.Neovim`, `Python.Python.3.13`, `BurntSushi.ripgrep.MSVC`, `sharkdp.fd`, `BrechtSanders.WinLibs.POSIX.UCRT` |
| Starship | Starship | `Starship.Starship` |
| Alacritty | Alacritty | `Alacritty.Alacritty` |
| LazyGit | LazyGit | `JesseDuffield.lazygit` |
| LazyDocker | LazyDocker | `JesseDuffield.Lazydocker` |

GNU Stow itself is not available in WinGet. The script downloads [GNU Stow 2.4.1 from CPAN](https://metacpan.org/dist/Stow), verifies its pinned SHA-256 checksum, and runs it with Git for Windows' Perl. It installs a `stow.cmd` launcher under `%LOCALAPPDATA%\Programs\GNUStow`, using `MSYS=winsymlinks:nativestrict` to create real Windows symlinks rather than copies. WinGet's Winstow is a different implementation, not a fully compatible GNU Stow port.

WinLibs supplies GNU Make as `mingw32-make.exe`. If `make` is missing or unusable, the script creates a native `make.exe` symlink under `%LOCALAPPDATA%\Programs\Dotfiles\bin` and adds that directory to your user PATH; no separate Make installer is needed.

By default, the script links `git`, `nvim`, `starship`, `alacritty`, `lazygit`, `lazydocker`, and `zellij` into your Windows profile. Existing conflicting files, directories, and links are moved to adjacent `.pre-stow-<timestamp>-<id>` backups; configurations are never adopted into the repository. Correct existing links are left in place, so the script can be rerun. A pre-existing `.config` must be a real directory, not a file or link.

It persists `XDG_CONFIG_HOME` to `<target>\.config` and adds working tool directories and the Stow launcher to your user PATH. Open a new terminal afterward. It verifies native symlinks and checks Neovim/LazyGit config discovery without starting Neovim's plugin installer.

Options, when invoking from PowerShell:

```powershell
# Only selected packages and their tool dependencies.
.\install-windows.ps1 -Packages git,nvim,lazygit

# Link configs without installing missing tools (Git/Perl and GNU Stow must already exist).
.\install-windows.ps1 -SkipToolInstall

# Read-only preview: checks tools and reports collisions; requires installed Git/Perl and Stow.
# Conflicts produce an error in preview; the normal run backs them up.
.\install-windows.ps1 -Preview

# Alternate link target; XDG_CONFIG_HOME will also point into this target.
.\install-windows.ps1 -Target 'D:\Home'
```

After installation, run normal Stow maintenance commands from this repository:

```powershell
stow -R nvim
stow -D nvim
```

Windows limitations:

- **Git:** This repository sets a personal name/email and external diff tools. Previous credential helpers remain in the backup, not the active configuration. Review `git/.gitconfig` before applying it on another account; omit `git` with `-Packages` if you want to keep your current Git settings.
- **Zellij, Zsh, tmux:** These are not native Windows applications. Only Zellij's configuration is linked by this script; use the Unix tools inside WSL with that environment's own home directory and Stow installation.
- **Alacritty:** Its WinGet MSI installer may require administrator approval. The configuration imports an Omarchy theme and expects CaskaydiaMono Nerd Font. Those resources are not installed by this script.
- **LazyGit:** Its Ghostty/Zsh worktree shortcut is Unix-specific.
- **LazyDocker:** A Docker engine must be configured separately; this script does not install Docker Desktop or enable WSL.
- **Neovim:** Plugins install on first normal launch. Language servers, SDKs, and plugin-specific Python libraries may have additional requirements beyond the core tools installed here.
- **Starship:** Installing the tool/config does not modify your PowerShell profile. Add `Invoke-Expression (&starship init powershell)` to `$PROFILE` if you want it enabled there.

## Usage

- **Add new configs**: Move the file/directory to the appropriate package folder, then run `stow <package>`
- **Remove symlinks**: `stow -D <package>`
- **Restow (refresh)**: `stow -R <package>`

## COSMIC appearance

Stow `cosmic` to link its settings, then apply the native theme preset:

```bash
cosmic-settings appearance import "$HOME/git/dotfiles/cosmic/violet.ron"
```

The panel and floating dock never auto-hide and reserve space for windows. The
wallpaper uses the Orion nebula image supplied by `cosmic-wallpapers`; COSMIC
Terminal uses the installed `FiraCode Nerd Font` and the `Space Station` palette.
The preset changes COSMIC settings only, not GNOME themes or extensions.

## Notes

- Symlinks point from your home directory back to this dotfiles directory
- Any changes made to the actual config files are automatically tracked since they're in the git repo
