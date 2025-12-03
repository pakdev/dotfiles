# Dotfiles

My personal configuration files managed with GNU Stow.

## Contents

- **zsh**: Zsh shell configuration
- **git**: Git configuration
- **alacritty**: Alacritty terminal emulator
- **nvim**: Neovim (LazyVim) configuration
- **zellij**: Zellij terminal multiplexer
- **lazygit**: LazyGit TUI configuration
- **lazydocker**: LazyDocker TUI configuration
- **starship**: Starship prompt configuration

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
   stow zsh git alacritty nvim zellij lazygit lazydocker starship
   ```

   Or stow individual packages:
   ```bash
   stow zsh
   stow nvim
   # etc...
   ```

## Usage

- **Add new configs**: Move the file/directory to the appropriate package folder, then run `stow <package>`
- **Remove symlinks**: `stow -D <package>`
- **Restow (refresh)**: `stow -R <package>`

## Notes

- Symlinks point from your home directory back to this dotfiles directory
- Any changes made to the actual config files are automatically tracked since they're in the git repo
