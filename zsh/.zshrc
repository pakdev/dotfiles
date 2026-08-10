# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:$HOME/.local/bin:/usr/local/bin:$PATH

# Path to your Oh My Zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time Oh My Zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME=""

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment one of the following lines to change the auto-update behavior
# zstyle ':omz:update' mode disabled  # disable automatic updates
# zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
# zstyle ':omz:update' frequency 13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
  git 
  command-not-found 
  common-aliases 
  docker 
  docker-compose 
  npm 
  node 
  sudo 
  web-search 
  history-substring-search 
  zoxide
)

# Silence direnv log output (must be before any direnv hook)
export DIRENV_LOG_FORMAT=""

source $ZSH/oh-my-zsh.sh

# Zsh plugins (macOS + non-mac)
if command -v brew >/dev/null 2>&1; then
  ZSH_PLUGIN_PREFIX="$(brew --prefix)/share"
elif [[ -d /usr/local/share ]]; then
  ZSH_PLUGIN_PREFIX="/usr/local/share"
else
  ZSH_PLUGIN_PREFIX="/usr/share"
fi

if [[ -f "$ZSH_PLUGIN_PREFIX/zsh-autosuggestions/zsh-autosuggestions.zsh" ]]; then
  source "$ZSH_PLUGIN_PREFIX/zsh-autosuggestions/zsh-autosuggestions.zsh"
elif [[ -f "/usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh" ]]; then
  source "/usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh"
fi

# We will source zsh-syntax-highlighting at the very end of the file.

# Enhanced completion settings
autoload -Uz compinit
compinit

# Case insensitive completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

# Partial completion suggestions
zstyle ':completion:*' list-suffixes
zstyle ':completion:*' expand prefix suffix

# Menu selection for completions
zstyle ':completion:*:*:*:*:*' menu select

# Color completion listings
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

# Completion caching
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path ~/.zsh/cache/

# Better directory completion
zstyle ':completion:*:cd:*' ignore-parents parent pwd

# Process completion for kill commands
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"

# User configuration

# Add opencode to PATH
export PATH="$HOME/.opencode/bin:$PATH"

# Add Cargo-installed binaries to PATH
export PATH="$HOME/.cargo/bin:$PATH"

# Add shasum for microsandbox
export PATH=/usr/bin/core_perl:$PATH

# Added by microsandbox installer
export PATH="$HOME/.local/bin:$PATH"

# For nix-direnv
# source "$HOME/.nix-profile/share/nix-direnv/direnvrc"

# Add devenv to PATH
export PATH="$HOME/.nix-profile/bin:$PATH"

# Initialize pyenv so local Python versions resolve through shims.
export PYENV_ROOT="$HOME/.pyenv"
[[ -d "$PYENV_ROOT/bin" ]] && export PATH="$PYENV_ROOT/bin:$PATH"

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
export EDITOR='nvim'
export VISUAL='nvim'

# Stowman.sh config
export STOWMAN_DOTDIR="$HOME/git/dotfiles/"

# Compilation flags
# export ARCHFLAGS="-arch $(uname -m)"

# Set personal aliases, overriding those provided by Oh My Zsh libs,
# plugins, and themes. Aliases can be placed here, though Oh My Zsh
# users are encouraged to define aliases within a top-level file in
# the $ZSH_CUSTOM folder, with .zsh extension. Examples:
# - $ZSH_CUSTOM/aliases.zsh
# - $ZSH_CUSTOM/macos.zsh
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# OpenCode alias
alias oc='opencode'

# GitHub Copilot autopilot alias
alias co='copilot --autopilot --yolo'

# Lazygit alias
alias lg='lazygit --use-config-dir ~/.config/lazygit'

# Lazydocker alias
alias ld='lazydocker'

# Docker summary alias (id, name, image)
alias dss='docker ps --format "{{.ID}}\t{{.Names}}\t{{.Image}}"'

# Pete SSH reverse tunnel aliases
alias pete-ssh-tunnel-status='systemctl --user status pete-ssh-tunnel.service'
alias pete-ssh-tunnel-logs='journalctl --user -u pete-ssh-tunnel.service -f'
alias pete-ssh-tunnel-start='systemctl --user start pete-ssh-tunnel.service'
alias pete-ssh-tunnel-stop='systemctl --user stop pete-ssh-tunnel.service'
alias pete-ssh-tunnel-restart='systemctl --user restart pete-ssh-tunnel.service'

# Launch OMP with native Git diff headers so /review can parse PR-style diffs.
# This preserves the global difft configuration for every other Git process.
ompr() {
  local config_dir="${XDG_CACHE_HOME:-$HOME/.cache}/omp"
  local review_config exit_code

  mkdir -p "$config_dir" || return 1
  review_config="$(mktemp "$config_dir/gitconfig-review.XXXXXX")" || return 1

  if ! sed '/^[[:space:]]*external[[:space:]]*=[[:space:]]*difft[[:space:]]*$/d' \
    "$HOME/.gitconfig" > "$review_config"; then
    rm -f -- "$review_config"
    return 1
  fi

  GIT_CONFIG_GLOBAL="$review_config" command omp "$@"
  exit_code=$?
  rm -f -- "$review_config"
  return "$exit_code"
}

# Open Yazi and change to its selected directory on exit.
function y() {
  local tmp
  tmp="$(mktemp -t 'yazi-cwd.XXXXXX')"
  yazi "$@" --cwd-file="$tmp"

  if read -r cwd < "$tmp" && [[ -n "$cwd" && "$cwd" != "$PWD" ]]; then
    builtin cd -- "$cwd"
  fi

  rm -f -- "$tmp"
}

# Added by ni-dev-tools setup - Go tools PATH
export PATH="$HOME/go/bin:$PATH"

# Initialize Starship prompt
eval "$(starship init zsh)"

# Initialize direnv (DIRENV_LOG_FORMAT="" suppresses the "direnv: export" line)
eval "$(direnv hook zsh)"

# Initialize pyenv after PATH setup so its shims take precedence.
eval "$(pyenv init - zsh)"

# Initialize mise
eval "$(mise activate zsh)"

# bun completions
[ -s "/home/peter/.bun/_bun" ] && source "/home/peter/.bun/_bun"

# Use GPG agent for SSH authentication
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

export STOWMAN_DOTDIR=~/git/dotfiles/

# Source zsh-syntax-highlighting at the very end of the file
if [[ -f "$ZSH_PLUGIN_PREFIX/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ]]; then
  source "$ZSH_PLUGIN_PREFIX/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
elif [[ -f "/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ]]; then
  source "/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
fi

# The following lines have been added by Docker Desktop to enable Docker CLI completions.
fpath=(/Users/peter.kurlak/.docker/completions $fpath)
autoload -Uz compinit
compinit
# End of Docker CLI completions

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

# mise
eval "$(mise activate zsh)"

# === agent-worktree BEGIN ===
# NOTE: Don't use 'path'/'status' as variable names - zsh reserves them
wt() {
  local wt_bin path_file target_path wt_status wt_arg path_file_inserted
  local -a wt_args
  if [[ -n "$ZSH_VERSION" ]]; then
    wt_bin=$(whence -p wt 2>/dev/null)
  else
    wt_bin=$(type -P wt 2>/dev/null)
  fi
  if [[ -z "$wt_bin" ]]; then
    echo "wt: binary not found. Install: npm install -g agent-worktree" >&2
    return 1
  fi
  # Pass through if -h/--help anywhere in args
  case " $* " in
    *" -h "*|*" --help "*) "$wt_bin" "$@"; return ;;
  esac
  case "$1" in
    cd|new|rm|mv|merge|clean|run)
      # Use mktemp so concurrent calls (and subshells where $$ is the parent
      # PID) get unique files; fall back to PID-based name if mktemp missing.
      path_file=$(mktemp 2>/dev/null) || path_file="${TMPDIR:-/tmp}/wt-path-$$"
      # `wt run -- <agent>` treats every argument after `--` as belonging to
      # the agent, so inject the wrapper option before that delimiter.
      wt_args=()
      path_file_inserted=
      for wt_arg in "$@"; do
        if [[ "$wt_arg" == "--" && -z "$path_file_inserted" ]]; then
          wt_args+=(--path-file "$path_file")
          path_file_inserted=1
        fi
        wt_args+=("$wt_arg")
      done
      if [[ -z "$path_file_inserted" ]]; then
        wt_args+=(--path-file "$path_file")
      fi
      "$wt_bin" "${wt_args[@]}"
      wt_status=$?
      # -s guards the empty file mktemp created: cd only on a written target
      if [[ $wt_status -eq 0 && -s "$path_file" ]]; then
        target_path=$(<"$path_file"); cd "$target_path"
      fi
      rm -f "$path_file"
      return $wt_status
      ;;
    *)
      "$wt_bin" "$@"
      ;;
  esac
}
# Dynamic completions: call binary directly to bypass wt function
if [[ -n "$ZSH_VERSION" ]]; then
  _wt_bin=$(whence -p wt 2>/dev/null)
  [[ -n "$_wt_bin" ]] && source <(COMPLETE=zsh "$_wt_bin" 2>/dev/null) 2>/dev/null
else
  _wt_bin=$(type -P wt 2>/dev/null)
  [[ -n "$_wt_bin" ]] && source <(COMPLETE=bash "$_wt_bin" 2>/dev/null) 2>/dev/null
fi
unset _wt_bin
# === agent-worktree END ===
