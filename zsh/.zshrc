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
fi

if [[ -f "$ZSH_PLUGIN_PREFIX/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ]]; then
  source "$ZSH_PLUGIN_PREFIX/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
fi

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

# Add shasum for microsandbox
export PATH=/usr/bin/core_perl:$PATH

# Added by microsandbox installer
export PATH="$HOME/.local/bin:$PATH"

# For nix-direnv
# source "$HOME/.nix-profile/share/nix-direnv/direnvrc"

# Add devenv to PATH
export PATH="$HOME/.nix-profile/bin:$PATH"

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
export EDITOR='nvim'
export VISUAL='nvim'

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

# Lazygit alias
alias lg='lazygit --use-config-dir ~/.config/lazygit'

# Lazydocker alias
alias ld='lazydocker'

# Docker summary alias (id, name, image)
alias dss='docker ps --format "{{.ID}}\t{{.Names}}\t{{.Image}}"'

# Added by ni-dev-tools setup - Go tools PATH
export PATH="$HOME/go/bin:$PATH"

# Initialize Starship prompt
eval "$(starship init zsh)"

# Initialize direnv (DIRENV_LOG_FORMAT="" suppresses the "direnv: export" line)
eval "$(direnv hook zsh)"

# bun completions
[ -s "/home/peter/.bun/_bun" ] && source "/home/peter/.bun/_bun"

# Use GPG agent for SSH authentication
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
