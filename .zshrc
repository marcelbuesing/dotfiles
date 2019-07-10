# Path to your oh-my-zsh installation.
export ZSH=/home/marcel/.oh-my-zsh
export GOPATH=/home/marcel/go

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="ys"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(ssh-agent)

# User configuration

path+=('/home/marcel/.cabal/bin')
# export PATH="/home/marcel/.cabal/bin"
# export MANPATH="/usr/local/man:$manpathzs"

# Locate NPM in home path
NPM_PACKAGES="$HOME/.npm-packages"
path+=('$NPM_PACKAGES/bin')
path+=('$(ruby -e 'print Gem.user_dir')/bin')
path+=('/home/marcel/haskell-setup/haskell-on-arm/bin')
path+=('/home/marcel/go/bin')
path+=('/home/marcel/.cargo/bin')
path+=('/home/marcel/.cabal/bin')
path+=('/home/marcel/.local/bin')
path+=('./yarn/bin')
path+=('~/.platformio/penv/bin')
path+=('~/.jx/bin')
path+=('$HOME/bin')
path+=('$HOME/.gem')
path+=('/home/marcel/.gem/ruby/2.6.0/bin')


#MANPATH="$NPM_PACKAGES/share/man:$(manpath)"
# Tell Node about these packages
NODE_PATH="$NPM_PACKAGES/lib/node_modules:$NODE_PATH"
ANDROID_HOME="/home/marcel/Android/Sdk/"
RUST_SRC_PATH="/home/marcel/.multirust/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"
RUSTC_WRAPPER=sccache
GOPATH="/home/marcel/go"
ESPIDF="/opt/esp-idf-sdk"
source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# sway
export XKB_DEFAULT_LAYOUT=de
# export XKB_DEFAULT_VARIANT=nodeadkeys

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias nixpkgs="nix-env -qa \* -P | fgrep -i"
# git
alias gco="git checkout"
alias ls=exa
alias cat=bat
alias find=fd
alias c=cargo
# added by travis gem
[ -f /home/marcel/.travis/travis.sh ] && source /home/marcel/.travis/travis.sh
#source $HOME/.nvm/nvm.sh
