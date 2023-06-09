# Base16 Shell
if status --is-interactive
# CLI inits
  zoxide init fish | source
  atuin init fish | source
  starship init fish | source
  pyenv init - | source
  set BASE16_SHELL_PATH "$HOME/.config/base16-shell"
  if test -s "$BASE16_SHELL_PATH" -a "$TERM" = "screen-256color"
    source "$BASE16_SHELL_PATH/profile_helper.fish"
  end
end

# Aliases
alias ls=lsd
alias l=lsd
alias ll="lsd -l"
alias la="lsd -a"
alias lh="lsd -hla"
alias sshpi="ssh pi@raspberrypi.local"
alias jpn="jupyter notebook"
alias jpl="jupyter lab"
alias pjl="julia -e 'import Pluto; Pluto.run()'"
alias fpublish='julia -e "using Franklin; publish()"'
alias fserve='julia -e "using Franklin; serve()"'
alias n=nvim
alias newmac="sudo /sbin/ifconfig en0 ether \`openssl rand -hex 6 | sed 's/\(..\)/\1:/g; s/.\$//'\`"
alias nm=pnpm
alias c=clear
alias typing-practice="open https://climech.github.io/typing-practice/"
alias github="open https://github.com"
alias git-repos="open 'https://github.com/wenzlawski?tab=repositories'"
alias git-me="open 'https://github.com/wenzlawski'"
alias sb="search bing"
alias sg="search google"
alias h=z
alias brew="env PATH=(string replace (pyenv root)/shims '' \"\$PATH\") brew"
alias nvd="neovide"
alias g="git"
alias cat="bat"
alias zj="zellij"
alias alt="alacritty-themes"

function gc
  command git clone "$1"
end

alias tm="tmux attach || tmux new -s work"
alias gi="git init"
alias gs="git status"
alias gl="git log --stat --pretty=oneline --graph --date=short"
# alias gg="gitg &"
alias ga="git add --all"
alias gr="git remote"
alias gf="git fetch"
alias gpl="git pull"
alias gp="git push"
alias gpm="git push origin master"

abbr --add jrnl " jrnl"

set -U fish_greeting
set -gx ATUIN_NOBIND "true"

# Environment variables
set -gx XDG_CONFIG_HOME ~/.config
set -x PATH $PATH ~/.cargo/bin
set PATH $PATH ~/.config/emacs/bin
set -gx EDITOR /usr/local/bin/nvim
set -gx VISUAL $EDITOR
set -x PATH $PATH $HOME/.local/bin
set -x PATH $PATH $HOME/.local/lib/python3.10/site-packages


#pyenv virtualenv-init - | source

# bind to ctrl-r in normal and insert mode, add any other bindings you want here too
bind \cr _atuin_search
bind -M insert \cr _atuin_search

# pnpm
set -gx PNPM_HOME "/Users/mw/Library/pnpm"
if not string match -q -- $PNPM_HOME $PATH
  set -gx PATH "$PNPM_HOME" $PATH
end
# pnpm end
