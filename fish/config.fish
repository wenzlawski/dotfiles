if not status --is-interactive
	exit
end
# Aliases
alias l=lsd
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
alias ch=z
alias brew="env PATH=(string replace (pyenv root)/shims '' \"\$PATH\") brew"
alias nvd="neovide"
alias g="git"

abbr --add jrnl " jrnl"

set -U fish_greeting
set -gx ATUIN_NOBIND "true"

# Environment variables
set -gx XDG_CONFIG_HOME ~/.config
set -x PATH $PATH ~/.cargo/bin
set PATH $PATH ~/.config/emacs/bin
set -gx EDITOR /usr/local/bin/nvim


# CLI inits
zoxide init fish | source
atuin init fish | source
starship init fish | source
pyenv init - | source
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
