# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# ALIASES
alias l=lsd
alias ls=lsd
alias la="lsd -a"
alias lh="lsd -hla"
alias sshpi="ssh pi@raspberrypi.local"
alias jpn="jupyter notebook"
alias jpl="jupyter lab"
alias pjl="julia -e 'import Pluto; Pluto.run()'"
alias fpublish='julia -e "using Franklin; publish()"'
alias fserve='julia -e "using Franklin; serve()"'
alias n=nvim
alias c=z

# Sets up pyenv
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/marcwenzlawski/Downloads/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/marcwenzlawski/Downloads/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/marcwenzlawski/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/marcwenzlawski/Downloads/google-cloud-sdk/completion.zsh.inc'; fi

#if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
#  exec tmux
#fi
alias newmac="sudo /sbin/ifconfig en0 ether \`openssl rand -hex 6 | sed 's/\(..\)/\1:/g; s/.$//'\`"

export PATH=/usr/local/Cellar/ruby/3.2.1/bin:$PATH
export PATH=/usr/local/lib/ruby/gems/3.2.0/bin:$PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=/usr/local/Cellar/recoll/1.35.0/recoll.app/Contents/MacOS:$PATH
export PATH=$HOME/.cargo/bin:$PATH

eval "$(zoxide init zsh)"

# pnpm end

eval "$(atuin init zsh)"
eval "$(atuin init zsh)"

source /Users/mw/.config/broot/launcher/bash/br
