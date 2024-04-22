# Base16 Shell
if status --is-interactive
    # CLI inits
    zoxide init fish | source
    #atuin init fish | source
    pyenv init - | source
end

# Environment variables
set -x EDITOR "/usr/local/bin/emacsclient -nw"
set -x VISUAL $EDITOR
set -x GOHOME ~/go
set -x XDG_CONFIG_HOME ~/.config
set -x VOLTA_FEATURE_PNPM 1
set -x VOLTA_HOME ~/.volta
set -gx PNPM_HOME ~/Library/pnpm
set -gx GPG_TTY $tty

# Path variables

fish_add_path /usr/local/opt/make/libexec/gnubin
fish_add_path ~/.cargo/bin
fish_add_path ~/.cargo/env
fish_add_path ~/.config/emacs/bin
fish_add_path ~/.local/bin
#fish_add_path ~/.local/lib/python3.10/site-packages
fish_add_path $GOHOME/bin
fish_add_path /usr/local/Cellar/recoll/1.35.0/recoll.app/Contents/MacOS
fish_add_path $VOLTA_HOME/bin
fish_add_path $PNPM_HOME
fish_add_path ~/.pyenv/shims
fish_add_path /Library/TeX/texbin
fish_add_path /usr/local/opt/gnupg@2.2/bin
