# Base16 Shell
if status --is-interactive
    # CLI inits
    set -gx ATUIN_NOBIND true
    zoxide init fish | source
    atuin init fish | source
    #starship init fish | source
    pyenv init - | source
    # set BASE16_SHELL_PATH "$HOME/.config/base16-shell"
    # set BASE16_SHELL "$HOME/.config/base16-shell"
    # if test -s "$BASE16_SHELL_PATH" -a "$TERM" = "screen-256color"
    # if test -s "$BASE16_SHELL_PATH"
    #   source "$BASE16_SHELL_PATH/profile_helper.fish"
    # end
end

# Environment variables
set -x EDITOR "/usr/local/bin/emacsclient -nw"
set -x VISUAL $EDITOR
set -x GOHOME ~/go
set -x XDG_CONFIG_HOME ~/.config
set -x VOLTA_FEATURE_PNPM 1
set -x VOLTA_HOME ~/.volta
set -gx PNPM_HOME ~/Library/pnpm

# Path variables
fish_add_path ~/.cargo/bin
fish_add_path ~/.cargo/env
fish_add_path ~/.config/emacs/bin
fish_add_path ~/.local/bin
#fish_add_path ~/.local/lib/python3.10/site-packages
fish_add_path $GOHOME/bin
fish_add_path /usr/local/Cellar/recoll/1.35.0/recoll.app/Contents/MacOS/
fish_add_path "$VOLTA_HOME/bin"
fish_add_path $PNPM_HOME


# FZF plugin config
set -x FZF_DEFAULT_COMMAND 'fd --type file --follow --hidden --exclude .git --color=always'
set -x FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND

# bind to ctrl-r in normal and insert mode, add any other bindings you want here too
bind \cr _atuin_search
bind -M insert \cr _atuin_search
