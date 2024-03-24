#Base16 Shell
if status --is-interactive
    # CLI inits
    set -gx ATUIN_NOBIND true
    zoxide init fish | source
    atuin init fish | source
    starship init fish | source
    pyenv init - | source
    # set BASE16_SHELL_PATH "$HOME/.config/base16-shell"
    # set BASE16_SHELL "$HOME/.config/base16-shell"
    # if test -s "$BASE16_SHELL_PATH" -a "$TERM" = "screen-256color"
    # if test -s "$BASE16_SHELL_PATH"
    #   source "$BASE16_SHELL_PATH/profile_helper.fish"
    # end
end

abbr --add jrnl " jrnl"

# Path variables
fish_add_path ~/.cargo/bin
fish_add_path ~/.cargo/env
fish_add_path ~/.config/emacs/bin
fish_add_path ~/.local/bin
#fish_add_path ~/.local/lib/python3.10/site-packages
fish_add_path $GOHOME/bin
fish_add_path /usr/local/Cellar/recoll/1.35.0/recoll.app/Contents/MacOS/
fish_add_path "$VOLTA_HOME/bin"

# Environment variables
set -x EDITOR "/usr/local/bin/emacsclient -nw"
set -x VISUAL $EDITOR
set -x GOHOME $HOME/go
set -x XDG_CONFIG_HOME ~/.config
set -x VOLTA_FEATURE_PNPM 1
set -x VOLTA_HOME "$HOME/.volta"

# FZF plugin config
set -x FZF_DEFAULT_COMMAND 'fd --type file --follow --hidden --exclude .git --color=always'
set -x FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND

# bind to ctrl-r in normal and insert mode, add any other bindings you want here too
bind \cr _atuin_search
bind -M insert \cr _atuin_search

# pnpm
set -gx PNPM_HOME /Users/mw/Library/pnpm
if not string match -q -- $PNPM_HOME $PATH
    set -gx PATH "$PNPM_HOME" $PATH
end
# pnpm end

# Vterm config (TODO: make loading conditional on emacs)
function vterm_printf
    if begin
            [ -n "$TMUX" ]; and string match -q -r "screen|tmux" "$TERM"
        end
        # tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end

function vterm_prompt_end
    vterm_printf '51;A'(whoami)'@'(hostname)':'(pwd)
end

functions --copy fish_prompt vterm_old_fish_prompt
function fish_prompt --description 'Write out the prompt; do not replace this. Instead, put this at end of your file.'
    # Remove the trailing newline from the original prompt. This is done
    # using the string builtin from fish, but to make sure any escape codes
    # are correctly interpreted, use %b for printf.
    printf "%b" (string join "\n" (vterm_old_fish_prompt))
    vterm_prompt_end
end
