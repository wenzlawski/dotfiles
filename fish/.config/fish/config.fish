#Base16 Shell
if status --is-interactive
# CLI inits
  set -gx ATUIN_NOBIND "true"
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

set -U fish_greeting

# Environment variables
set -gx XDG_CONFIG_HOME ~/.config
set -x PATH $PATH ~/.cargo/bin
set -x PATH $PATH $HOME/.cargo/env
set PATH $PATH ~/.config/emacs/bin
set -gx EDITOR "/usr/local/bin/emacsclient -nw"
set -gx VISUAL $EDITOR
set -x PATH $PATH $HOME/.local/bin
set -x PATH $PATH $HOME/.local/lib/python3.10/site-packages
set -x GOHOME $HOME/go
set -x PATH $PATH $GOHOME/bin
set -x PATH $PATH /usr/local/Cellar/recoll/1.35.0/recoll.app/Contents/MacOS/

set -x FZF_DEFAULT_COMMAND 'fd --type file --follow --hidden --exclude .git --color=always'
set -x FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND
set -x OPENAI_API_KEY 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


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
