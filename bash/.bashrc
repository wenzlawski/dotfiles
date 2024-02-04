export PATH=/usr/local/bin/:$PATH
export PATH=/usr/local/Cellar/recoll/1.35.0/recoll.app/Contents/MacOS:$PATH
export PATH=$HOME/.cargo/bin:$PATH

# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
. '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix

source /Users/mw/.config/broot/launcher/bash/br
. "$HOME/.cargo/env"
