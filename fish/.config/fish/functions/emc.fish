function emc --wraps='emacsclient -n -c' --wraps='emacsclient -nc' --description 'alias emc emacsclient -nc'
  emacsclient -nc $argv
end
