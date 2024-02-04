function sshpi --wraps='ssh pi@raspberrypi.local' --description 'alias sshpi ssh pi@raspberrypi.local'
  ssh pi@raspberrypi.local $argv
        
end
