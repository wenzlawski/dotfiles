function gopen --wraps='git remote get-url origin | xargs open' --description 'alias gopen git remote get-url origin | xargs open'
  git remote get-url origin | xargs open $argv
        
end
