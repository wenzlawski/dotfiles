function gl --wraps='git log --stat --pretty=oneline --graph --date=short' --description 'alias gl git log --stat --pretty=oneline --graph --date=short'
  git log --stat --pretty=oneline --graph --date=short $argv
        
end
