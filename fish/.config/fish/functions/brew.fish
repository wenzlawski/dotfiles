function brew --description alias\ brew\ env\ PATH=\(string\ replace\ \(pyenv\ root\)/shims\ \'\'\ \"\$PATH\"\)\ brew
  env PATH=(string replace (pyenv root)/shims '' "$PATH") brew $argv
        
end
