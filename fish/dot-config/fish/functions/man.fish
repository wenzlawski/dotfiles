if vterm-p
    function man -d "run man in emacs"
        vterm_cmd man "$argv"
    end
end
