if vterm-p
    function man -d "run woman in emacs"
        vterm_cmd woman "$argv"
    end
end
