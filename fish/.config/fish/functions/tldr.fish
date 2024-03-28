if vterm-p
    function tldr -d "Display tldr pages in an emacs buffer"
        vterm_cmd tldr "$argv"
    end
end
