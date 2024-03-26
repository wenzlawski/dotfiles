if vterm-p
    function vfo -d "vterm test function"
        vterm_cmd find-file-other-window (realpath "$argv")
    end
end
