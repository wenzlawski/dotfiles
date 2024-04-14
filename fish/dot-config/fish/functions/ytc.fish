function ytc -d "Get the channel id of a youtube account"
  curl $argv[1] | rg -Po -m1 'youtube.com\\/channel\\/([a-zA-Z1-9-]*)">' -r '$1' | tail -1
end
