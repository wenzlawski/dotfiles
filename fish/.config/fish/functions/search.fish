function search
  set -l engine $argv[1]
  set -l query (echo $argv[2..-1] | xargs -I{} jq -rn --arg x '{}' '$x|@uri')
  set -l url "https://www.$engine.com/search?q=$query"
  open $url
end

