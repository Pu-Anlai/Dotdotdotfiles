function watch -d "Watch a video through a detached mpv instance"
    away mpv $argv 1>/dev/null
end
