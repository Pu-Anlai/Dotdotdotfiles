function watch -d "Watch a video through a detached mpv instance" -w mpv
    away mpv $argv 1>/dev/null
end
