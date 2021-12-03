function plctl
    # start mpd if it isn't running
    if not systemctl --user is-active mpd.service >/dev/null
        systemctl --user start mpd.service
    end

    command playlistctl
end
