function plctl
    # start mpd if it isn't running
    if not systemctl --user is-active mpd.service >/dev/null
        systemctl --user start mpd.service
    end

    # start mpdscribble if it isn't running
    if not pgrep mpdscribble >/dev/null
        mpdscribble --conf ~/.config/mpdscribble.conf
    end

    command playlistctl
end
