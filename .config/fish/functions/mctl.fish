function mctl
    # start mpdscribble if it isn't running
    if not pgrep mpdscribble >/dev/null
        mpdscribble --conf ~/.config/mpdscribble.conf
    end

    ncmpcpp
end
