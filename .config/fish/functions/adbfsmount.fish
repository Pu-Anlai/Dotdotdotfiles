function adbfsmount --description "Provide a adbfs mountpoint as $adbfsmnt"
    set adbfs (findmnt -rnt fuse.adbfs | head -n1 | cut -d\  -f1)

    if [ -n "$adbfs" ]
        if set -qg adbfsmnt
            set_color -o; echo "Unmounting \$adbfsmnt ($adbfsmnt)..."; set_color normal
            fusermount -u "$adbfsmnt"
        else
            set_color -o; echo "Setting \$adbfsmnt to $adbfs..."; set_color normal
            set -g adbfsmnt "$adbfs"
        end

    else
        set -g adbfsmnt (mktemp -d --tmpdir 'adbfs.XXXXXX')
        set_color -o; echo "Mounting under $adbfsmnt (\$adbfsmnt)..."; set_color normal
        adbfs "$adbfsmnt"
        or set -eg adbfsmnt
    end
end
