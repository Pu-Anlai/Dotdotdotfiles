function aurmake -w cower -d 'Build specified AUR package'
    test (count $argv) -eq 1 || return 1

    set pkg $argv[1]
    set aur_rpc_info "https://aur.archlinux.org/rpc/v5/info?arg[]="
    set response (curl "$aur_rpc_info$pkg" 2>/dev/null) || return 1
    set resultcount (echo "$response" | jq .resultcount) || return 1

    switch $resultcount
        case 0
            echo "Package $pkg not found in AUR." >&2
            return 1
        case 1
            :
        case '*'
            echo -e "More than one RPC result found for package $pkg.\nWhat's going on?"
            return 1
    end

    for dep in (echo "$response" | jq '.results[0].Depends.[]')
        if pacman -Si $dep >/dev/null 2>&1
            echo "Package $dep not in pacman repos. Trying AUR..." >&2
            aurmake $dep || return 1
        end
    end

    set build_dir (mktemp -d)
    set orig_dir (pwd)
    cd $build_dir
    aurclone -d "build_$pkg" $pkg || return 1
    cd "build_$pkg"
    makepkg -sri || return 1
    # only remove $build_dir if build was succesful, otherwise we might be able
    # to still use the contents of the directory
    rm -rf $build_dir || return 1
end
