function aurclone
    argparse 'd/directory=' -- $argv
    if set -q _flag_directory
        git clone "https://aur.archlinux.org/$argv[1].git" $_flag_directory
    else
        git clone "https://aur.archlinux.org/$argv[1].git"
    end
end
