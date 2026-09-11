function arch-pkg-clean
    argparse 'v/verbose' 'i/interactive' 'k/keep=!_validate_int --min 1' 'd/directory' -- $argv
    set -q _flag_keep || set _flag_keep 3
    set -q _flag_directory || set _flag_directory /var/cache/pacman/*
    set keep -$_flag_keep

    for d in $_flag_directory
        test -d $d || continue

        for f in (string split -r -m3 -f1 -- "-" $d/* | sort -u)
            set versions (string match -gr '('$f'(?:-[^\-]+){3}.pkg)' $f* | sort -uV)
            test (count $versions) -le $_flag_keep && continue

            # for interactive mode we need to retain the files we're gonna keep
            # to print them to the terminal
            set -q _flag_interactive && set keepers $versions[$keep..]
            set -e versions[$keep..]

            if set -q _flag_interactive
                set answer
                while [ "$answer" != "y" -o "$answer" != "n" ]
                    printf "-> %s\n" $versions*
                    printf "   %s\n" $keepers*
                    read -n1 -p 'set_color -o; echo -n "Delete the marked files? "; set_color normal; echo -n "[y/n] "' answer || return 1

                    if [ "$answer" = "y" ]
                        sudo rm $_flag_v $versions* || return 1
                        break
                    else if [ "$answer" = "n" ]
                        break
                    end
                end
            else
                sudo rm $_flag_v $versions* || return 1
            end

        end
    end
end
