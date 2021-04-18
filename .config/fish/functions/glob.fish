function glob
    argparse -n 'glob' -x 'b,c,d,f,g,G,L,O,p,r,s,S,t,u,w,x' 'h/hidden' 'X/except=+' 'b/block' 'c/character' 'd/directory' 'g/group' 'G/Group' 'f/file' 'L/Link' 'O/Owned' 'p/pipe' 'r/readable' 's/size' 'S/Socket' 't/terminal' 'u/user' 'w/writable' 'x/xecutable' -- $argv
    or return 1

    # build list of files
    set -l all_files
    if [ -z "$argv" ]
        set all_files *
    else
        set all_files $argv
    end

    if [ "$_flag_hidden" ]
        set -a all_files .*
    end

    set -l flag
    # trim flags so we can use them as operators
    for f in $_flag_b $_flag_c $_flag_d $_flag_f $_flag_g $_flag_G $_flag_L $_flag_O $_flag_p $_flag_r $_flag_s $_flag_S $_flag_t $_flag_u $_flag_w $_flag_x
        test -n "$f"; and set flag "$f"; or continue
        if [ (string length -- "$flag") -gt 2 ]
            set flag (string sub -s 2 -l 2 -- $flag)
            break
        end
    end

    # loop through all_files and print those that match
    for f in $all_files
        # skip for any exception defined on the command line
        set -l except ""
        for x in $_flag_except
            string match -q "$x" (basename "$f"); and set except "$f"; and break
        end
        test "$except"; and continue

        if [ $flag "$f" -a -e "$f" ]
            printf '%s\n' "$f"
        end
    end
end
