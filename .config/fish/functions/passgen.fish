function passgen -a filename
    if [ -z "$filename" ]
        echo "Specify a password file name." >&2
        return 1
    end

    set counter 1
    set pass_in (randstring)
    while true
        read -P "$(set_color -o)Enter line $counter$(set_color normal): " input
        test -z "$input" && break
        set -a pass_in $input
        set counter (math $counter + 1)
    end
    printf "%s\n" $pass_in | pass insert -m $filename
end
