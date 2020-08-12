function colortest
    argparse 's/start=?!_validate_int --min 0 --max 15' 't/to=?!_validate_int --min 0 --max 15' -- $argv
    
    [ -z "$_flag_s" ] && set _flag_s 0
    [ -z "$_flag_t" ] && set _flag_t 15
    
    set width (seq "$_flag_s" "$_flag_t")

    for i in $width
        printf 'BASE0%X ' $i
    end

    for i in (seq 0 15)
        set fg __BASE0(printf '%X' $i)
        echo

        for k in $width
            set bg __BASE0(printf '%X' $k)
            set_color $$fg -b $$bg
            printf 'BASE0%X' $i
            set_color normal
            echo -n ' '
        end

        echo
    end
end
