function glob
	set -l first_param_char (string sub -s 1 -l 1 -- $argv[1])
    set -l mode
	if test "$first_param_char" = '-'
        set mode (string sub -s 2 -- $argv[1])
    else
        return
    end

    # include a check for a hidden switch at some point
    set -l hidden -not -name '.*'
    set args $argv[2..(count $argv)]

    switch $mode
        case 'x'
            # match all except args
            set -l params
            for arg in $args
                set params $params -not -name {$arg}
            end
            find . -maxdepth 1 $params $hidden | sed -n 's/^\.\///p'
        case '*'
            return
    end
end
