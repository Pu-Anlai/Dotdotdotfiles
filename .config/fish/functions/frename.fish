function frename
    argparse --name='frename' 'a/all' 'n/dry-run' 'r/regex' 'i/ignore-case' 'I/non-interactive' -- $argv

    set mv_options "-i"
    [ -n "$_flag_I" ]; and set mv_options ""

    # validate input
    if [ ! (count $argv) -ge 3 ]
        echo "Required format: [OPTIONS] PATTERN REPLACEMENT FILE(S)"
        return 2
    end

    set -l pattern $argv[1]
    set -l replacement $argv[2]
    set -l file_list $argv[3..-1]
    # validate file input
    for file in $file_list
        if [ ! \( -f $file -o -d $file \) ]
            echo "$file is not a file or directory."
            return 2
        end
    end

    # do the renaming
    for file in $file_list
        set -l new_name (string replace $_flag_a $_flag_i $_flag_r -- $argv[1] $argv[2] "$file")
        if set -ql _flag_n
            echo "$file --> $new_name"
        else
            if [ "$file" != "$new_name" ]
                mv $mv_options "$file" "$new_name"
            end
        end
    end
end
