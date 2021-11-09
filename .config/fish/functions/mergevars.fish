function mergevars -S
    argparse -n mergevar 'q/quiet' 'i/ignore=+' -- $argv

    set -l infile $argv
    if [ ! -f $infile ]
        echo -e "\e[1m$infile\e[0m is not a file." >&2
        return 1
    end

    # get all marked fields in the file
    set -l vars
    for match in (string match -ra '\{\{\{[^\}]+\}\}\}' < $infile)
        set -l v (string trim -c '{}' -- $match)

        # check if ignored
        if not contains $v $_flag_i
            # check if var is set
            if not set -q $v
                echo -e "Variable \e[1m$v\e[0m for file \e[1m$infile\e[0m not specified." >&2
                return 1
            end

            set vars $vars $v
        end
    end

    # abort if there are no fields
    if not count $vars > /dev/null; and test -z "$_flag_q"
        echo -e "No variable fields in file \e[1m$infile\e[0m or all relevant fields ignored." >&2
    end

    # do the actual replacing
    set -l file_string (cat $infile)
    for v in $vars
        set file_string (string replace -- '{{{'$v'}}}' $$v $file_string)
    end

    # print the result
    string join -- \n $file_string

end
