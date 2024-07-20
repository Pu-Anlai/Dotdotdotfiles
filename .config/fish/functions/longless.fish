function longless -d "Pipe to less unless output fits the screen"
    cat | while read line
        set -a output $line
    end

    # leave an extra line to account for the prompt
    if [ (count $output) -gt (math $LINES - 1) ]
        printf '%s\n' $output | less $argv
    else
        printf '%s\n' $output
    end
end
