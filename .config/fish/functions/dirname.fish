function dirname -d 'Remove trailing filename from path'
    if [ (string sub -s -1 -- "$argv") = "/" ]
        echo "$argv"
    else
        command dirname $argv
    end
end
