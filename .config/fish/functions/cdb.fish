function cdb -d "Browse through directory bookmarks."
    argparse -n cdb 'a/add' 'd/delete' -- $argv

    if [ -n "$_flag_a" ]
        # add bookmark
        if [ (count $argv) -ne 1 ]
            echo "Wrong number of arguments."
            return 1
        end

        set -Ua FISH_CDB_BOOKMARKS (realpath $argv[1])
        return
    else if [ -n "$_flag_d" ]
        # delete bookmark
        if [ (count $argv) -ne 1 ]
            echo "Wrong number of arguments."
            return 1
        end

        set bm (realpath $argv[1])
        set index (contains -i $bm $FISH_CDB_BOOKMARKS)
        or begin
            echo "Bookmark $bm does not exist."
            return 1
        end
        
        set -Ue FISH_CDB_BOOKMARKS[$index]
        return
    end

    # no bookmarks?
    any $FISH_CDB_BOOKMARKS || return 1

    printf '%s\n' $FISH_CDB_BOOKMARKS | fzf --with-nth -1 -d "/" +m --preview="if [ -e {} ]; then echo -e \"\e[1;37m\$(basename {})\e[0m\"; ls {}; else echo \"Directory doesn't exist.\"; fi" | read directory
    [ -e "$directory" ] && cd "$directory"
end
