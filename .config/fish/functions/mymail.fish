function mymail
    set -l current_dir (pwd)
    set -l mutt_dir "$HOME/Downloads/Mutt"

    mkdir -p $mutt_dir
    cd $mutt_dir

    mbsync -a
    notmuch new 2> /dev/null
    neomutt
    and mbsync -a

    cd $current_dir
end
