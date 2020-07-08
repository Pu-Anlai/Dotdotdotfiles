function cdp -d "Browse through the file system with fzf."
    set orig_destination (pwd)
    true
    while true
        printf "%s\n" .. * | fzf --preview="echo -e \"\e[1;37m\"{}\"\e[0m\"; if [ -d {} ]; then ls {}; elif grep -qI '' {}; then head -n \$((\$FZF_PREVIEW_LINES - 1)) {}; else file {} | fold -s -w \$FZF_PREVIEW_COLUMNS; fi" \
                              --bind="ctrl-c:execute(cd \"$orig_destination\")+abort" | read destination
        if [ $status -eq 0 ]
            cd $destination
        else
            break
        end
    end
end
