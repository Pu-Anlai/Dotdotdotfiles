function imath -d "Use math interactively"
    set_color -o && echo -n "imath: " && set_color normal
    echo 'Type "exit" to quit.  "$$" will be replaced with the result of the previous operation.'

    set result

    while true
        read -p 'set_color blue; echo -n math; set_color normal; echo -n "> "' input
        if [ "$input" = "exit" ]
            return
        end

        set input (string replace '$$' "$result" -- "$input")
        math "$input" && set result (math "$input")
    end
end
