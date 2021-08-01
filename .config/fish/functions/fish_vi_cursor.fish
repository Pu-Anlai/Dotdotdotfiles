function fish_vi_cursor --on-variable fish_bind_mode --on-event fish_prompt
    if [ $fish_bind_mode = "insert" ]
        echo -ne '\033[0 q'
    else
        echo -ne '\033[1 q'
    end
            
end
