function __fish_bell_on_completion_handle --on-event fish_postexec
    if test $CMD_DURATION -ge 20000
        printf "\a"
    end
end
