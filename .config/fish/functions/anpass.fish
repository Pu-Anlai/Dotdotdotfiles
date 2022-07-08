function anpass -d 'Send password from pass to a connected Android device' -w pass
    adb shell input text "$(pass show $argv | head -n 1 | string escape)"
end
