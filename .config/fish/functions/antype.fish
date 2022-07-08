function antype -d 'Send string input to a connected Android device'
    adb shell input text "$(string escape "$argv")"
end
