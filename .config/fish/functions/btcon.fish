function btcon
    argparse -n btcon -X 1 'n/new' 's/scan' 'l/list' -- $argv
    or return 1

    if not systemctl is-active bluetooth.service > /dev/null
        sudo systemctl start bluetooth.service
    end

    bluetoothctl power on 2>/dev/null 1>/dev/null

    # scan for devices before listing them
    if [ -n "$_flag_s" ]
        bluetoothctl scan on
        bluetoothctl scan off
        return
    end

    # list known devices
    if any $_flag_s $_flag_l
        echo "Available devices:"
        bluetoothctl devices 2>/dev/null
        return 1
    end

    # remove and re-pair specified device
    if [ -n "$_flag_n" ]
        bluetoothctl remove "$argv" 2>/dev/null 1>/dev/null

        set found false
        stdbuf -oL bluetoothctl scan on | while read line
            echo $line
            if string match -eq "$argv" "$line"
                set found true
                break
            end
        end
        bluetoothctl scan off
        test "$found" = "false"; and return

        bluetoothctl pair "$argv"
    end

    # connect to specified device
    bluetoothctl connect "$argv"
end
