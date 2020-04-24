function chksum
    argparse -n 'chksum' 'b/binary=' -- $argv
    or return

    set -a _flag_binary md5sum
    
    if [ ! (count $argv) -eq 2  -o ! -f "$argv[-1]" ]
        echo "syntax: chksum [-b BINARY] CHECKSUM FILE"
        return
    end

    set filehash ($_flag_binary[1] "$argv[-1]" | awk '{print $1}')
    or return

    if [ "$filehash" = "$argv[-2]" ]
        echo "OK: Checksum matches." > /dev/stderr
        return 0
    else
        echo "FAIL: Checksum does not match." > /dev/stderr
        return 1
    end
end
