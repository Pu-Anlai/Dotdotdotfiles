function xtr -d "Extract files, create a base directory if the archive does not include one."
    if not command -q 7z
        echo "p7zip not installed."
        return 1
    end

    7z l -slt $argv[1] | \
        awk -F " = " '/^Path / {print $2}' | \
        tail -n +2 | cut -d/ -f1 | sort -u | \
        wc -l | read folder_count

    if [ $folder_count -gt 1 ]
        set base_folder (string replace -r '\.[^.]+' '' -- "$argv[1]")
        7z x -o"$base_folder" $argv[1]
    else
        7z x $argv[1]
    end
end
