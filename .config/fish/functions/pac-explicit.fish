function pac-explicit
    set -lx LANG C
    set -l pacs (pacman -Qetq)
    set -l explicits
    for pac in $pacs
        contains $pac $PACEXPLICIT_BLACKLIST && continue
        if not string match -qr '^Required By.+?\bbase\b' (pacman -Qi $pac)
            set -a explicits $pac
        end
    end
    if [ (count $explicits) -gt 0  ]
        string join \n $explicits
        return 0
    else
        return 1
    end
end
