function bootwin
    set -l win_entry (efibootmgr | grep "Windows Boot Manager" | cut -f1 -d\* | tr -d [:alpha:])
    command sudo efibootmgr -n $win_entry
    and command systemctl reboot
end
