function bootwin
    set -l win_entry (efibootmgr | perl -ne 'print if s/^Boot(\d+).*Windows Boot Manager/\1/')
    command sudo efibootmgr -n $win_entry
    and command systemctl reboot
end
