function bootwin
    set -l win_entry (efibootmgr | perl -ne 'print if s/^Boot(\d+).*Windows Boot Manager/\1/')
    command sudo efibootmgr -n $win_entry
    command systemctl reboot
end
