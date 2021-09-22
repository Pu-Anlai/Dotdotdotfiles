whitelist ${HOME}/Sync/Diverses/Bookmarks
noblacklist ${HOME}/.config/mpv
whitelist ${HOME}/.config/mpv
noblacklist ${HOME}/.config/nvim
whitelist ${HOME}/.config/nvim
noblacklist ${HOME}/.config/alacritty
whitelist ${HOME}/.config/alacritty
noblacklist ${HOME}/.config/zathura
whitelist ${HOME}/.config/zathura
noblacklist /usr/lib
whitelist ${HOME}/.config/zathura

# allow lua for starting mpv from within qutebrowser
include /etc/firejail/allow-lua.inc
include /etc/firejail/qutebrowser.profile
