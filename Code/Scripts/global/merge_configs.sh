#!/bin/bash

folder_list=( "$HOME/.config/i3"  "$HOME/.config/sway"  "$HOME/.config/i3blocks")
for folder in ${folder_list[@]}; do
    cd $folder
    if [[ ! -L config ]]; then
        echo -e "$(cat config.skel)\n\n# CUSTOM CONFIG\n\n$(cat custom)" > config
    fi
done
