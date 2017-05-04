#!/bin/bash

# This script displays the contents of the clipboard, cut down to the first 20
# characters. If the clipboard is empty, nothing is shown. Whenever the
# blocklet is clicked (with any mouse button), the clipboard is emptied and the
# blocklet disappears.

if [[ -e /usr/bin/xsel ]]; then
    function clip_out {
        xsel -bo
    }
    function clip_clear {
        xsel -bc
    }
elif [[ -e /usr/bin/xclip ]]; then
    function clip_out {
        xclip -selection clipboard -o
    }
    function clip_clear {
        echo -n "" | xclip -selection clipboard -i
    }
else
    exit
fi

if [[ -n ${BLOCK_BUTTON} ]]; then
    clip_clear
else
    clipcontent=$(clip_out)
    [[ -z $clipcontent ]] && exit
    # format string and escape pango (=html) special chars
    clipcontent=$(echo $clipcontent | head -n 1)

    if [[ ${#clipcontent} -gt 20 ]]; then
        clipcontent=$(echo "${clipcontent:0:20}…" | sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g')
    else
        clipcontent=$(echo $clipcontent | sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g')
    fi

    echo "<span font_size='small'>${clipcontent}</span>"
fi
