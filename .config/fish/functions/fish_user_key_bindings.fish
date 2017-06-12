function fish_user_key_bindings
    fish_default_key_bindings -M insert
	bind -M insert \ek up-or-search
    bind -M insert \ej down-or-search
    bind -M insert \el accept-autosuggestion

    # use escape to get to normal mode
    bind -M insert -m default \e backward-char force-repaint
    bind -M visual -m default \e backward-char force-repaint
    bind -M replace-one -m default \e backward-char force-repaint
end