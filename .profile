export FZF_DEFAULT_OPTS="
--height 60% --border
--color=bg+:-1,bg:-1,spinner:#${__BASE0C},hl:#${__BASE0D},hl+:#${__BASE0D},fg:#${__BASE04},fg+:#${__BASE07}
--color=header:#${__BASE0D},info:#${__BASE0A},pointer:#${__BASE0C},marker:#${__BASE0C},prompt:#${__BASE09}
--bind=alt-j:down,alt-k:up --reverse
"
export FZF_DEFAULT_COMMAND="command find -P \$dir -mindepth 1 \( -regex '\.?/snp' -o -path '*/Steam' -o -path '*/.cache' -o -path '*/.git' \) -prune -o -print 2>/dev/null"
export FZF_OVERLAY_OPTS="--no-border --margin 10%,8% --no-height --layout reverse-list"
export FZF_ALT_C_COMMAND=$FZF_DEFAULT_COMMAND
export FZF_CTRL_T_COMMAND=$FZF_DEFAULT_COMMAND
