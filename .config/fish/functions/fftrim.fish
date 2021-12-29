function fftrim
    argparse -n fftrim -x 'l,r' -x 'l,s' -x 'r,t' -N 1 -X 1 'l/left' 'r/right' 's/ss=' 't/to=' -- $argv
    test -f "$argv" || return 1

    set vbase (string replace -r '\.[^.]+$' '' -- (basename "$argv"))
    set vext (string match -r '[^.]+$' -- (basename "$argv"))

    if [ -n "$_flag_l" -a -n "$_flag_t" ]
        ffmpeg -i "$argv" -ss "$_flag_t" -c copy "$vbase""_end"".$vext"
    else if [ -n "$_flag_r" -a -n "$_flag_s" ]
        ffmpeg -i "$argv" -to "$_flag_s" -c copy "$vbase""_start"".$vext"
    else
        return 1
    end
end
