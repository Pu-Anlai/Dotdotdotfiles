function __fish_draw_git_prompt_seg -a content --description 'Put together a git segment that changes color if the repo is not in a clean state'
    set -l git_state (command git status --porcelain 2> /dev/null)
    if test -z "$git_state"
        __fish_draw_second_prompt_seg $BASE07 ""$content
    else
        __fish_draw_second_prompt_seg $BASE0E ""$content
    end
end
