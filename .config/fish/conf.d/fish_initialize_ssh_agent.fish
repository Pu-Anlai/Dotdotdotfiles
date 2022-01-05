# if ssh isn't installed/set-up, do nothing
if not command -s ssh 1>&- 2>&-; or not test -e $HOME/.ssh
    return
end

if test -z "$SSH_ENV"
    set -xg SSH_ENV $HOME/.ssh/environment
end

if not __ssh_agent_is_started
    __ssh_agent_start
end
