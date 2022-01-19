# if ssh isn't installed/set-up, do nothing
if command -s ssh 1>&- 2>&- && test -e "$HOME/.ssh"
    test -z "$SSH_ENV" && set -xg SSH_ENV $HOME/.ssh/environment
    not __ssh_agent_is_started && __ssh_agent_start
end
