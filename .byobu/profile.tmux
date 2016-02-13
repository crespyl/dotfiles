source $BYOBU_PREFIX/share/byobu/profiles/tmux

# set the default TERM
set -g default-terminal screen

# update the TERM variable of terminal emulator when creating a new session or attaching a existing session
set -g update-environment 'DISPLAY SSH_ASKPASS SSH_AGENT_PID SSH_CONNECTION WINDOWID XAUTHORITY TERM'
# determine if we should enable 256-colour support
if "[[ ${TERM} =~ 256color || ${TERM} == fbterm ]]" 'set -g default-terminal screen-256color'

# Update env variables to the existing "outer" env
set -g update-environment 'DISPLAY SSH_ASKPASS SSH_AGENT_PID SSH_CONNECTION WINDOWID XAUTHORITY'

# Enable mouse support for scrolling and changing window/pane
set -g mouse

# Bind <prefix> u to urlscan
bind-key u capture-pane\; save-buffer /tmp/tmux-buffer \; new-window -n "urlscan" '$SHELL -c "urlscan < /tmp/tmux-buffer"'

# Bind <prefix> p to fpp
bind f run "sh -s fpp #{pane_id}"
