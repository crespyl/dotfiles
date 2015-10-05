source $BYOBU_PREFIX/share/byobu/profiles/tmux

# Default terminal
set -g default-terminal "screen"

# Update env variables to the existing "outer" env
set -g update-environment 'DISPLAY SSH_ASKPASS SSH_AGENT_PID SSH_CONNECTION WINDOWID XAUTHORITY TERM'

# Determine if we should enable 256 color support
if "[[ ${TERM} =~ 256color || ${TERM} == fbterm]]" 'set -g default-terminal screen-256color'

# Enable mouse support for scrolling
setw -g mode-mouse on

# Bind <prefix> u to urlscan
bind-key u capture-pane\; save-buffer /tmp/tmux-buffer \; new-window -n "urlscan" '$SHELL -c "urlscan < /tmp/tmux-buffer"'
