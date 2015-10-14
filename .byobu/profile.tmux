source $BYOBU_PREFIX/share/byobu/profiles/tmux

# Update env variables to the existing "outer" env
set -g update-environment 'DISPLAY SSH_ASKPASS SSH_AGENT_PID SSH_CONNECTION WINDOWID XAUTHORITY'

# Enable mouse support for scrolling and changing window/pane
setw -g mode-mouse on
set -g mouse-select-window on
set -g mouse-select-pane on

# Bind <prefix> u to urlscan
bind-key u capture-pane\; save-buffer /tmp/tmux-buffer \; new-window -n "urlscan" '$SHELL -c "urlscan < /tmp/tmux-buffer"'
