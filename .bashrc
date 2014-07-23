#set prompt
# [green] username [white] @ [green] hostname [white] : [orange/red] path return $
PS1='\[\e[38;5;82m\]\u\[\e[00m\]@\[\e[38;5;82m\]\H\[\e[00m\]:\[\e[38;5;172m\]\w\[\e[00m\]\[\e[8;33;40m\]$(rc=$?; if [ $rc -ne 0 ]; then echo " $rc "; fi)\[\e[00m\]\$ '

dircolors >/dev/null

#set aliases
alias ls='ls --color=auto'
alias ll='ls -l --color=auto'
alias la='ls -la --color=auto'

alias grep='egrep --color=auto -n'

alias emacs='emacs -nw'

alias mocq='mocp -Q'
alias mocp='padsp mocp'

alias please="sudo \`fc -ln -1\`"

alias mail="mutt"

export TERM=xterm-256color
export EDITOR="~/bin/editor.sh"

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
