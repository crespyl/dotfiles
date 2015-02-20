#!/usr/bin/zsh

# Options
WORDCHARS='*?[]~=/&;!#$%^(){}<>'

DIRSTACKSIZE=10
setopt autopushd pushdminus pushdsilent pushdtohome

# Prompt stuff
autoload -U compinit && compinit
autoload -U colors && colors
autoload -U add-zsh-hook

source ~/bin/zsh/spectrum.zsh
source ~/bin/zsh/prompt.zsh

# Aliases
alias ls='ls -F --color=always'
alias ll='ls -lF'
alias la='ls -aF'

alias less='less -R'

alias grep='grep --color=always'

# Syntax highlighting
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh