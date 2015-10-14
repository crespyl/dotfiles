#!/usr/bin/zsh

##########################################################
# This file is evaluated once per login shell, and       #
# and is used to set general preferences and environment #
# variables.                                             #
##########################################################

#
# General Environment Variables
#

# Set EDITOR, VISUAL, and BROWSER
EDITOR=~/bin/editor.sh
VISUAL=~/bin/editor.sh
BROWSER=elinks

# Set PATH to include ~/bin/
PATH=$PATH:~/bin

#
# Configure ZSH specific options
#

# WORDCHARS defines which special characters are treated
# as part of a word for ZLE navigation purposes.
# The default setting happens to include '/' which bothers
# me
WORDCHARS='*?[]~=&;!#$%^(){}<>'

# DIRSTACKSIZE is used in conjunction with ZSH's pushd
# features, and controls the maximum size of the directory
# history stack
DIRSTACKSIZE=10

# If FBTERM is defined, set the terminal type to fbterm
if [[ -n $FBTERM ]] then
   TERM=fbterm
fi

#
# Command Aliases
#

alias e='$EDITOR '
alias dh='dirs -v'
alias ls='ls -F --color=always'
alias ll='ls -lF --color=always'
alias la='ls -aF --color=always'
alias fbt='FBTERM=1 ~/bin/fbterm-bi ~/.tty-wallpaper'

# this makes less pass through color escape codes
alias less='less -R'

#
# ZSH specific features and options
#

# pushd features, autopushd makes cd act like pushd
setopt autopushd pushdminus pushdsilent pushdtohome

# basic color support
autoload -U colors && colors

# completion, also enable completion for aliases
autoload -U compinit && compinit
setopt completealiases

# add support for some fancier prompt features
setopt prompt_bang
setopt prompt_subst
setopt prompt_percent

# use the syntax highlighting plugin, if it's available
if [[ -f /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]
then
	source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

# use k, if present (https://github.com/rimraf/k)
if [[ -f ~/bin/zsh/k.sh ]]
then
    source ~/bin/zsh/k.sh
fi

# if .dircolors is present, read that to define colors used by ls
if [[ -f ~/.dircolors ]]
then
	source ~/.dircolors
fi

# if using tmux, check for 256color support and set terminal type
if [[ -n $TMUX && `tmux show-environment -g TERM` =~ "256color" ]]
then
	export TERM=screen-256color
fi

# add ~/bin/zsh/ to fpath, enable prompt theme feature
fpath=(~/bin/zsh $fpath)
autoload -U promptinit && promptinit
prompt crespyl

# enable prompt auto-refresh, for the clock
TMOUT=1
TRAPALRM() {
	zle && zle reset-prompt
}
