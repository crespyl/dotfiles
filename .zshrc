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
EDITOR='nano'
VISUAL='nano'
BROWSER='elinks'

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

#
# Command Aliases
#

alias dh='dirs -v'
alias ls='ls -F --color=always'
alias ll='ls -lF --color=always'
alias la='ls -aF --color=always'

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

# add ~/bin/zsh/ to fpath, enable prompt theme feature
fpath=(~/bin/zsh $fpath)
autoload -U promptinit && promptinit
prompt crespyl
