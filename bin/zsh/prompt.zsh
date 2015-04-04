#!/usr/bin/zsh

setopt PROMPT_BANG
setopt PROMPT_SUBST
setopt PROMPT_PERCENT

# Helper functions
function git_branch {
    git status >/dev/null 2>/dev/null && git symbolic-ref --short HEAD 2>/dev/null
}

function git_status {
    gitstatus=$(git status -s 2>/dev/null)
    s=''
    if [[ -n $gitstatus ]]; then
        if [[ $(echo $gitstatus | grep -c "^ M") > 0 ]]; then
            clean=0
            s=$(echo -en "$s$FX[bold]$fg[red]\u25CF")
        fi
        if [[ $(echo $gitstatus | grep -c "^A") > 0 ]]; then
            clean=0
            s=$(echo -en "$s$FX[bold]$fg[green]\u25CF")
        fi
        if [[ $(echo $gitstatus | grep -c "^?") > 0 ]]; then
            clean=0
            s=$(echo -en "$s$FX[bold]$fg[yellow]\u25CF")
        fi
        if [[ -n $s ]]; then
            echo ":$s"
        fi
    fi
}

function git_state {
    branch=$(git_branch)
    if [[ -n $branch ]]; then
        echo -n "$FG[110]$branch$(git_status) "
    fi
}

# copied from Pure zsh prompt, https://github.com/sindresorhus/pure
function prompt_pure_human_time() {
    local tmp=$1
    local days=$(( tmp / 60 / 60 / 24 ))
    local hours=$(( tmp / 60 / 60 % 24 ))
    local minutes=$(( tmp / 60 % 60 ))
    local seconds=$(( tmp % 60 ))
    (( $days > 0 )) && echo -n "${days}d "
    (( $hours > 0 )) && echo -n "${hours}h "
    (( $minutes > 0 )) && echo -n "${minutes}m "
    echo "${seconds}s "
}

function prompt_pure_preexec() {
    cmd_timestamp=$(date +%s)
}

function prompt_pure_precmd() {
    if [[ -n "$cmd_timestamp" ]]; then
        local stop=$(date +%s)
        local start=${cmd_timestamp:-$stop}
        elapsed=$stop-$start
        if [[ $elapsed -gt 5 ]]; then
            prompt_cmd_runtime=$(prompt_pure_human_time $elapsed)
        else
            prompt_cmd_runtime=''
        fi
        cmd_timestamp=''
    elif [[ -n "$prompt_cmd_runtime" ]]; then
        prompt_cmd_runtime=''
    fi
}

# Set zsh hooks
add-zsh-hook preexec prompt_pure_preexec
add-zsh-hook precmd prompt_pure_precmd

# Set variables
case "$USER" in
    peter|pjacobs|jacopa01)
        usercolor=$fg[green]
        terminator=❯
        ;;
    root)
        usercolor=$fg[red]
        terminator='#'
        ;;
    *)
        usercolor=$FX[bold]$fg[yellow]
        terminator='$'
        ;;
esac
if [[ -z "$HOST" ]]; then
    HOST=$HOSTNAME
fi

case "$HOST" in
    galatea)
        hostcolor=$FG[042]
        ;;
    crespyl.net)
        hostcolor=$FG[025]
        ;;
    jonah)
        hostcolor=$FG[201]
        ;;
    ambassador-enterprises.net)
        hostcolor=$FG[214]
        ;;
    *)
        hostcolor=$fg[yellow]
        ;;
esac

prompt_char_normal="%{$FX[bold]%}%(?.%{$fg[green]%}$terminator.%{$fg[red]%}$terminator"
prompt_char_root="%{$FX[bold]$fg[red]%}$terminator"
# only show user@host if logged in over ssh
[[ "$SSH_CONNECTION" != '' ]] && prompt_userhost="[ %{$usercolor%}%n%{$reset_color%}@%{$hostcolor%}%m%{$reset_color%} ]"

# Fallback to basic prompt if term doesn't support fancy colors
if [[ "$TERM" =~ .*256color.* ]] || [[ "$TERM" =~ .*konsole.* ]]
then
    export PROMPT='╭─( %{${fg[cyan]}%}%~ $(git_state)%{$fg[yellow]%}$prompt_cmd_runtime%{$reset_color%})
╰$prompt_userhost%(?..%{$FG[220]%}(%?%))%(!.$prompt_char_root.$prompt_char_normal)%{$reset_color%} '
else
    terminator='>'
    prompt_char_normal="%{$FX[bold]%}%(?.%{$fg[green]%}$terminator.%{$fg[red]%}$terminator"
    prompt_char_root="%{$FX[bold]$fg[red]%}$terminator"
    export PROMPT='/─( %{${fg[cyan]}%}%~ $(git_state)%{$fg[yellow]%}$prompt_cmd_runtime%{$reset_color%})
\\$prompt_userhost%(?..%{$FG[220]%}(%?%))%(!.$prompt_char_root.$prompt_char_normal)%{$reset_color%} '
fi
