#!/bin/sh
## Mute or toggle the main microphone, as controlled by the button and
## microphone arm position

# file used to store current state
# $STATE is one of
# 'up'   the microphone arm is up, always muted,
# 'down' arm is down and unmuted, or,
# 'mute' arm is down, and muted
STATE_FILE=/tmp/g930-state

if [[ -f $STATE_FILE ]]
then
    STATE=$(cat $STATE_FILE)
else
    STATE="up"
fi

set_state() {
    STATE=$1
    echo $STATE > $STATE_FILE
}

mute() {
    echo "mute"
}

unmute() {
    echo "unmute"
}

arm() {
    case $1 in
	  up)
		mute
		set_state up
		;;
	  down)
		unmute
		set_state down
		;;
	  *)
		echo "unknown: $1, use 'up', or 'down'"
    esac
}

button() {
    case $STATE in
	  down)
		mute
		set_state mute
		;;
	  mute)
		unmute
		set_state down
		;;
    esac
}

case $1 in
    arm)
	  arm $2
	  ;;
    button)
	  button $2
	  ;;
    *)
	  echo "unknown command '$1', 'arm' or 'button'"
	  ;;
esac
