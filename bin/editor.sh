#!/bin/sh

screen -X select 0
emacsclient $@
screen -X select $WINDOW
