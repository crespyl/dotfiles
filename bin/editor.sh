#!/bin/sh

emacsclient --create-frame --tty --alternate-editor=nano "$@"
