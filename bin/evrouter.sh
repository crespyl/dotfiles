#!/bin/sh

if [[ -f "/tmp/.evrouter$DISPLAY" ]]
then
    evrouter -q
    rm "/tmp/.evrouter$DISPLAY"
fi

evrouter \
    /dev/input/by-id/usb-Logitech_USB-PS_2_Optical_Mouse-event-mouse \
    /dev/input/by-id/usb-Microsoft_Natural®_Ergonomic_Keyboard_4000-if01-event-kbd \
    /dev/input/by-id/usb-Logitech_Logitech_G930_Headset-event-if03
