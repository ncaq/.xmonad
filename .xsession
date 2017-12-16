#!/usr/bin/env bash
export GTK_IM_MODULE="ibus"
export QT_IM_MODULE="ibus"
export XMODIFIERS="@im=ibus"
trayer --edge top --align right --widthtype percent --width 10 --heighttype pixel --height 16&
nm-applet&
ibus-daemon --xim --replace&
exec stack exec -- xmonad
