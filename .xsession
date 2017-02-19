#!/usr/bin/env bash
export GTK_IM_MODULE="ibus"
export QT_IM_MODULE="ibus"
export XMODIFIERS="@im=ibus"
trayer --edge top --align left --margin 1820 --widthtype pixel --width 100 --heighttype pixel --height 16&
nm-applet&
ibus-daemon --xim --replace&
exec stack exec -- xmonad
