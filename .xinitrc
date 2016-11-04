#!/usr/bin/env bash
export GTK_IM_MODULE="ibus"
export XMODIFIERS="@im=ibus"
export QT_IM_MODULE="ibus"
exec stack exec -- xmonad
