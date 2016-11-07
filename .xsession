#!/usr/bin/env bash
export GTK_IM_MODULE="ibus"
export QT_IM_MODULE="ibus"
export XMODIFIERS="@im=ibus"
exec stack exec -- xmonad
