#!/bin/bash
export BROWSER=firefox
dunst &
xrdb -merge .Xresources

trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --height 10 --transparent true --tint 0x000000 &
#trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --height 10 --tint 0x000000 &

#gnome-settings-daemon

#if [ -x /usr/bin/gnome-power-manager ] ; then
#   sleep 1
#   gnome-power-manager
#else 
#   batti &
#fi

#udiskie & # auto-mount USB

if [ -x /usr/bin/nm-applet ] ; then
   nm-applet --sm-disable &
fi

#dropboxd & 
#unclutter & # hide the mouse

export PATH=/home/benh/bin:$PATH
source /home/benh/.profile

xmodmap $HOME/.Xmodmap

exec xmonad
