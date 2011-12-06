#!/bin/sh
 
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
#
 
# Start URxvt daemon so we can quickly open the other clients
urxvtd -q -o -f
[[ -x "/usr/bin/numlockx" ]] && numlockx &
[[ -x "/usr/bin/unclutter" -a -z "`pidof unclutter`" ]] && \
    unclutter -idle 5 -root&
 
# Set Wallpaper with feh
eval `cat ~/.fehbg` &
 
# Set mouse cursor and background colour
xsetroot -cursor_name left_ptr -solid '#090909' &
 
# Launch tray and statusbar
stalonetray -i 16 --max-width 48 --icon-gravity E --geometry 48x16-0+0 -bg '#2e3436' --sticky --skip-taskbar &
~/.bin/dzen.sh | dzen2 -e 'onstart=lower' -p -ta r -fn '-*-terminus-medium-*-*-*-12-*-*-*-*-*-iso8859-1' -bg '#2e3436' -fg '#babdb6' -h 16 -w 1632 &
 
# Launch WM
exec xmonad
3 dzen2 script 
