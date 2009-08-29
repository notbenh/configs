# set background image
#exec xloadimage -onroot -center -fit -fullscreen -zoom 60 -quiet ~/WallPaper/current.jpg &
#exec xloadimage -onroot -center -fit -fullscreen -quiet ~/WallPaper/current.jpg &
#exec xloadimage -onroot -center -fit -fullscreen -quiet -shrink -smooth -zoom 60 ~/WallPaper/current.jpg &
#exec xloadimage -onroot -fit -shrink -xzoom 80 -at -60,0 -fullscreen ~/WallPaper/current.jpg

exec gnome-settings-daemon &
exec /home/benh/apps/tb &
exec pidgin &
exec mitter &
exec conky &
#exec /home/benh/.cabal/bin/xmobar &


# Alter the path when we start so we have access to things that we like
PATH=/home/benh/apps:/home/benh/bin:/usr/local/bin:/usr/bin:/bin:/opt/local/bin:/home/benh/apps/Songbird:$PATH

#exec xmonad
exec /home/benh/.xmonad/xmonad-i386-linux
