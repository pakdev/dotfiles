#!/bin/sh
xrandr --setprovideroutputsource modesetting NVIDIA-0
xrandr --dpi 192
xrandr --auto

slstatus &

#feh --bg-fill /home/peter/dotfiles/wallpapers/chall_deep_blocks.h.png

dwm 
