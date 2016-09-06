#!/bin/sh
PATH=$PATH:/opt/X11/bin:/usr/X11/bin
test ! -L `which xset` || {
   echo "No X Display!"
   echo `env`
   echo `which xset`
   osascript -e 'tell app "Installer" to display alert "No working X Display Found. Please install XQuartz, restart, and try again." buttons "OK" default button 1 giving up after 30'
   exit 1
}
echo "Found X Display!"
echo `env`
echo `which xset`
defaults write com.apple.x11 wm_click_through -bool true
defaults write org.x.x11 wmclickthrough -bool true
defaults write org.macosforge.xquartz.X11 wm_click_through -bool true
