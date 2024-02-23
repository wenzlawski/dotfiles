#!/usr/bin/osascript

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Create New Desktop
# @raycast.mode fullOutput

# Optional parameters:
# @raycast.icon 🆕

tell application "System Events"
	do shell script quoted form of "/System/Applications/Mission Control.app/Contents/MacOS/Mission Control"
	click button 1 of group "Spaces Bar" of group 1 of group "Mission Control" of process "Dock"
	do shell script quoted form of "/System/Applications/Mission Control.app/Contents/MacOS/Mission Control"
end tell
