#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Emacs Capture
# @raycast.mode silent

# Optional parameters:
# @raycast.icon 🦄

# Documentation:
# @raycast.description Org-capture emacsclient

emacsclient -c -F "((name . \"doom-capture\"))" --eval '(mw/launch-note)'