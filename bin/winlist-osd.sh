#!/bin/sh
text=$(xdotool get_desktop search --desktop $(xdotool get_desktop) --class '.*' getwindowname %@ |\
           grep -v pqiv |\
           sed 's/\(.\{25\}\).\{3,\}\(.\{7\}\)/\1...\2/')
`dirname $0`/osd.sh -s 25 -c blue "$text"
