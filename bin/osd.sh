#!/bin/sh
text=$*
color=green
light="light $color"
if [ -n `convert -size 5x5 xc:white -fill "$light" -draw "point 1,1" png:- 2>&1 >/dev/null` ]; then light="white"; fi

font="Century" #Ravie
weight="-weight bold"
fontsize=72

while [[ $# -gt 0 ]]; do
    case $1 in
        "-s")
            fontsize="$2"
            shift
            shift
            ;;
        "-c")
            color="$2"
            shift
            shift
            ;;
        *)
            text="$*"
            shift $#
            ;;
    esac
done

#win=`xdotool getactivewindow`
# Keep the border for now. XMonad borders are not under control
pkill pqiv
convert -size 1918x1078 xc:transparent -font "$font" $weight -pointsize "$fontsize" -gravity center \
        -fill "$light" -annotate -2-2 "$text" \
        -fill "$light" -annotate +0-2 "$text" \
        -fill "$light" -annotate -2+0 "$text" \
        -fill "black" -annotate +0+2 "$text" \
        -fill "black" -annotate +2+0 "$text" \
        -fill "black" -annotate +2+2 "$text" \
        -fill "$color" -annotate +0+0 "$text" \
        -trim +repage xc:transparent \
        -bordercolor none -border 8 \
        png:- | \
    pqiv -d 2 -s --end-of-files-action="quit" -i -F --disable-scaling --lazy-load -P off -c - &
#while [ "$win" -eq `xdotool getactivewindow` ]; do sleep 0.05; done
#xdotool getactivewindow windowmove x 0 windowraise
#xdotool windowfocus $win
