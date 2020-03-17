#!/bin/sh

# I do not want messages in KDE
if [ "`xprop -root | grep KWIN_RUN | cut -f 2 -d= | sed 's/ 0x//'`" == "1" ]; then exit 0; fi

# nor in stumpwm -- for now (need to make it work)
if [ `ps ax | grep lx.*stump | wc -l` -eq "2" ]; then exit 0; fi

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
	pqiv --action="set_scale_level_absolute(1)" -d 2 -s --end-of-files-action="quit" -i -F --lazy-load -P off -c - &
