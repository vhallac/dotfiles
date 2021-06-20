#!/bin/sh

# I do not want messages in KDE
if [ "`xprop -root | grep KWIN_RUN | cut -f 2 -d= | sed 's/ 0x//'`" == "1" ]; then exit 0; fi

# nor in stumpwm -- for now (need to make it work)
#if [ `ps ax | grep lx.*stump | wc -l` -eq "2" ]; then exit 0; fi

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

echo "$text" | osd_cat -f "$font $fontsize" -c $color -p middle -A center -O 2 -d 2 -l 1 &
echo "$text" | osd_cat -f "$font $fontsize" -c $color -p top -A center -O 2 -d 2 -i 1920 -l 1



exit 0
pqiv=/home/vedat/pkgbuild/pqiv/pqiv

pkill pqiv
convert -size 1920x500 xc:transparent -font "$font" $weight -pointsize "$fontsize" -gravity Center \
        -fill "$light" -annotate -2-2 "$text" \
        -fill "$light" -annotate +0-2 "$text" \
        -fill "$light" -annotate -2+0 "$text" \
        -fill "black" -annotate +0+2 "$text" \
        -fill "black" -annotate +2+0 "$text" \
        -fill "black" -annotate +2+2 "$text" \
        -fill "$color" -annotate +0+0 "$text" \
        -trim +repage  \
        -bordercolor none -border 8 \
        png:- | \
	$pqiv --action='set_scale_level_absolute(1);set_shift_align_corner(N)' -d 2 -s --end-of-files-action="quit" -i --lazy-load -c -f -F - &
#;; Scale-level=1 ==> center align, otherwise follow directions
