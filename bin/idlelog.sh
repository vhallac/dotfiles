#!/bin/sh
# 10 minutes is counted as inactivity
INACTIVITY_SECS=60
STOP_SECS=600
ACTIVITY_LOG_FILE=/tmp/activity
ACTIVITY_LOG_HIGH_WATERMARK=1000
ACTIVITY_LOG_TRIM_TO=800

procname=`basename $0`
if [ `pgrep $procname | wc -l` -gt 2 ]; then
    echo $0 is already running. Not starting another instance.
    exit 0
fi

activityStream() {
    while true; do
        sleep 30
        echo `xprintidle`/1000 | bc
    done | while read idleSec; do
        if [ "$idleSec" -gt "$STOP_SECS" ]; then
            echo S
        elif [ "$idleSec" -gt "$INACTIVITY_SECS" ]; then
            echo I
        else
            echo A
        fi
    done
}

lastState=I

timelogMaybeStop() {
    if t cat | tail -1 | grep '^i' >/dev/null; then
        # Copy the start of inactivity into timelog
        tail -1 $ACTIVITY_LOG_FILE | sed 's/^[^ ]\+ /o /' >> `t timelog`
    fi
}

activityStream | while read state; do
    if [ $lastState = "I" -a $state = "S" ]; then
        timelogMaybeStop
    elif [ $lastState = "S" -a $state = "A" ]; then
        echo "Started: `date +'%Y-%m-%d %H:%M:%S'`"
        notify-send "Remember to start timer if you are working"
    elif [ $lastState = "I" -a $state = "A" ]; then
        echo "Started: `date +'%Y-%m-%d %H:%M:%S'`"
    elif [ $lastState = "A" -a $state = "I" ]; then
        echo "Stopped: `date +'%Y-%m-%d %H:%M:%S'`"
    fi
    lastState=$state
done | while read activity; do
    echo $activity >> $ACTIVITY_LOG_FILE
    if [ `wc -l $ACTIVITY_LOG_FILE | cut -f1 -d\ ` -gt $ACTIVITY_LOG_HIGH_WATERMARK ]; then
        tmpfile=`mktemp`
        # Using cp to preserve owner and mode
        tail -$ACTIVITY_LOG_TRIM_TO $ACTIVITY_LOG_FILE > $tmpfile
        cp $tmpfile $ACTIVITY_LOG_FILE
        rm $tmpfile
    fi
done
