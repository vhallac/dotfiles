# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
#HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend
# append to history immediately - no option for this, so we will use a work-around
PROMPT_COMMAND="history -a;$PROMPT_COMMAND"

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-*color|screen-*color|st-*color) color_prompt=yes;;
    dumb) skip_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -z "$skip_prompt" ]; then
    if [ -n "$force_color_prompt" ]; then
        if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	        # We have color support; assume it's compliant with Ecma-48
	        # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	        # a case would tend to support setf rather than setaf.)
	        color_prompt=yes
        else
	        color_prompt=
        fi
    fi

    if [ "$color_prompt" = yes ]; then
        PS1="\n┌┬───[\[\033[01;35m\]\t\[\033[00m\]]─[\[\033[01;32m\]\u@\h\[\033[00m\]]─[\[\033[01;34m\]\w\[\033[00m\]]${debian_chroot:+─($debian_chroot)}\n└┴\$ "
        #    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
    else
        PS1="\n┌┬──[\t]─[\u@\h]─[\w]${debian_chroot:+─($debian_chroot)}\n└┴\$ "
        #    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
    fi
    unset color_prompt force_color_prompt

    # If this is an xterm set the title to user@host:dir
    case "$TERM" in
        xterm*|rxvt*|screen*)
            PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
            ;;
        *)
            ;;
    esac
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

if ! echo $PATH | grep ~/bin >/dev/null 2>&1; then
    PATH="$PATH:~/bin"
fi

#CDPATH=.:~:~/work/avea-prm
#export CDPATH

# Pull in custom completion scripts
if [ -d "$HOME/.bash-completion" ]; then
    for i in $HOME/.bash-completion/*; do
        . $i
    done
fi

ec() {
	emacsclient -nw $*
}

ecw() {
	emacsclient -n -c $*
}

# Alias definitions.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

export http_proxy=''
export https_proxy=''
export ftp_proxy=''
export socks_proxy=''
