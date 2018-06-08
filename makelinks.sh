#!/bin/bash

makelink() {
	target_dir=$1
	shift
	ln -t "${target_dir}" -s -r -v "$@"
}

autolink() {
	while [ -n "$1" ]; do
		dirname="${HOME}/$(dirname "$1")"
		mkdir -p "$dirname"
		makelink "$dirname" "$1"
		shift
	done
}

autolink_dir() {
	while [ -n "$1" ]; do
		find "$1" -type f -print0 | while read -r -d '' file; do
			autolink "$file"
		done
		shift
	done
}

create_templates() {
    while [ -n "$1" ]; do
	echo $1
        if [ ! -e "${HOME}/$1" ]; then
            dirname="${HOME}/$(dirname "$1")"
            mkdir -p "$dirname"
            cp "templates/$1" "$dirname"
        fi
	shift
    done
}

autolink .aliases .bash_aliases .environment .gitconfig .gitk .inputrc .screenrc .screenrc-irc .tmux.conf .XCompose .Xresources .zshrc

autolink_dir bin .i3 .emacs.d

create_templates .emacs-local-config.el
