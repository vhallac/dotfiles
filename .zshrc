# The following lines were added by compinstall
zstyle :compinstall filename '/home/vedat/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

## Completion ##
eval "$(dircolors -b)"
zstyle ':completion:*' list-colors ''
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' menu select=1

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER --forest -o pid,%cpu,%mem,tty,bsdtime,cmd'

#Ubuntu command-not-found
[[ -r /etc/zsh_command_not_found ]] && . /etc/zsh_command_not_found

## zsh Options ##
REPORTTIME=20
DIRSTACKSIZE=16

setopt extendedglob

setopt autopushd
setopt nohup
unsetopt autocd
unsetopt beep
unsetopt notify

## History ##
HISTFILE=~/.histfile
HISTORY_IGNORE='ls *'
HISTSIZE=32768
SAVEHIST=32768
HIST_IGNORE_ALL_DUPS=y

setopt extendedhistory
setopt histignoredups
setopt histignorespace
setopt histreduceblanks
#setopt incappendhistorytime
setopt sharehistory

## Key Bindings ##
bindkey -e # emacs mode

#history search
#bindkey '^R' history-beginning-search-backward
#bindkey '^F' history-beginning-search-forward

#emacs bindings
bindkey '^A' beginning-of-line
bindkey '^E' end-of-line

#shift-enter same as enter
bindkey '^[OM' accept-line

#backspace through newlines
bindkey '^H' backward-delete-char
bindkey '^?' backward-delete-char

# Fancy prompt
export  PS1C=$'\n┌┬───\[%F{red}%D{%H:%M:%S}%f\]──\[%?\]—\[%F{green}%n@%m%f\]—\[%F{blue}%~%f\]\n└┴%# '
export PS1NC=$'\n┌┬───\[%D{%H:%M:%S}\]──\[%?\]—\[%n@%m\]—\[%~\]\n└┴%# '
if [ "$TERM" = "dumb" ]; then
	export PS1=$PS1NC
	unset zle_bracketed_paste
else
	export PS1=$PS1C
fi

#path
typeset -U path
path=(~/bin ~/.local/bin ~/go/bin $path ~/.dotnet/tools)

## Common Files ##
[[ -r ~/.environment ]] && . ~/.environment
[[ -r ~/.aliases ]] && . ~/.aliases

autoload -U select-word-style
select-word-style bash
## Startup ##
#fortune -a | cowsay -n -TUU
