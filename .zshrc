# C:/msys64/msys2_shell.cmd -defterm -here -no-start -mingw64 -use-full-path -shell zsh

export INFOPATH="$INFOPATH:$HOME/info:"

alias diff='diff --color=auto'
alias emacs='nohup C:/Program\ Files/Emacs/emacs-28.2/bin/emacs.exe &>/dev/null &'
alias grep='grep --color=auto'
alias ls='ls --color=auto'

HISTFILE=~/.histfile
HISTSIZE=10000
PROMPT='%F{magenta}%~ %F{cyan}%% %f'
SAVEHIST=10000
setopt inc_append_history
setopt share_history
unsetopt beep
bindkey -e

zstyle :compinstall filename '/c/Users/brent/.zshrc'
autoload -Uz compinit
compinit
