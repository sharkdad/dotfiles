export INFOPATH="$INFOPATH:$HOME/info:"
export PATH="$HOME/bin:$HOME/.local/bin:$PATH"

alias diff='diff --color=auto'
alias grep='grep --color=auto'
alias ls='ls --color=auto'
alias la='ls -A'
alias ll='ls -l'
alias lla='ls -lA'

HISTFILE=~/.histfile
HISTSIZE=10000
PROMPT='%F{magenta}%~ %F{cyan}%% %f'
SAVEHIST=10000
setopt inc_append_history
setopt share_history
unsetopt beep
bindkey -e

zstyle :compinstall filename '$HOME/.zshrc'
autoload -Uz compinit
compinit
