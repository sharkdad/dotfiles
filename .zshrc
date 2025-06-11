alias diff='diff --color=auto'
alias grep='grep --color=auto'
alias ls='ls --color=auto'
alias la='ls -A'
alias ll='ls -l'
alias lla='ls -lA'

HISTFILE=~/.zsh_history
HISTSIZE=10000
PROMPT='%F{default}[%F{blue}%m%F{default}:%F{cyan}%20<..<%~%F{default}] %(?.%F{cyan}.%F{red})%# %F{default}%f'
SAVEHIST=10000
setopt inc_append_history
setopt hist_ignore_dups
unsetopt beep
bindkey -e

zstyle :compinstall filename '$HOME/.zshrc'
autoload -Uz compinit
compinit

[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  source "$EAT_SHELL_INTEGRATION_DIR/zsh"
