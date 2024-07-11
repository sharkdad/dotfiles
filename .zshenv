export INFOPATH="$INFOPATH:$HOME/info:"
export PATH="$HOME/bin:$HOME/.local/bin:$PATH"
export MOZ_USE_XINPUT2=1

if [[ "$TERM" == "dumb-emacs-ansi" ]]; then
    export COLORTERM=truecolor
    export PAGER=cat
else
    export EDITOR=vim
fi
