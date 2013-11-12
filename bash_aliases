if [ "${OSTYPE#darwin}" != "${OSTYPE}" ]; then
    export PATH=/Applications/Emacs.app/Contents/MacOS/bin:${PATH}
fi

if [ "${INSIDE_EMACS}" ]; then
    export EDITOR=emacsclient
elif which zile >/dev/null; then
    export EDITOR=zile
else
    export EDITOR=nano
fi

alias v="source ~/.ssh/setscreenenv"
alias screen='echo -e "SSH_AUTH_SOCK=${SSH_AUTH_SOCK}\nDISPLAY=${DISPLAY}" >~/.ssh/setscreenenv; \screen'
