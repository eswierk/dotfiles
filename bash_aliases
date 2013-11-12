if [ "${INSIDE_EMACS}" ]; then
    export EDITOR=emacsclient
else
    export EDITOR=zile
fi
alias v="source ~/.ssh/setscreenenv"
alias screen='echo -e "SSH_AUTH_SOCK=${SSH_AUTH_SOCK}\nDISPLAY=${DISPLAY}" >~/.ssh/setscreenenv; \screen'
