if [ "${INSIDE_EMACS}" ]; then
    export EDITOR=emacsclient
else
    export EDITOR=zile
fi
