export PATH=${HOME}/.bin:${PATH}

if [ "${OSTYPE#darwin}" != "${OSTYPE}" ]; then
    export PATH=/Applications/Emacs.app/Contents/MacOS/bin:${PATH}
fi

if [ "${INSIDE_EMACS}" ]; then
    export EDITOR=emacsclient
    export COLUMNS=150
    export PAGER=cat
elif [ "${TERM}" = xterm-color ]; then
    export EDITOR='/bin/sh -rc "printf \"\\e]51;Efind-file \\\"/scp:\$(hostname -s):\$0\\\"\\e\\\\\"; read x"'
elif which zile >/dev/null; then
    export EDITOR=zile
else
    export EDITOR=nano
fi

alias v="source ~/.ssh/setscreenenv"
alias screen='echo -e "export SSH_AUTH_SOCK=${SSH_AUTH_SOCK}\nexport DISPLAY=${DISPLAY}" >~/.ssh/setscreenenv; \screen'
alias tmux='echo -e "export SSH_AUTH_SOCK=${SSH_AUTH_SOCK}\nexport DISPLAY=${DISPLAY}" >~/.ssh/setscreenenv; \tmux'
alias rst='echo -en "\\ec"'
alias e="${EDITOR}"

fx() { p="*$1"; shift; find . -name "$p" -print0 | xargs -0 grep "$@" }
