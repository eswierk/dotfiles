# On Mac, ln -s .bash_aliases ~/.bash_profile

export PATH=${HOME}/.bin:${PATH}

if [ "${OSTYPE#darwin}" != "${OSTYPE}" ]; then
    export PATH=/Applications/Emacs.app/Contents/MacOS/bin:${PATH}
fi

if [ "${INSIDE_EMACS}" ]; then
    export EDITOR=emacsclient
    export COLUMNS=150
    export PAGER=cat
elif [ "${TERM}" = dumb ]; then
    export EDITOR='/bin/bash -c "trap \"echo; exit 1\" INT; echo -n \"\$0\"; read x"'
elif which zile >/dev/null; then
    export EDITOR=zile
else
    export EDITOR=nano
fi

alias m="fusermount -u /mnt/hgfs/eswierk; sshfs -o idmap=user -o reconnect -o follow_symlinks kristof.local: /mnt/hgfs/eswierk"
alias v="source ~/.ssh/setscreenenv"
alias screen='echo -e "export SSH_AUTH_SOCK=${SSH_AUTH_SOCK}\nexport DISPLAY=${DISPLAY}" >~/.ssh/setscreenenv; \screen'
alias tmux='echo -e "export SSH_AUTH_SOCK=${SSH_AUTH_SOCK}\nexport DISPLAY=${DISPLAY}" >~/.ssh/setscreenenv; \tmux'
alias c="echo -e \"\033[m\""
