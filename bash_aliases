# On Mac, ln -s .bash_aliases ~/.bash_profile

if [ "${OSTYPE#darwin}" != "${OSTYPE}" ]; then
    export PATH=/Applications/Emacs.app/Contents/MacOS/bin:${PATH}
fi

if [ "${INSIDE_EMACS}" ]; then
    export EDITOR=emacsclient
    export COLUMNS=150
    export PAGER=cat
elif which zile >/dev/null; then
    export EDITOR=zile
else
    export EDITOR=nano
fi

alias m="fusermount -u /mnt/hgfs/eswierk; sshfs -o idmap=user -o reconnect -o follow_symlinks dogcow: /mnt/hgfs/eswierk"
alias v="source ~/.ssh/setscreenenv"
alias screen='echo -e "export SSH_AUTH_SOCK=${SSH_AUTH_SOCK}\nexport DISPLAY=${DISPLAY}" >~/.ssh/setscreenenv; \screen'
alias c="echo -e \"\033[m\""
