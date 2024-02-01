if [[ $TERM != "dumb" ]]; then
    PS1=${PS1}"\e]51;A\u@\h:\w\e\\"
fi
