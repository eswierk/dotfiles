if [[ $TERM != "dumb" ]]; then
    escape_pwd() {
        local q=${PWD//\\/\\\\}   # escape backslashes
        q=${q//\"/\\\"}           # escape embedded double quotes
        printf '"%s"' "$q"        # wrap in literal double quotes
    }
    PROMPT_COMMAND='printf "\033]52;e;set-dir %s %s %s\033\\" "$(id -un)" "$(hostname)" "$(escape_pwd)"'
fi
