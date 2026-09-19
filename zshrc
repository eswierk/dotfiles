if [[ ! "${INSIDE_EMACS}" ]] && [[ "$TERM" = "xterm-ghostty" ]]; then
  source ~/.local/share/ghostel/ghostel.zsh
fi

if [[ $TERM == "dumb" ]]; then
  unsetopt zle
else
  _kill_line_to_clip() {
      zle .set-mark-command
      zle .end-of-line
      zle .exchange-point-and-mark
      zle .copy-region-as-kill
      printf "\033]52;c;$(printf "%s" "${CUTBUFFER}" | base64)\a"
      zle .kill-line
  }
  zle -N kill-line-to-clip _kill_line_to_clip
  bindkey '^k' kill-line-to-clip

  _backward_kill_word_to_clip() {
      zle .set-mark-command
      zle .backward-word
      zle .exchange-point-and-mark
      zle .copy-region-as-kill
      printf "\033]52;c;$(printf "%s" "${CUTBUFFER}" | base64)\a"
      zle .backward-kill-word
  }
  zle -N backward-kill-word-to-clip _backward_kill_word_to_clip
  bindkey '^w' backward-kill-word-to-clip
  bindkey "\ep" history-beginning-search-backward
  bindkey "\en" history-beginning-search-forward

  autoload -U colors
  colors

  PROMPT="%{$fg[cyan]%}%m:%{$fg[yellow]%}%~ %{$reset_color%}%% %{%}"
fi

if [[ $SHLVL -eq 1 && -z $SSH_TTY ]]; then
    setopt IGNORE_EOF
fi

yes() { echo "No." >&2; return 1 }

if type brew &>/dev/null
then
  FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"

  autoload -Uz compinit
  compinit
fi

source ~/.bash_aliases
