if [[ -f ~/dotfiles/emacs-vterm-zsh.sh ]] && [[ $TERM != "dumb" ]]; then
  source ~/dotfiles/emacs-vterm-zsh.sh

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
fi

if type brew &>/dev/null
then
  FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"

  autoload -Uz compinit
  compinit
fi

[[ $TERM == "dumb" ]] && unsetopt zle

source ~/.bash_aliases
