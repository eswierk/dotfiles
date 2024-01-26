if [[ -f ~/dotfiles/emacs-vterm-zsh.sh ]]; then
  source ~/dotfiles/emacs-vterm-zsh.sh
fi

if type brew &>/dev/null
then
  FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"

  autoload -Uz compinit
  compinit
fi

[[ $TERM == "dumb" ]] && unsetopt zle

source ~/.bash_aliases
