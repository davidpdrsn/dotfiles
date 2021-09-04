#!/bin/sh

setup_symlinks() {
  for source in `find ~/dotfiles -iname "*.symlink"`
  do
    dest="$HOME/.`basename \"${source%.*}\"`"
    rm -rf $dest
    ln -s $source $dest
  done

  rm -rf $HOME/.config/nvim
  ln -s $HOME/dotfiles/nvim $HOME/.config/nvim
  ln -s $HOME/dotfiles/nvim/ $HOME/.vim
  ln -s $HOME/dotfiles/alacritty/config.yml $HOME/.config/alacritty/alacritty.yml

  echo "symlinks ✓"
}

setup_symlinks
