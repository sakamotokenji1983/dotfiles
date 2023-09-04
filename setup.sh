#!/bin/bash

SRC=$(cd $(dirname ${BASH_SOURCE:-$0}); pwd)

mkdir ~/.config

files=(
    ".bashrc"
    ".config/git/ignore"
    ".config/nvim/after"
    ".config/nvim/init.lua"
    ".config/nvim/lua"
    ".config/tmux/statusline.conf"
    ".config/tmux/tmux.conf"
    ".config/tmux/utility.conf"
    ".emacs.d/init.el"
    ".gitconfig"
    ".gitignore"
    ".vimrc"
    ".zshrc"
)

for file in ${files[@]}; do
  unlink ~/$file
  ln -sf $SRC/$file ~/$file
done

