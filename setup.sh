#!/bin/bash

SRC=$(cd $(dirname ${BASH_SOURCE:-$0}); pwd)

files=(
    ".bashrc"
    ".config/nvim/after"
    ".config/nvim/init.lua"
    ".config/nvim/lua"
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

