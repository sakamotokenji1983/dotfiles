#!/bin/bash

SRC=$(cd "$(dirname "${BASH_SOURCE:-$0}")" && pwd)

# ディレクトリのパスを配列として指定します
directories=(
    "${HOME}/.emacs.d"
    "${HOME}/.emacs.d/theme/iceberg"
    "${HOME}/.config/nvim"
    "${HOME}/.config/nvim/after/plugin"
    "${HOME}/.config/nvim/lua"
    "${HOME}/.config/alacritty"
)

# ディレクトリがなければ作成する
for dir in "${directories[@]}"; do
    if [ ! -d "$dir" ]; then
        mkdir -p "$dir"
    fi
done

files=(
    ".bashrc"
    ".config/nvim/after/plugin/lualine.rc.lua"
    ".config/nvim/after/plugin/neosolarized.rc.lua"
    ".config/nvim/after/plugin/telescope.rc.lua"
    ".config/nvim/init.lua"
    ".config/nvim/lua/base.lua"
    ".config/nvim/lua/highlights.lua"
    ".config/nvim/lua/macos.lua"
    ".config/nvim/lua/maps.lua"
    ".config/nvim/lua/plugins.lua"
    ".config/alacritty/alacritty.toml"
    ".emacs.d/init.el"
    ".emacs.d/theme/iceberg/iceberg-theme.el"
    ".vimrc"
    ".zshrc"
    ".tmux.conf"
    ".screenrc"
)

for file in "${files[@]}"; do
  ln -sf "${SRC}/${file}" ~/"${file}"
done