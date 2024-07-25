#!/bin/bash

# Copyright (c) 2024 SAKAMOTO Kenji
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

SRC=$(cd "$(dirname "${BASH_SOURCE:-$0}")" && pwd)

# ディレクトリのパスを配列として指定します
directories=(
    "${HOME}/.emacs.d"
    "${HOME}/.emacs.d/theme/iceberg"
    "${HOME}/.config/nvim"
    "${HOME}/.config/nvim/after/plugin"
    "${HOME}/.config/nvim/lua"
    "${HOME}/.config/alacritty"
    "${HOME}/.config/karabiner"
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
    ".config/karabiner/karabiner.json"
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