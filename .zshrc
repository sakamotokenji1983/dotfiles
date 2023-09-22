# プロンプト ユーザ@ホスト:カレントディレクトリ 現在日時
export PS1="[%n@%M:%~ %D{%Y-%m-%dT%H:%M:%S}]
%# "

# vim系をコマンドを全てneovimにする
alias vi="nvim"
alias vim="nvim"
alias view="nvim -R"

# docker
alias d="docker"
alias dc="docker compose"
alias dcb='docker compose build --no-cache && docker image prune -f'

# python
alias python="python3"
alias pip="pip3"

# lsコマンドエイリアス
alias ls="lsd -a --date '+%Y-%m-%d %H:%M:%S'"
alias ll="lsd -la --date '+%Y-%m-%d %H:%M:%S'"

# treeコマンドで日本語が表示できるようにする
# -N ... Print non-printable characters as is.
# 『印刷不可能な文字をそのまま印刷する』
alias tree='tree -N'

# 履歴ファイルの保存先
HISTFILE=${HOME}/.zsh_history

# メモリに保存される履歴の件数
HISTSIZE=1000

# 履歴ファイルに保存される履歴の件数
export SAVEHIST=100000

# ディレクトリ移動にcdを省略できるようにする
setopt auto_cd

# cd履歴をみれるように変更
setopt auto_pushd

# インタラクティブシェルにおいて#以降をコメントとして扱う
setopt interactive_comments

# history に時刻を記録する
setopt extended_history

# zsh-syntax-highlighting を導入する
if [ ! -e ~/.zsh/zsh-syntax-highlighting ]; then
    git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ~/.zsh/zsh-syntax-highlighting
fi
source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# zsh-completion を導入する
if [ ! -e ~/.zsh/zsh-completions ]; then
    git clone https://github.com/zsh-users/zsh-completions.git ~/.zsh/zsh-completions
fi
fpath=(~/.zsh/completions $fpath)
autoload -U compinit
compinit -u

# zsh-autosuggestions を導入する
if [ ! -e ~/.zsh/zsh-autosuggestions ]; then
    git clone https://github.com/zsh-users/zsh-autosuggestions ~/.zsh/zsh-autosuggestions
fi
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=#ff1493'
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

# 個別コマンドファイルがある場合は読み込む
if [ -e ~/.zsh_localrc ]; then
    source ~/.zsh_localrc
fi

