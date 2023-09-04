# alias
alias ls='ls -a'
alias ll='ls -la'

# pオプションで深い階層のディレクトリも一気に作成する
alias mkdir='mkdir -p'

# docker
alias d='docker'
alias dc='docker compose'
alias dcb='docker compose build --no-cache && docker image prune -f'

# python
alias python="python3"
alias pip="pip3"

# prompt
export PS1="[\u@\H:\w \D{%Y-%m-%dT%H:%M:%S}]\n\$ "

# MacでZshデフォルト警告を非表示にする
export BASH_SILENCE_DEPRECATION_WARNING=1
