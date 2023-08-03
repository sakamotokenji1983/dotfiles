" 読み込み時の文字コードの設定
set encoding=utf-8

" バックアップファイルを作らない
set nobackup

" スワップファイルを作らない
set noswapfile

" 編集中のファイルが変更されたら自動で読み直す
set autoread

" ヘルプメッセージを日本語にする
set helplang=ja

" 背景色を透過させる設定
highlight Normal ctermbg=none
highlight NonText ctermbg=none
highlight LineNr ctermbg=none
highlight Folded ctermbg=none
highlight EndOfBuffer ctermbg=none 

" INSERTモードでEmacsKeyBindingを使用する
imap <silent> <C-P> <Up>
imap <silent> <C-N> <Down>
imap <silent> <C-B> <Left>
imap <silent> <C-F> <Right>
imap <silent> <C-A> <Home>
imap <silent> <C-E> <End>
imap <silent> <C-D> <Del>
imap <silent> <C-K> <C-O>D

" コマンドモードでEmacsKeyBindingを使用する
cmap <C-P> <Up>
cmap <C-N> <Down>
cmap <C-B> <Left>
cmap <C-F> <Right>
cmap <C-A> <Home>
cmap <C-E> <End>
cmap <C-D> <Del>

" yank clipboard
set clipboard+=unnamed

" 曖昧文字の設定
if exists('&ambw')
  set ambiwidth=double
endif

" 折返ししない
set nowrap

" 行番号を表示する
set number

" 挿入モードで挿入するときタブの代わりに空白を使う
set expandtab

set tabstop=4

" 検索にマッチするテキストをハイライトする
set hlsearch

" 検索時に大文字と小文字を区別しない
set ignorecase

" インクリメンタルサーチを有効化
set incsearch

" ウィンドウの最下部にステータス行を常に表示する
set laststatus=2

" シンタックスハイライトを有効化
syntax on

" 自動インデントを有効化
set autoindent

" カーソルのルーラを有効化
set ruler
