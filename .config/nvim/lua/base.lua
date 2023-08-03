-- 文字コードの設定
vim.scriptencoding = 'utf-8'
vim.opt.encoding = 'utf-8'
vim.opt.fileencoding = 'utf-8'

-- 行番号を表示する
vim.wo.number = true

-- Vimウィンドウのタイトルバーに表示されるテキスト
vim.opt.title = true

-- 新しい行を挿入する際に自動的に適切にインデントする
vim.opt.autoindent = true

-- 自動的にインデントする
vim.opt.smartindent = true

-- 検索結果をハイライトする
vim.opt.hlsearch = true

vim.opt.completeopt = {'menuone', 'noselect'}
vim.opt.backup = false
vim.opt.showcmd = true
vim.opt.cmdheight = 1
vim.opt.laststatus = 2
vim.opt.expandtab = true -- タブキー押下時にスペースを使用する
vim.opt.scrolloff = 10
vim.opt.shell = 'zsh'
vim.opt.backupskip = { '/tmp/*', '/private/tmp/*' }
vim.opt.inccommand = 'split'
vim.opt.ignorecase = true
vim.opt.smarttab = true
vim.opt.breakindent = true

-- ノーマルモードで行末にカーソルを当てれるようにする
vim.opt.virtualedit:append 'onemore'

-- テキストのインデントに使用されるスペースの数
vim.opt.shiftwidth = 4

vim.opt.tabstop = 4
vim.opt.wrap = false
vim.opt.backspace = { 'start', 'eol', 'indent' }
vim.opt.path:append { '**' }
vim.opt.wildignore:append { '*/node_modules/*' }

vim.cmd([[let &t_Cs = "\e[4:3m"]])
vim.cmd([[let &t_Ce = "\e[4:0m"]])

vim.api.nvim_create_autocmd("InsertLeave", {
  pattern = '*',
  command = "set nopaste"
})

vim.opt.formatoptions:append { 'r' }

