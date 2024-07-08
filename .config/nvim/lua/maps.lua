-- vim.keymap.set はデフォルトで noremap=true

-- ; : の入れ替え
vim.api.nvim_set_keymap('n', ';', ':', {noremap=true})
vim.api.nvim_set_keymap('n', ':', ';', {noremap=true})

-- jj を ESC に
vim.keymap.set({'i', 'n', 'v', 'c'}, '<C-l>', '<ESC>')

-- ----------------------------------------------------------------------------
-- vim強制ギプス(emacsキーバインドを無効にする)
-- ----------------------------------------------------------------------------
vim.keymap.set({'i', 'n', 'v', 'c'}, '<C-a>', '<Nop>')
vim.keymap.set({'i', 'n', 'v', 'c'}, '<C-e>', '<Nop>')
vim.keymap.set({'i', 'n', 'v', 'c'}, '<C-b>', '<Nop>')
vim.keymap.set({'i', 'n', 'v', 'c'}, '<C-f>', '<Nop>')
vim.keymap.set({'i', 'n', 'v', 'c'}, '<C-n>', '<Nop>')
vim.keymap.set({'i', 'n', 'v', 'c'}, '<C-j>', '<Nop>')
vim.keymap.set({'i', 'n', 'v', 'c'}, '<C-p>', '<Nop>')
vim.keymap.set({'i', 'n', 'v', 'c'}, '<C-g>', '<Nop>')
vim.keymap.set({'i', 'n', 'v', 'c'}, '<C-d>', '<Nop>')

-- ----------------------------------------------------------------------------
-- shiftと移動で大きく移動できるようにする
-- ----------------------------------------------------------------------------
vim.keymap.set({'n', 'v'}, 'J', '10j')
vim.keymap.set({'n', 'v'}, 'K', '10k')
vim.keymap.set({'n', 'v'}, 'H', '10h')
vim.keymap.set({'n', 'v'}, 'L', '10l')

-- ----------------------------------------------------------------------------
-- COC
-- ----------------------------------------------------------------------------

-- 定義へ移動
vim.api.nvim_set_keymap('n', 'gd', '<Plug>(coc-definition)', {noremap=false, silent=true})

vim.api.nvim_set_keymap('n', 'gy', '<Plug>(coc-type-definition)', {noremap=false, silent=true})
vim.api.nvim_set_keymap('n', 'gi', '<Plug>(coc-implementation)', {noremap=false, silent=true})
vim.api.nvim_set_keymap('n', 'gr', '<Plug>(coc-references)', {noremap=false, silent=true})
vim.api.nvim_set_keymap('n', 'rn', '<Plug>(coc-rename)', {noremap=false, silent=true})
vim.api.nvim_set_keymap('n', 'gf', '<Plug>(coc-format)', {noremap=false, silent=true})
vim.api.nvim_set_keymap('i', '[g', '<Plug>(coc-diagnostic-prev)', {noremap=false, silent=true})
vim.api.nvim_set_keymap('i', ']g', '<Plug>(coc-diagnostic-next)', {noremap=false, silent=true})

-- 補完内容をリフレッシュする
vim.api.nvim_set_keymap("i", "<C-Space>", "coc#pum#stop() : coc#refresh()", {expr=true, noremap=true, silent=true})

-- 次の補完アイテムを選択する
vim.api.nvim_set_keymap("i", "<Tab>", "coc#pum#visible() ? coc#pum#next(1) : '<Tab>'", {expr=true, noremap=true, silent=true})

-- 前の補完アイテムを選択する
vim.api.nvim_set_keymap("i", "<S-Tab>", "coc#pum#visible() ? coc#pum#prev(1) : '<C-h>'", {expr=true, noremap=true, silent=true})

-- 選択中の補完アイテムで確定する
vim.api.nvim_set_keymap("i", "<CR>", "coc#pum#visible() ? coc#pum#confirm() : '<C-g>u<CR>'", {expr=true, noremap=true, silent=true})


