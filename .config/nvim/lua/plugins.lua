-- lazy設定
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable", -- latest stable release
        lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({

    {
        -- LSP
        'neoclide/coc.nvim',
        branch = 'release'
    },

    {
        -- Solarizedテーマ
        'svrana/neosolarized.nvim',
        dependencies = { 'tjdevries/colorbuddy.nvim' }
    },

    {
        -- カスタマイズステータスライン
        'nvim-lualine/lualine.nvim',
        dependencies = { 'nvim-tree/nvim-web-devicons', opt = true }
    },

    {
        -- 便利なユーティリティ関数やヘルパー関数
        'nvim-lua/plenary.nvim'
    },


    {
        -- 構文解析エンジン
        'nvim-treesitter/nvim-treesitter'
    },

    {
        -- ファジーファインダー
        'nvim-telescope/telescope.nvim'
    },

    {
        -- ファイルブラウザ機能
        'nvim-telescope/telescope-file-browser.nvim'
    }
})


