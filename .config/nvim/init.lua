require('base')
require('plugins')
require('highlights')
require('maps')

local has = vim.fn.has
local is_mac = has "macunix"

if is_mac == 1 then
  require('macos')
end

