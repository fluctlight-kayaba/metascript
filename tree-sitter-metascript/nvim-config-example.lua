-- Neovim configuration for Metascript syntax highlighting
-- Add this to your init.lua or init.vim

-- 1. Register Metascript parser
local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config.metascript = {
  install_info = {
    url = "~/projects/metascript/tree-sitter-metascript", -- local path
    files = { "src/parser.c" },
    generate_requires_npm = false,
  },
  filetype = "metascript",
}

-- 2. Set up filetype detection
vim.filetype.add({
  extension = {
    ms = "metascript",
    mts = "metascript",
  },
})

-- 3. Enable Tree-sitter highlighting
require('nvim-treesitter.configs').setup {
  ensure_installed = { "metascript" },
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
  },
  indent = {
    enable = true,
  },
}

-- 4. Optional: Custom highlights for macro keywords
vim.api.nvim_set_hl(0, "@keyword.directive", { fg = "#56B6C2", bold = true }) -- Cyan for @macros
vim.api.nvim_set_hl(0, "@function.macro", { fg = "#C678DD" }) -- Purple for macro names

-- 5. LSP setup (when mls is ready)
-- require('lspconfig').metascript.setup {
--   cmd = { '/path/to/mls' },
--   filetypes = { 'metascript' },
--   root_dir = function(fname)
--     return vim.fn.getcwd()
--   end,
-- }

print("Metascript syntax highlighting loaded!")
