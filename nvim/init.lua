---------------------------------------
-- Install packer and plugins, if necessary
--------------------------------------------
require('setup')

-- Setup language servers.
local lspconfig = require('lspconfig')
lspconfig.hls.setup { cmd = { 'haskell-language-server-wrapper', '--lsp' } }
--lspconfig.rust_analyzer.setup { settings = { ['rust-analyzer'] = {} } } -- Server-specific settings. See `:help lspconfig-setup`
--lspconfig.rust_analyzer.setup { cmd = { 'rustup run stable rust-analyzer' } }
lspconfig.rust_analyzer.setup {}
lspconfig.clangd.setup {}
lspconfig.cmake.setup {}
lspconfig.dhall_lsp_server.setup {}
lspconfig.dockerls.setup {}
lspconfig.pylsp.setup {}
-- lspconfig.lua_ls.setup {}
lspconfig.bashls.setup {}
lspconfig.asm_lsp.setup {}
lspconfig.awk_ls.setup {}
lspconfig.dotls.setup {}
lspconfig.qml_lsp.setup {}
lspconfig.vhdl_ls.setup {}
--lspconfig.slint_lsp.setup {}
lspconfig.racket_langserver.setup {}

-- require('telescope').load_extension('fzy_native')

-- Setup improved syntax highlighting
-- See: https://github.com/nvim-treesitter/nvim-treesitter#supported-languages
--      Use :TSInstall <lang> to add support for a language
require('nvim-treesitter.configs').setup {
	highlight = {
		enable = true,
		additional_vim_regex_highlighting = false,
	},
}

-- Setup color theme
require('onedark').setup {
	--style = 'darker'
	style = 'warmer'
}
require('onedark').load()

-- Enable command completion
require('mini.completion').setup()

-- Enable line at top to show open buffers
vim.opt.termguicolors = true
require("bufferline").setup {
	options = {
		diagnostics = "nvim_lsp",
		separator_style = "slant",
	}
}

require('lualine').setup {}
require('Comment').setup {}

-- Is this really necessary?  Why doesn't lspconfig for bashls do this for me?
vim.api.nvim_create_autocmd('FileType', {
	pattern = 'sh',
	callback = function()
		vim.lsp.start({
			name = 'bash-language-server',
			cmd = { 'bash-language-server', 'start' },
		})
	end,
})

-- Highlight on yank
vim.api.nvim_create_autocmd('TextYankPost', {
  group = vim.api.nvim_create_augroup('highlight_yank', {}),
  desc = 'Hightlight selection on yank',
  pattern = '*',
  callback = function()
    vim.highlight.on_yank { higroup = 'IncSearch', timeout = 200 }
  end,
})

-- Enable hybrid line numbering
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.autoindent = true
vim.opt.smartindent = true
vim.opt.expandtab = true
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.wrap = false
--vim.opt.listchars = 'tab:▷▷⋮'
--vim.opt.listchars = {eol = '↲', tab = '▸ ', trail = '·'}
vim.opt.listchars = {tab = '▸ ', trail = '·'}
vim.opt.list = true
-- vim.opt.clipboard = 'unnamed'


-- General keybindings
vim.keymap.set('n', ':', ';', {})
vim.keymap.set('n', ';', ':', {})
vim.keymap.set('n', '<space>rc', ':source ~/.config/nvim/init.lua<cr>', {})
vim.keymap.set('n', '<space>tw', ':set invlist<cr>', {})
vim.keymap.set('n', '<space>bp', ':bp<cr>', {})
vim.keymap.set('n', '<space>bn', ':bn<cr>', {})
vim.keymap.set('n', '<C-0>', ':Neotree toggle reveal_force_cwd<cr>', {})
vim.keymap.set('n', '<space>ts', ':Neotree toggle reveal_force_cwd<cr>', {})


-- Telescope keybindings
local ts = require('telescope.builtin')
vim.keymap.set('n', '<space>ou', ts.find_files, {})
vim.keymap.set('n', '<space>oi', ts.live_grep, {})
vim.keymap.set('n', '<space>ob', ts.buffers, {})
vim.keymap.set('n', '<space>oh', ts.help_tags, {})
vim.keymap.set('n', '<space>ok', ts.keymaps, {})
vim.keymap.set('n', '<space>/', ts.current_buffer_fuzzy_find, {})


-- See `:help vim.diagnostic.*` for documentation on any of the below functions
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist)

-- Use LspAttach autocommand to only map the following keys
-- after the language server attaches to the current buffer
vim.api.nvim_create_autocmd('LspAttach', {
	group = vim.api.nvim_create_augroup('UserLspConfig', {}),
	callback = function(ev)
		-- Enable completion triggered by <c-x><c-o>
		vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

		-- Buffer local mappings.
		-- See `:help vim.lsp.*` for documentation on any of the below functions
		local opts = { buffer = ev.buf }
		vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
		vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
		vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
		vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
		vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
		vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, opts)
		vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, opts)
		vim.keymap.set('n', '<space>wl', function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end, opts)
		vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, opts)
		vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
		vim.keymap.set({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action, opts)
		vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
		vim.keymap.set('n', '<space>f', function() vim.lsp.buf.format { async = true } end, opts)
	end,
})
