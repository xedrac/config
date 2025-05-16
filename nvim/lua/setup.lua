local ensure_packer = function()
	local fn = vim.fn
	local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'
	if fn.empty(fn.glob(install_path)) > 0 then
		fn.system({ 'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path })
		vim.cmd [[packadd packer.nvim]]
		return true
	end
	return false
end

local packer_bootstrap = ensure_packer()

require('packer').startup(function(use)
	use 'wbthomason/packer.nvim'
	use 'neovim/nvim-lspconfig'
	use 'navarasu/onedark.nvim'
	use { 'nvim-treesitter/nvim-treesitter',
		run = ':TSUpdate'
	}
	use 'echasnovski/mini.nvim'
	use { 'nvim-telescope/telescope.nvim', tag = '0.1.8', -- branch = '0.1.x'
		requires = { { 'nvim-lua/plenary.nvim' } }
	}
	use {'nvim-telescope/telescope-fzf-native.nvim', run = 'make' }

	-- Unless you are still migrating, remove the deprecated commands from v1.x
	vim.cmd([[ let g:neo_tree_remove_legacy_commands = 1 ]])

	use { "nvim-neo-tree/neo-tree.nvim",
		branch = "v2.x",
		requires = {
			"nvim-lua/plenary.nvim",
			"nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
			"MunifTanjim/nui.nvim",
		}
	}
	use { 'akinsho/bufferline.nvim', tag = "*", requires = 'nvim-tree/nvim-web-devicons' } -- list of buffers at top
	use { 'nvim-lualine/lualine.nvim',
		requires = { 'nvim-tree/nvim-web-devicons', opt = true } -- pretty status line
	}
	use { 'numToStr/Comment.nvim',
		config = function() require('Comment').setup() end
	}
	use { 'lewis6991/spaceless.nvim',
		config = function() require'spaceless'.setup() end
	}
    --use 'vlime/vlime'
    --use 'bhurlow/vim-parinfer'
    use 'gpanders/nvim-parinfer'

    --use 'neovim/nvim-lspconfig'
    use 'hrsh7th/cmp-nvim-lsp'
    use 'hrsh7th/cmp-buffer'
    use 'hrsh7th/cmp-path'
    use 'hrsh7th/cmp-cmdline'
    use 'hrsh7th/nvim-cmp'

	-- other plugins go here...

	if packer_bootstrap then
		require('packer').sync()
	end
end)


-- vim.cmd("colorscheme doom-one")



local cmp = require 'cmp'
  cmp.setup({
    --snippet = {
    --  -- REQUIRED - you must specify a snippet engine
    --  expand = function(args)
    --    vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
    --    -- require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
    --    -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
    --    -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
    --    -- vim.snippet.expand(args.body) -- For native neovim snippets (Neovim v0.10+)

    --    -- For `mini.snippets` users:
    --    -- local insert = MiniSnippets.config.expand.insert or MiniSnippets.default_insert
    --    -- insert({ body = args.body }) -- Insert at cursor
    --    -- cmp.resubscribe({ "TextChangedI", "TextChangedP" })
    --    -- require("cmp.config").set_onetime({ sources = {} })
    --  end,
    --},
    window = {
      -- completion = cmp.config.window.bordered(),
      -- documentation = cmp.config.window.bordered(),
    },
    mapping = cmp.mapping.preset.insert({
      ['<C-b>'] = cmp.mapping.scroll_docs(-4),
      ['<C-f>'] = cmp.mapping.scroll_docs(4),
      ['<C-Space>'] = cmp.mapping.complete(),
      ['<C-e>'] = cmp.mapping.abort(),
      ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    }),
    sources = cmp.config.sources({
      { name = 'nvim_lsp' },
      { name = 'vsnip' }, -- For vsnip users.
      -- { name = 'luasnip' }, -- For luasnip users.
      -- { name = 'ultisnips' }, -- For ultisnips users.
      -- { name = 'snippy' }, -- For snippy users.
    }, {
      { name = 'buffer' },
    })
  })

  -- To use git you need to install the plugin petertriho/cmp-git and uncomment lines below
  -- Set configuration for specific filetype.
  --[[ cmp.setup.filetype('gitcommit', {
    sources = cmp.config.sources({
      { name = 'git' },
    }, {
      { name = 'buffer' },
    })
 })
 require("cmp_git").setup() ]]--

  -- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline({ '/', '?' }, {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {
      { name = 'buffer' }
    }
  })

  -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline(':', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
      { name = 'path' }
    }, {
      { name = 'cmdline' }
    }),
    matching = { disallow_symbol_nonprefix_matching = false }
  })

  -- Set up lspconfig.
  --local capabilities = require('cmp_nvim_lsp').default_capabilities()
  ---- Replace <YOUR_LSP_SERVER> with each lsp server you've enabled.
  --require('lspconfig')['<YOUR_LSP_SERVER'].setup {
  --  capabilities = capabilities
  --}
