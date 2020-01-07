"  Need to manually install some stuff:
"     vim-plug:  (Plugin manager for vim/neovim)
"         curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
"
"     pynvim:  (used by deoplete autocompletion plugin)
"         pip3 install neovim
"
"     git:      sudo dnf install git
"     bat:      sudo dnf install bat      (Used for colorized preview window when searching files)
"     ripgrep:  sudo dnf install ripgrep
"
"
" ----------------------------------------------------------------
" Plugins
"
" Run :PlugInstall to install the plugins
" Run :PlugUpdate to update to latest
" ----------------------------------------------------------------
call plug#begin('~/.config/nvim/plugged')


Plug 'drewtempelmeyer/palenight.vim'
let g:palenight_terminal_italics=1

Plug 'sheerun/vim-polyglot'
Plug 'scrooloose/nerdtree'
let g:NERDTreeWinSize=50

Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-airline/vim-airline'
let g:airline_theme = "palenight"

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
" Hide status line in fzf window
if has('nvim') && !exists('g:fzf_layout')
  autocmd! FileType fzf
  autocmd  FileType fzf set laststatus=0 noshowmode noruler
    \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler
endif
" Give the :Files command a preview window
"command! -bang -nargs=? -complete=dir Files
    "\ call fzf#vim#files(<q-args>, fzf#vim#with_preview({'options': ['--layout=reverse', '--info=inline']}), <bang>0)
command! -bang -nargs=? -complete=dir Files
    \ call fzf#vim#files(<q-args>, {'options': ['--layout=reverse', '--info=inline', '--preview', '~/.config/nvim/plugged/fzf.vim/bin/preview.sh {}']}, <bang>0)
command! -bang ProjectFiles call fzf#vim#files('~/projects/camera-build/subprojects/camera-apps', <bang>0)

Plug 'tomasr/molokai'
Plug 'kovisoft/paredit', { 'for': 'clojure' }
Plug 'iamcco/markdown-preview.nvim', { 'do': ':call mkdp#util#install()', 'for': 'markdown', 'on': 'MarkdownPreview' }
Plug 'bling/vim-bufferline'
let g:bufferline_echo = 0
let g:bufferline_show_bufnr = 0
let g:bufferline_modified = '*'

Plug 'tpope/vim-fugitive'

" If you get an error with deoplete, make sure to:  pip3 install neovim
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
let g:deoplete#enable_at_startup = 1

" LSP client
Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'bash install.sh' }
let g:LanguageClient_autoStart = 1
let g:LanguageClient_serverCommands = {
    \ 'rust': ['rustup', 'run', 'stable', 'ra_lsp_server'],
    \ 'cpp': ['clangd'],
    \ 'python': ['/usr/local/bin/pyls'],
    \ }
let g:LanguageClient_rootMarkers = {
    \ 'rust': ['Cargo.lock'],
    \ }

Plug 'preservim/nerdcommenter'
Plug 'sakhnik/nvim-gdb', { 'do': ':!./install.sh \| UpdateRemotePlugins' }


call plug#end()

" ----------------------------------------------------------------
" Vim settings
" ----------------------------------------------------------------
set number
set autoindent
set smartindent
"set lazyredraw
set hlsearch
set incsearch
set ignorecase smartcase
set shiftwidth=4
set tabstop=4
set expandtab smarttab
set noshowmode
set laststatus=2
set cursorline
hi CursorLine  cterm=NONE ctermbg=black ctermfg=NONE gui=NONE guibg=darkgray guifg=white
"set colorcolumn=120
set backupdir=/tmp//,.
set directory=/tmp//,.
set undodir=/tmp//,.
" Allow unsaved buffers to be hidden
set hidden
set showtabline=0
set background=dark
colorscheme palenight

if (has("termguicolors"))
  set termguicolors
endif


" ----------------------------------------------------------------
" Key mappings
" ----------------------------------------------------------------
let mapleader      = ' '
let maplocalleader = ' '

" editor
nnoremap <leader>Q :qa!<cr>
nmap <C-n> :NERDTreeToggle<CR>
nnoremap <leader>` :edit ~/.config/nvim/init.vim<cr>

" saving
nnoremap <C-S> :wa<cr>
nnoremap <C-s> :update<cr>
nnoremap <leader>bs :update<cr>

" buffer management
nnoremap <leader>bq :bd<cr>
nnoremap <leader>bQ :bd!<cr>
nnoremap <leader>bn :bn<cr>
nnoremap <leader>bp :bp<cr>
nnoremap <leader>bN :enew<cr>

" Window management
nmap <A-j> <C-w>w5j<C-w>w
nmap <A-k> <C-w>w5k<C-w>w

" LSP
nnoremap <F5> :call LanguageClient_contextMenu()<cr>
"nnoremap <silent> K :call LanguageClient#textDocument_hover()<cr>
nnoremap <leader>h :call LanguageClient#textDocument_hover()<cr>
"nnoremap <silent> gd :call LanguageClient#textDocument_definition()<cr>
nnoremap <leader>d :call LanguageClient#textDocument_definition()<cr>
nnoremap <leader>f :call LanguageClient#textDocument_formatting()<cr>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<cr>

" Searching
nnoremap <leader>p :Files<cr>
nnoremap <leader><S-p> :ProjectFiles<cr>
nnoremap <leader>s :Rg<cr>



" Rust specific bindings
au FileType rust set makeprg=cargo\ build
au FileType rust nmap <leader>b :term cargo build<cr>
"au FileType rust nmap <leader>t :!cargo test<cr>
au FileType rust nmap <leader>t :term cargo test<cr>
au FileType rust nmap <leader>r :term RUST_BACKTRACE=1 cargo run<cr>
