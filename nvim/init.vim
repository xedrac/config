" ----------------------------------------------------------------
" Plugins
"
" Run :PlugInstall to install the plugins
" Run :PlugUpdate to update to latest
" ----------------------------------------------------------------
call plug#begin('~/.config/nvim/plugged')

Plug 'drewtempelmeyer/palenight.vim'
Plug 'joshdick/onedark.vim'
Plug 'sheerun/vim-polyglot'

Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'tomasr/molokai'
Plug 'kovisoft/paredit', { 'for': 'clojure' }
"Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'iamcco/markdown-preview.nvim', { 'do': ':call mkdp#util#install()', 'for': 'markdown', 'on': 'MarkdownPreview' }
Plug 'bling/vim-bufferline'
Plug 'tpope/vim-fugitive'

" If you get an error with deoplete, make sure to:  pip3 install neovim
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

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

"Plug 'airblade/vim-gitgutter'
Plug 'preservim/nerdcommenter'


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

" saving
nnoremap <leader>S :wa<cr>
nnoremap <C-s> :update<cr>
nnoremap <leader>s :update<cr>
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

nnoremap <F5> :call LanguageClient_contextMenu()<CR>
"nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
nnoremap <leader>h :call LanguageClient#textDocument_hover()<CR>
"nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <leader>d :call LanguageClient#textDocument_definition()<CR>
nnoremap <leader>f :call LanguageClient#textDocument_formatting()<CR>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>

" Rust specific bindings
au FileType rust set makeprg=cargo\ build
au FileType rust nmap <leader>b :term cargo build<cr>
"au FileType rust nmap <leader>t :!cargo test<cr>
au FileType rust nmap <leader>t :term cargo test<cr>
au FileType rust nmap <leader>r :term RUST_BACKTRACE=1 cargo run<cr>


" ----------------------------------------------------------------
" Plugin configurations
"  ----------------------------------------------------------------
"let g:airline#extensions#tabline#enabled = 1
"let g:airline_theme='bubblegum'
let g:airline_theme = "palenight"
let g:palenight_terminal_italics=1
let g:bufferline_echo = 0
let g:bufferline_show_bufnr = 0
let g:bufferline_modified = '*'

let g:ctrlp_working_path_mode = 'ra'
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn|npm)$',
  \ 'file': '\v\.(exe|so|dll)$',
  \ 'link': 'some_bad_symbolic_links',
  \ }
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']
"let g:ctrlp_user_command = 'find %s -type f -path "*/.git*" -prune -o -print'

let g:deoplete#enable_at_startup = 1



