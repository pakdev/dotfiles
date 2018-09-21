let mapleader = " "

call plug#begin('~/.vim/plugged')

if has('nvim')
        Plug 'Shougo/defx.nvim', { 'do': ':UpdateRemotePlugins' }
else
        Plug 'Shougo/defx.nvim'
        Plug 'roxma/nvim-yarp'
        Plug 'roxma/vim-hug-neovim-rpc'
endif

Plug 'dracula/vim', {'as':'dracula'}
Plug 'tmhedberg/SimpylFold'
Plug 'vim-syntastic/syntastic'
Plug 'kien/ctrlp.vim'
Plug 'Valloric/YouCompleteMe'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

call plug#end()

" Plugin options
colorscheme dracula
let g:airline#extensions#tabline#enabled = 1

" Universal settings
set expandtab
set tabstop=4
set foldmethod=syntax
set clipboard=unnamedplus

" Enable filetype specific configurations
filetype plugin indent on

" {{{Mappings
nnoremap : ;
nnoremap ; :
vnoremap ; :

" Easy window management
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Easy buffer management
nnoremap <leader>n :bn<cr>
nnoremap <leader>p :bp<cr>
nnoremap <leader>d :bd<cr>

" Folding
nnoremap <leader> za

" Defx
autocmd FileType defx call s:defx_my_settings()
function! s:defx_my_settings() abort
        " Define mappings
        nnoremap <silent><buffer><expr> <CR>    defx#do_action('open')
        nnoremap <silent><buffer><expr> c       defx#do_action('copy')
        nnoremap <silent><buffer><expr> m       defx#do_action('move')
        nnoremap <silent><buffer><expr> p       defx#do_action('paste')
        nnoremap <silent><buffer><expr> l       defx#do_action('open')
        nnoremap <silent><buffer><expr> E       defx#do_action('open', 'vsplit')
        nnoremap <silent><buffer><expr> P       defx#do_action('open', 'pedit')
        nnoremap <silent><buffer><expr> K       defx#do_action('new_directory')
        nnoremap <silent><buffer><expr> N       defx#do_action('new_file')
        nnoremap <silent><buffer><expr> d       defx#do_action('remove')
        nnoremap <silent><buffer><expr> r       defx#do_action('rename')
        nnoremap <silent><buffer><expr> x       defx#do_action('execute_system')
        nnoremap <silent><buffer><expr> .       defx#do_action('toggle_ignored_files')
        nnoremap <silent><buffer><expr> h       defx#do_action('cd', ['..'])
        nnoremap <silent><buffer><expr> ~       defx#do_action('cd')
        nnoremap <silent><buffer><expr> q       defx#do_action('quit')
        nnoremap <silent><buffer><expr> <Space> defx#do_action('toggle_select') . 'j'
        nnoremap <silent><buffer><expr> *       defx#do_action('toggle_select_all')
        nnoremap <silent><buffer><expr> j       line('.') == line('$') ? 'gg' : 'j'
        nnoremap <silent><buffer><expr> k       line('.') == 1 ? 'G' : 'k'
        nnoremap <silent><buffer><expr> <C-l>   defx#do_action('redraw')
        nnoremap <silent><buffer><expr> <C-g>   defx#do_action('print')
endfunction
"}}}
