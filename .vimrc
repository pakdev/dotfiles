let mapleader = " "

call plug#begin('~/.vim/plugged')

Plug 'dracula/vim', {'as':'dracula'}
Plug 'tmhedberg/SimpylFold'
Plug 'vim-syntastic/syntastic'
Plug 'scrooloose/nerdtree'
Plug 'jistr/vim-nerdtree-tabs'
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
nnoremap <space> za
"}}}
