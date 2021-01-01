set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
"
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'

Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/nerdcommenter'
Plugin 'joshdick/onedark.vim'
Plugin 'ryanoasis/vim-devicons'
Plugin 'vim-airline/vim-airline'
Plugin 'ap/vim-css-color'
Plugin 'arcticiceStudio/nord-vim'
Plugin 'frazrepo/vim-rainbow'
Plugin 'yggdroot/indentline'

Plugin 'junegunn/fzf'
Plugin 'neoclide/coc.nvim'
Plugin 'raimondi/delimitmate'

Plugin 'pangloss/vim-javascript'

call vundle#end()            " required
filetype plugin indent on    " requirede

" Settings
let mapleader = ' ' 
syntax enable
set t_Co=256
set number
colorscheme nord
" set conceallevel=3

" Tabs
set expandtab
set smarttab
set shiftwidth=2
set tabstop=2

" Nerd Tree
" autocmd vimenter * NERDTree
map <C-n> :NERDTreeToggle<CR>
let NERDTreeShowHidden=1

" Mouse scrolling
set mouse=nicr

" Splits and tabs
set splitbelow splitright

nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Airline symbols
let g:airline_powerline_fonts=1

" Source COC
source $HOME/.config/nvim/plug-config/coc.vim

" Rainbow 
let g:rainbow_active = 1
