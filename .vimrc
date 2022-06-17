" Disable compatibility with vi
set nocompatible

" Disable backups
set nobackup

" Configure history
set history=1000

" Configure general editing experience
syntax on
set number
set cursorline
set showmode
set ruler " show line and column position of cursor
set fileencoding=utf-8
set encoding=utf-8

" Configure text editing settings
set shiftwidth=2

set tabstop=2
set expandtab

" Configure search behavior
set incsearch
set ignorecase
set smartcase " overrides ignorecase if searching for capitalized letters
set showmatch

" Add some smart detection for filetype
filetype on
filetype plugin on
filetype indent on

" Configure completion menu
set wildmenu " shows after pressing TAB
set wildmode=list:longest
set wildignore=*.jpg,*.png,*.gif,*.img,*.pdf

" PLUGINS ------------------------------------------------------------------ {{{

" Prerequisite: install locally to use vim-plug for plugin management
" curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
"    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

call plug#begin('~/.vim/plugged')

  Plug 'preservim/nerdtree'

  Plug 'jceb/vim-orgmode'

call plug#end()

" }}}


" VIMSCRIPT ---------------------------------------------------------------- {{{
" }}}


" STATUS LINE ------------------------------------------------------------ {{{

" Clear status line when vimrc is reloaded.
set statusline=

" Status line left side.
set statusline+=\ %F\ %M\ %Y\ %R

" Use a divider to separate the left side from the right side.
set statusline+=%=

" Status line right side.
set statusline+=\ ascii:\ %b\ hex:\ 0x%B\ row:\ %l\ col:\ %c\ percent:\ %p%%

" Show the status on the second to last line.
set laststatus=2

" }}}
