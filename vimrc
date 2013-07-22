" ----------------------------------------
" Environment
" ----------------------------------------

set nocompatible

" ----------------------------------------
" Vundle and plugin stuff
" ----------------------------------------
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

" Surround stuff
Bundle 'tpope/vim-surround'
" Autoclose brackets
Bundle 'Townk/vim-autoclose'
" Fuzzy file matching
Bundle 'kien/ctrlp.vim'
" Ack for vim
Bundle 'mileszs/ack.vim'
" Repeat commands like surround
Bundle 'tpope/vim-repeat'
" Tree file browser
Bundle 'scrooloose/nerdtree'
" Comment mappings
Bundle 'tpope/vim-commentary'
" Git wrapper for vim
Bundle 'tpope/vim-fugitive'
" Extend % to also work with HTML, Ruby, and more
Bundle 'edsono/vim-matchit'
" Snippets
Bundle 'SirVer/ultisnips'
" Super tab completion
Bundle 'ervandew/supertab'
" Better HTML5 syntax highlighting
Bundle 'othree/html5-syntax.vim'
" Coffeescript syntax highlighting
Bundle 'kchmck/vim-coffee-script'
" Markdown syntax highlighting
Bundle 'plasticboy/vim-markdown'
" SASS syntax highlighting
Bundle 'cakebaker/scss-syntax.vim'
" Ruby stuff
Bundle 'vim-ruby/vim-ruby'
" HTML expansion awesome plugin thing
Bundle 'mattn/zencoding-vim'
" Make vim into a Rails IDE
Bundle 'tpope/vim-rails.git'
" Improved indentation for javascript
Bundle "pangloss/vim-javascript"
" Quickly align things
Bundle 'godlygeek/tabular'
" Navigate vim and tmux more naturally
Bundle 'christoomey/vim-tmux-navigator'
" highlights the enclosing html/xml tags
Bundle 'Valloric/MatchTagAlways'
" A simple Vim plugin to switch segments of text with predefined replacements
Bundle 'AndrewRadev/switch.vim'
" Insert ends automatically for Ruby
Bundle 'tpope/vim-endwise'


" ----------------------------------------
" General
" ----------------------------------------

filetype plugin indent on
syntax enable
set shell=bash
set history=1000
set undolevels=1000
set autowrite
set relativenumber
set numberwidth=4
set autoread
set timeoutlen=1000
set hidden
set tabstop=2
set shiftwidth=2
set softtabstop=2
set expandtab
set smartindent
set autoindent
set foldenable
set foldmethod=syntax
set foldcolumn=0
set foldlevelstart=99
set foldlevel=20
set backspace=indent,eol,start
set wildmenu
set wildmode=list:longest
set ignorecase
set smartcase
set nobackup
set noswapfile
set nospell
set spelllang=en_us
set statusline=%<%f\ (%{&ft})\ %-4(%m%)%=%-19(%3l,%02c%03V%)
set statusline+=%{fugitive#statusline()}
set tags=./tags,tags;$HOME
set splitbelow
set splitright
set listchars=tab:>\ ,trail:•,extends:>,precedes:<,nbsp:+
set list
set winwidth=84
set winheight=5
set winminheight=5
set winheight=999

" ----------------------------------------
" Auto commands
" ----------------------------------------
" Jump to last cursor position unless it's invalid or in an event handler
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif

autocmd FileType scss,sass,css,html setlocal foldmethod=indent
au BufRead,BufNewFile *.scss set filetype=scss

autocmd InsertEnter * if !exists('w:last_fdm') | let w:last_fdm=&foldmethod | setlocal foldmethod=manual | endif
autocmd InsertLeave,WinLeave * if exists('w:last_fdm') | let &l:foldmethod=w:last_fdm | unlet w:last_fdm | endif

" ----------------------------------------
" Vim UI
" ----------------------------------------

set background=dark
colorscheme github
set t_Co=256
if has("gui_running")
  set guioptions=egmrt
  set guifont=Ubuntu\ Mono:h20
  set guioptions-=r
else
endif
set cmdheight=1
set scrolloff=3
set cursorline
set ruler
set showcmd
set showmode
set laststatus=2
set wrap
set incsearch
set hlsearch
set visualbell
set linebreak

" ----------------------------------------
" Key (re)mappings
" ----------------------------------------

let mapleader = ','
map <cr> :w<cr>
map <space> :

map Q <Nop>
map K <Nop>

nnoremap Y y$

nmap k gk
nmap j gj

cabbrev gs Gstatus
cabbrev ga Gwrite
cabbrev gc Gcommit

map <leader><leader> <C-^>

nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

map <cr> :nohlsearch<cr>

"a
"b
map <leader>b :call ToggleBackgroundColor()<cr>
"c
map <leader>ct my^lyy%p/classf"v0c.f"D:s/ /./eg<cr>gcckJ:nohlsearch<cr>`y
"d
map <leader>dt ^lma%mb'ajV'bk<'add'bdd
"e
map <leader>ea :tabnew ~/dropbox/code/toolsharpeninglist.md<cr>
map <leader>ee :tabnew ~/dropbox/code/vimcheatsheet.md<cr>
map <leader>ev :tabnew $MYVIMRC<cr>
map <leader>es :UltiSnipsEdit
"f
map <leader>f :CtrlPClearCache<cr>:CtrlP<cr>
map <leader>F :!ctags -R<cr>:CtrlPTag<cr>
"g
map <leader>gg :topleft 20 :split Gemfile<cr>
map <leader>gr :topleft 20 :split config/routes.rb<cr>
map <leader>gv :CtrlPClearCache<cr>:CtrlP app/views<cr>
map <leader>gc :CtrlPClearCache<cr>:CtrlP app/controllers<cr>
map <leader>gm :CtrlPClearCache<cr>:CtrlP app/models<cr>
map <leader>ga :CtrlPClearCache<cr>:CtrlP app/assets<cr>
map <leader>gs :CtrlPClearCache<cr>:CtrlP specs<cr>
"h
map <leader>h :nohlsearch<cr>
map <leader>ha <esc>:call ToggleHardMode()<CR>
"i
"j
"k
"l
"m
"n
map <leader>nt :NERDTreeToggle<cr>
"o
"p
map <Leader>p :set paste<CR>o<esc>"*]p:set nopaste<cr>
"q
map <leader>qq :q!<cr>
map <leader>q :lclose<cr>
"r
map <leader>rn :call RenameFile()<cr>
"s
map <leader>S :source $MYVIMRC<cr>:nohlsearch<cr>
map <leader>sw :Switch<cr>
"t
map <Leader>t :w<cr>:call RunCurrentTest()<CR>
"u
"v
"w
map <leader>w :bufdo w<cr>
map <leader>wo :only<cr>
"x
map <leader>x :set filetype=
"y
map <leader>y "*y
"z
map <leader>z :call CorrectSpelling()<cr>




" ----------------------------------------
" Plugin configs
" ----------------------------------------
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)$',
  \ 'file': '\v\.(exe|so|dll|png|jpg|gif|jpeg|psd|css)$',
  \ '_site': '_site'
  \ }

let g:UltiSnipsEditSplit = 'horizontal'
let g:UltiSnipsSnippetDirectories = ["snippets"]

let g:switch_custom_definitions =
    \ [
    \   ['white', 'black'],
    \   ['right', 'left'],
    \   ['top', 'bottom'],
    \   ['red', 'blue'],
    \   ['width', 'height'],
    \   ['min', 'max'],
    \   ['require', 'require_relative'],
    \   ['margin', 'padding'],
    \   ['block', 'inline-block', 'inline']
    \ ]

" ----------------------------------------
" Abbreviation
" ----------------------------------------

" When typing %% expand it into the path to the current file
cnoremap %% <C-R>=expand('%:h').'/'<cr>

" ----------------------------------------
" Functions
" ----------------------------------------

" Rename the current file
function! RenameFile()
    let old_name = expand('%')
    let new_name = input('New file name: ', expand('%'), 'file')
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        exec ':silent !rm ' . old_name
        redraw!
    endif
endfunction

function! CorrectSpelling()
  set spell
  normal 1z=
  set nospell
endfunction

" Toggle background color
function! ToggleBackgroundColor()
  if &background == 'dark'
    set background=light
  else
    set background=dark
  endif
endfunction

function! RunCurrentTest()
  let in_test_file = match(expand("%"), '\(.feature\|_spec.rb\|_test.rb\)$') != -1
  if in_test_file
    call SetTestFile()

    if match(expand('%'), '\.feature$') != -1
      call SetTestRunner("!zeus cucumber")
      exec g:bjo_test_runner g:bjo_test_file
    elseif match(expand('%'), '_spec\.rb$') != -1
      call SetTestRunner("!zeus rspec")
      exec g:bjo_test_runner g:bjo_test_file
    else
      call SetTestRunner("!ruby -Itest")
      exec g:bjo_test_runner g:bjo_test_file
    endif
  else
    exec g:bjo_test_runner g:bjo_test_file
  endif
endfunction

function! SetTestRunner(runner)
  let g:bjo_test_runner=a:runner
endfunction

function! RunCurrentLineInTest()
  let in_test_file = match(expand("%"), '\(.feature\|_spec.rb\|_test.rb\)$') != -1
  if in_test_file
    call SetTestFileWithLine()
  end

  exec "!bin/rspec" g:bjo_test_file . ":" . g:bjo_test_file_line
endfunction

function! SetTestFile()
  let g:bjo_test_file=@%
endfunction

function! SetTestFileWithLine()
  let g:bjo_test_file=@%
  let g:bjo_test_file_line=line(".")
endfunction

source ~/.after.vim
