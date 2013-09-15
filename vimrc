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
" Navigate vim and tmux more naturally
Bundle 'christoomey/vim-tmux-navigator'
" highlights the enclosing html/xml tags
Bundle 'Valloric/MatchTagAlways'
" A simple Vim plugin to switch segments of text with predefined replacements
Bundle 'AndrewRadev/switch.vim'
" Insert ends automatically for Ruby
Bundle 'tpope/vim-endwise'
" Solarized colorscheme
Bundle 'altercation/vim-colors-solarized'
" Send commands to other tmux sessions/windows/panes
Bundle 'jgdavey/tslime.vim'
" Multiple cursors in vim
Bundle 'terryma/vim-multiple-cursors'
" Align text
Bundle 'godlygeek/tabular'


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
set list listchars=tab:»·,trail:·
set list

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
  set guifont=Monaco:h14
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

map Q <Nop>
map K <Nop>

command! W w
command! Q q
command! Qall qall

nnoremap Y y$

nmap k gk
nmap j gj

cabbrev gs Gstatus
cabbrev ga Gwrite
cabbrev gc Gcommit

map <up> <C-W>+
map <down> <C-W>-
map <left> 3<C-W>>
map <right> 3<C-W><

map <return> :nohlsearch<cr>

map <leader><leader> <C-^>

"a
map <leader>a :CtrlPMixed<cr>
map <leader>ab :CtrlPBuffer<cr>
map <leader>at :CtrlPTag<cr>
map <leader>atb :CtrlPBufTag<cr>
map <leader>al :CtrlPLine<cr>
map <leader>ac :CtrlPChange<cr>
"b
map <leader>b :call ToggleBackgroundColor()<cr>
"c
" comment closing HTML tag
map <leader>ct my^lyy%p/classf"v0c.f"D:s/ /./eg<cr>gcckJ:nohlsearch<cr>`y
map <leader>cc :CtrlPClearAllCache<cr>
"d
" delete wrapping HTML tag
map <leader>dt ^lma%mb'ajV'bk<'add'bdd
map <leader>do ma^/do<cr>ciw{<esc>lxJJ$ciw}<esc>`a
"e
map <leader>ea :tabnew ~/dropbox/code/toolsharpeninglist.md<cr>
map <leader>ee :tabnew ~/dropbox/code/vimcheatsheet.md<cr>
map <leader>ev :tabnew $MYVIMRC<cr>
map <leader>es :UltiSnipsEdit
"f
map <leader>f :CtrlP<cr>
map <leader>F :CtrlPTag<cr>
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
map <leader>mh yypVr=k
map <leader>m2h yypVr-k
"n
map <leader>nt :NERDTreeToggle<cr>
"o
map <leader>o :only<cr>
"p
map <Leader>p :set paste<CR>o<esc>"*]p:set nopaste<cr>
"q
map <leader>q :q<cr>
map <leader>Q :qall<cr>
" map <leader>Q :lclose<cr>:cclose<cr>
"r
map <leader>rn :call RenameFile()<cr>
map <leader>re :%s/\r\(\n\)/\1/eg<cr>:retab<cr>:%s/\s\+$//e<cr>
map <leader>rt :!ctags -R --exclude=.svn --exclude=.git --exclude=log --exclude=tmp --exclude=vendor *<cr>:CtrlPTag<cr>
"s
map <leader>S :source $MYVIMRC<cr>:nohlsearch<cr>
map <leader>sw :Switch<cr>
"t
map <leader>t :call RunCurrentTest()<cr>
"u
"v
"w
map <leader>w :w<cr>
map <leader>W :wq<cr>
"x
map <leader>x :set filetype=
"y
map <leader>y "*y
"z
map <leader>z :call CorrectSpelling()<cr>

" ----------------------------------------
" Plugin configs
" ----------------------------------------
let g:ctrlp_map = '<c-f>'
let g:ctrlp_cmd = 'CtrlP'

" Sane Ignore For ctrlp
let g:ctrlp_custom_ignore = {
  \ 'dir': '\.git$\|\.hg$\|\.svn$\|\.yardoc\|public\/images\|public\/system\|data\|log\|tmp$\|vendor\/rails\|_site',
  \ 'file': '\.exe$\|\.so$\|\.dat$\|\.exe$\|\.so$\|\.dll$\|\.png$\|\.jpg$\|\.jpeg$\|\.gif$\|\.psd$\|\.css$'
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
    \   ['foo', 'bar', 'baz'],
    \   ['block', 'inline-block', 'inline']
    \ ]

autocmd FileType sml set commentstring=(*\ %s\ *)

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
  if FilenameIncludes('\.sml')
    call OpenFileInMosMl()
    return
  elseif FilenameIncludes('_spec')
    call SetTestFile()

    if FilenameIncludes('\.rb')
      if InRailsApp()
        call SetTestRunner("zeus rspec")
      else
        call SetTestRunner("rspec")
      endif
    elseif FilenameIncludes('\.coffee')
      call SetTestRunner('karma run')
    elseif FilenameIncludes('\.js')
      call SetTestRunner('karma run')
    endif

    exec g:bjo_test_runner g:bjo_test_file
  endif
endfunction

function! SetTestFile()
  let g:bjo_test_file=@%
endfunction

function! SetTestRunner(runner)
  let command = a:runner

  if InRailsApp()
    let command = 'bin/' . command
  endif

  let command = 'clear; ' . command

  if InTmux() && NumberOfTmuxPanes() > 1
    let command = 'Tmux ' . command
  else
    let command = '!' . command
  endif

  let g:bjo_test_runner = command
endfunction

function! InTmux()
  silent exec '!in_tmux'
  exec "redraw!"

  if v:shell_error
    return 0
  else
    return 1
  endif
endfunction

function! NumberOfTmuxPanes()
  return system('number_of_tmux_panes')
endfunction

function! InRailsApp()
  return filereadable("app/controllers/application_controller.rb")
endfunction

function! FilenameIncludes(pattern)
  return match(expand('%:p'), a:pattern) != -1
endfunction

function! MosMlRunning()
  silent exec '!mosml_running'
  exec "redraw!"

  if v:shell_error
    return 0
  else
    return 1
  endif
endfunction

function! OpenFileInMosMl()
  if NumberOfTmuxPanes() == 1
    silent exec '!tmux split-window -h'
    silent exec '!tmux last-pane'
  elseif MosMlRunning()
    exec "Tmux quit();"
  endif

  exec "Tmux clear; mosml " . expand('%:p')
endfunction

source ~/.after.vim
