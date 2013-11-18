" ==== Plugins ===================== {{{
" ==================================

if has('vim_starting')
  set nocompatible
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#rc(expand('~/.vim/bundle/'))

" Let NeoBundle manage NeoBundle
NeoBundleFetch 'Shougo/neobundle.vim'

NeoBundle 'Shougo/vimproc', { 'build': {
  \   'windows': 'make -f make_mingw32.mak',
  \   'cygwin': 'make -f make_cygwin.mak',
  \   'mac': 'make -f make_mac.mak',
  \   'unix': 'make -f make_unix.mak',
  \ } }

" Unite
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/unite-outline'
NeoBundle 'tsukkee/unite-tag'
NeoBundle 'ujihisa/unite-rake'
NeoBundle 'tsukkee/unite-help'

" Utils
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-commentary'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-abolish'
NeoBundle 'tpope/vim-dispatch'
NeoBundle 'edsono/vim-matchit'
NeoBundle 'Valloric/MatchTagAlways'
NeoBundle 'AndrewRadev/switch.vim'
NeoBundle 'terryma/vim-multiple-cursors'
NeoBundle 'godlygeek/tabular'
NeoBundle 'vim-scripts/Emmet.vim'
NeoBundle 'ervandew/supertab'
NeoBundle 'Shougo/vimfiler.vim'
NeoBundle 'vim-scripts/ZoomWin'
NeoBundle 'chrisbra/NrrwRgn'
NeoBundle 'vim-scripts/scratch.vim'
NeoBundle 'Raimondi/delimitMate/'

" UI
NeoBundle 'bling/vim-airline'
NeoBundle 'airblade/vim-gitgutter'

" Snippets
NeoBundle 'SirVer/ultisnips'

" Filetypes
NeoBundle 'vim-ruby/vim-ruby'
NeoBundle 'othree/html5-syntax.vim'
NeoBundle 'kchmck/vim-coffee-script'
NeoBundle 'plasticboy/vim-markdown'
NeoBundle 'wlangstroth/vim-haskell'
NeoBundle 'hspec/hspec.vim'
NeoBundle 'cakebaker/scss-syntax.vim'

" Colors
NeoBundle 'altercation/vim-colors-solarized'
NeoBundle 'nanotech/jellybeans.vim'

" Tmux
NeoBundle 'christoomey/vim-tmux-navigator'
NeoBundle 'jgdavey/tslime.vim'
NeoBundle 'tomasr/molokai'

NeoBundleCheck

" }}}

" ==== General ===================== {{{
" ==================================

filetype plugin indent on
syntax enable
set shell=/bin/zsh                " Use zsh as shell
set history=1000                  " Sets how many lines of history vim has to remember
set undolevels=1000               " How many steps of undo history vim should remember
set nonumber                      " Don't show line numbers
set relativenumber                " Use relative line numbers
set numberwidth=4                 " The width of the number column
set timeoutlen=1000               " Time to wait before completing a key sequence
set timeout                       " Lower the delay of escaping out of other modes
set timeoutlen=1000               " ...
set ttimeoutlen=0                 " ...
set hidden                        " Don't unload buffers when leaving them
set tabstop=2                     " Number of spaces a <tab> counts for
set shiftwidth=2                  " Number of spaces to use when indenting
set softtabstop=2                 " Number of spaces a <tab> counts for when inserting
set expandtab                     " Indent with spaces
set smartindent                   " Auto indent new lines
set foldenable                    " Enable folds
set foldmethod=indent             " Fold by indentation
set foldlevelstart=99             " Fold by indentation
set backspace=indent,eol,start    " Backspace over everything in insert mode
set laststatus=2                  " Always show the status line
set wildmenu                      " Enable command-line like completion
set wildmode=list:longest         " List all matches and complete till longest common string
set smartcase                     " Do case insensitive search unless there are capital letters
set nobackup                      " Don't make backups
set noswapfile                    " Don't make swap files
set nospell                       " Disable spell checking
set spelllang=en_us               " Use english US for spell checking
set tags=./tags,tags;$HOME        " Tell Vim where to look for tags files
set splitbelow                    " Open splits below
set splitright                    " Open splits to the right
set list                          " Show unprintable characters
set listchars=tab:▸\              " Char representing a tab
set listchars+=extends:❯          " Char representing an extending line
set listchars+=precedes:❮         " Char representing an extending line in the other direction
set listchars+=nbsp:␣             " Non breaking space
set listchars+=trail:·            " Show trailing spaces as dots
set showbreak=↪                   " Show wraped lines with this char
set t_Co=256                      " More colors please
set scrolloff=3                   " Min. lines to keep above or below the cursor when scrolling
set ruler                         " Show current cursor position
set noshowmode                    " Don't show current mode, let airline handle that
set nocursorline                  " Don't highlight the current line
set nocursorcolumn                " Don't highlight the current column
set scrolljump=5                  " Scroll more than one line
set wrap                          " Wrap long lines
set incsearch                     " Perform incremental searching
set hlsearch                      " Highlight search matches
set visualbell                    " Disable annoying beep
set linebreak                     " Don't break lines in the middle of words
set fillchars+=vert:\             " Don't show pipes in vertical splits
set background=light              " Tell Vim the color of my background

colorscheme solarized

" Some GUI specific settings
if has("gui_running")
  set guifont=Ubuntu\ Mono\ derivative\ Powerline:h16
  set guioptions-=r
  set cursorline
  set nonumber
  set relativenumber
  colorscheme macvim
  set background=light
endif

" }}}

" ==== Auto commands =============== {{{
" ==================================

augroup miscGroup
  autocmd!

  " Jump to last cursor position unless it's invalid or in an event handler
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

  autocmd FileType * match ErrorMsg '\%>100v.\+'
  autocmd FileType sml match ErrorMsg '\%>80v.\+'
  autocmd FileType mkd match none
  autocmd FileType text match none
  autocmd FileType html match none
  autocmd FileType eruby match none
  autocmd FileType unite match none
  autocmd FileType vimfiler match none
  autocmd FileType plaintex match none
  autocmd FileType tex match none

  autocmd FileType mkd setlocal spell nofoldenable
  autocmd FileType text setlocal spell nofoldenable

  autocmd FileType vim setlocal foldmethod=marker
augroup END

" }}}

" ==== Mappings ==================== {{{
" ==================================

" `noremap` means to make a nonrecursive mapping
" that means that vim will not take other mapping
" into account when doing your new map

" Disable useless and annoying keys
noremap Q <Nop>
noremap K <Nop>

" Don't wanna retrain my fingers
command! W w
command! Q q
command! Qall qall

" Make Y work as expected
nnoremap Y y$

" Intuitive movement over long lines
nnoremap k gk
nnoremap j gj

" Quickly leave insert mode, and safe at the same time
noremap <C-s> <esc>:w<CR>
inoremap <C-s> <esc>:w<CR>

" Resize windows with the arrow keys
noremap <up>    <C-W>+
noremap <down>  <C-W>-
noremap <left>  3<C-W>>
noremap <right> 3<C-W><

" Don't use those stupid arrow keys!
inoremap <up>    <nop>
inoremap <down>  <nop>
inoremap <left>  <nop>
inoremap <right> <nop>

" Spell correct current word
imap <c-z> <esc>,zea

" Always use very magic regex mode when searching
nnoremap / /\v
nnoremap ? ?\v

" Don't jump around when yanking paragraphs
noremap yip mayip`a
noremap yap mayap`a

" }}}

" ==== Leader Mappings ============= {{{
" ==================================

let mapleader = ','
let maplocalleader = '\\'

noremap <leader><leader> <C-^>

" Add semicolon at the end of the line
noremap <leader>; maA;<esc>`a

" Auto indent the whole buffer
noremap <leader>== magg=G`a

vnoremap <leader>= :Tabularize /

"-- a --"
" yank the whole buffer
noremap <leader>a maggyG`a
" yank the whole buffer into the system clipboard
noremap <leader>A maggVG"*y`a

"-- b --"
noremap <leader>b :call ToggleBackgroundColor()<cr>

"-- c --"
" comment closing HTML tag
noremap <leader>ct my^lyy%p/classf"v0c.f"D:s/ /./eg<cr>gcckJ:nohlsearch<cr>`y
noremap <leader>cd :cd %:p:h<cr>:pwd<cr>

"-- d --"
" delete wrapping HTML tag
noremap <leader>dt ^lma%mb'ajV'bk<'add'bdd
" convert ruby do/end to {}
noremap <leader>do ma^/do<cr>ciw{<esc>lxJJ$ciw}<esc>`a
noremap <leader>di :Dispatch 

"-- e --"
noremap <leader>ev :tabedit $MYVIMRC<cr>
noremap <leader>es :UltiSnipsEdit<cr>

"-- f --"

"-- g --"
noremap <leader>g :Git 
noremap <leader>gb :Gblame<cr>
noremap <leader>gc :Gcommit<cr>
noremap <leader>gd :Gdiff<cr>
noremap <leader>gp :Git push<cr>
noremap <leader>gr :Gremove<cr>
noremap <leader>gs :Gstatus<cr>
noremap <leader>ga :Gwrite<cr>
noremap <leader>gg :w<cr>:Gwrite<cr>:Gcommit -m 'update'<cr>:Git push<cr><cr>:e<cr>

hi Cursor ctermfg=red ctermbg=white

"-- h --"

"-- i --"

"-- j --"
noremap <leader>j :tabe<cr>:cd ~/hax/journal/entries<cr>:e.<cr>

"-- k --"

"-- l --"

"-- m --"
" For quickly making markdown headings
noremap <leader>mh yypVr=k
noremap <leader>m2h yypVr-k
" Format SML comments
vnoremap <leader>mlc ^:s/(\*/ */g<cr>gv:s/ \*)//g<cr>A *)<esc>gvo<esc>r(gvo<esc>:nohlsearch<cr>

"-- n --"
noremap <leader>n :set number!<cr>

"-- o --"
noremap <leader>o :only<cr>

"-- p --"
" Paste from system clipboard
noremap <leader>p <esc>o<esc>"+]p

"-- q --"
noremap <leader>q :cclose<cr>

"-- r --"
noremap <leader>rn :call RenameFile()<cr>
" Format buffer
noremap <leader>re :%s/\r\(\n\)/\1/eg<cr>:retab<cr>:%s/\s\+$//e<cr>
" Evaluate selection as ruby and insert the output
vnoremap <leader>r :!ruby<cr>
noremap <leader>rd :redraw!<cr>

"-- s --"
noremap <leader>sv :source $MYVIMRC<cr>:nohlsearch<cr>:e<cr>
noremap <leader>sw :Switch<cr>

"-- t --"
noremap <leader>t :w\|:call RunCurrentTests()<cr>
noremap <leader>T :w\|:call RunCurrentFile()<cr>

"-- u --"

"-- v --"

"-- w --"
noremap <leader>w :w<cr>
noremap <leader>W :wq<cr>

"-- x --"
noremap <leader>x :set filetype=

"-- y --"
" Yank to system clipboard
noremap <leader>y "+y

"-- z --"
noremap <leader>z :call CorrectSpelling()<cr>

" }}}

" ==== Misc Plugin Configs =============== {{{
" ==================================

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
    \   ['small', 'large'],
    \   ['block', 'inline-block', 'inline']
    \ ]

autocmd FileType sml set commentstring=(*\ %s\ *)

highlight link hspecDescribe Type
highlight link hspecIt Identifier
highlight link hspecDescription String

let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

highlight SignColumn ctermbg=black

" }}}

" ==== Unite ======================= {{{
" ==================================

call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#sorter_default#use(['sorter_rank'])

let g:unite_source_history_yank_enable = 1
let g:unite_force_overwrite_statusline = 0
if executable('ag')
  let g:unite_source_grep_command = 'ag'
  let g:unite_source_grep_default_opts = '--nogroup --nocolor --column'
  let g:unite_source_grep_recursive_opt = ''
endif

call unite#custom_source('file_rec,file_rec/async,file_mru,file,buffer,grep',
  \ 'ignore_pattern', join([
  \ '\.git/',
  \ '\.sass-cache/',
  \ '\vendor/',
  \ '\node_modules/',
  \ ], '\|'))

" Custom mappings for the unite buffer
autocmd FileType unite call s:unite_settings()
function! s:unite_settings()
  let b:SuperTabDisabled=1

  imap <buffer> <C-j> <Plug>(unite_select_next_line)
  imap <buffer> <C-k> <Plug>(unite_select_previous_line)
  imap <buffer> <c-a> <Plug>(unite_choose_action)

  imap <silent><buffer><expr> <C-s> unite#do_action('split')
  imap <silent><buffer><expr> <C-v> unite#do_action('vsplit')
  imap <silent><buffer><expr> <C-t> unite#do_action('tabopen')

  nmap <buffer> <ESC> <Plug>(unite_exit)
endfunction

" The prefix key
nnoremap [unite] <Nop>
nmap <space> [unite]

" General purpose
nnoremap [unite]<space> :Unite -no-split -start-insert source<cr>

" Files
nnoremap [unite]f :Unite -no-split -start-insert file_rec/async<cr>

" Files in rails
nnoremap [unite]rm :Unite -no-split -start-insert -input=app/models/ file_rec/async<cr>
nnoremap [unite]rv :Unite -no-split -start-insert -input=app/views/ file_rec/async<cr>
nnoremap [unite]ra :Unite -no-split -start-insert -input=app/assets/ file_rec/async<cr>
nnoremap [unite]rs :Unite -no-split -start-insert -input=spec/ file_rec/async<cr>

" Grepping
nnoremap [unite]g :Unite -no-split grep:.<cr>
nnoremap [unite]d :Unite -no-split grep:.:-s:\(TODO\|FIXME\)<cr>

" Content
nnoremap [unite]o :Unite -no-split -start-insert -auto-preview outline<cr>
nnoremap [unite]l :Unite -no-split -start-insert line<cr>
nnoremap [unite]t :!retag<cr>:Unite -no-split -auto-preview -start-insert tag<cr>

" Quickly switch between recent things
nnoremap [unite]F :Unite -no-split buffer tab file_mru directory_mru<cr>
nnoremap [unite]b :Unite -no-split buffer<cr>
nnoremap [unite]m :Unite -no-split file_mru<cr>

" Yank history
nnoremap [unite]y :Unite -no-split history/yank<cr>

" }}}

" ==== Abbreviations =============== {{{
" ==================================

" When typing %% expand it into the path to the current file
cnoremap %% <C-R>=expand('%:h') . '/'<cr>

iabbrev @@ david.pdrsn@gmail.com

" inoremap ( ()<esc>i
" inoremap { {}<esc>i
" inoremap [ []<esc>i
" inoremap ' ''<esc>i
" inoremap " ""<esc>i

" more abbreviations can be found in ~/.vim/after/plugin/abolish.vim

" }}}

" ==== Functions =============== {{{
" ==================================

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
  " $background means return whats currently in `set bacground`
  " and `==?` means that the comparison will be case sensitive no matter what
  if &background ==? 'dark'
    let &background = 'light'
    " same as `set background=light`
  else
    let &background = 'dark'
  endif
endfunction

function! RunCurrentFile()
  if &filetype == "ruby"
    call RunCommand("ruby", PathToCurrentFile())
  elseif &filetype == "sml"
    call RunCommand("rlwrap mosml -P full", PathToCurrentFile())
  elseif &filetype == "javascript"
    call RunCommand("node", PathToCurrentFile())
  elseif &filetype == "shell"
    call RunCommand("sh", PathToCurrentFile())
  elseif &filetype == "python"
    call RunCommand("python", PathToCurrentFile())
  elseif &filetype == "haskell"
    call RunCommand("ghci", PathToCurrentFile())
  elseif &filetype == "coffee"
    call RunCommand("run_coffeescript", PathToCurrentFile())
  elseif &filetype == "tex" || &filetype == "plaintex"
    execute "Dispatch compile_and_open_tex %"
  else
    echo "Dunno how to run such a file..."
  endif
endfunction

function! RunCurrentTests()
  if &filetype == "ruby"
    if has("gui_running")
      let rspec = "rspec --no-color"
    else
      let rspec = "rspec"
    endif

    if InRailsApp()
      if FilenameIncludes("_spec")
        let g:vimrc_test_file = PathToCurrentFile()
        call RunCommand("spring " . rspec, g:vimrc_test_file)
      elseif exists("g:vimrc_test_file")
        call RunCommand("spring " . rspec, g:vimrc_test_file)
      else
        call RunCommand("spring " . rspec, PathToCurrentFile())
      endif
    else
      if FilenameIncludes("_spec")
        let g:vimrc_test_file = PathToCurrentFile()
        call RunCommand(rspec, g:vimrc_test_file)
      elseif exists("g:vimrc_test_file")
        call RunCommand(rspec, g:vimrc_test_file)
      else
        call RunCommand(rspec, PathToCurrentFile())
      endif
    endif
  elseif &filetype == "sml"
    call RunCommand("run_sml_tests", PathToCurrentFile())
  elseif &filetype == "javascript" || &filetype == "coffee"
    call RunCommand("karma run", "")
  elseif &filetype == "haskell"
    call RunCommand("runhaskell", PathToCurrentFile() . " -f progress")
  else
    echo "Dunno how to test such a file..."
  endif
endfunction

function! RunCommand(cmd, args)
  let command = 'clear; ' . a:cmd . " " . a:args

  if InTmux() && NumberOfTmuxPanes() > 1
    let command = 'Tmux ' . command
  else
    let command = '!' . command
  endif

  exec command
endfunction

function! PathToCurrentFile()
  return expand('%:p')
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

" }}}

" ==== Local Settings =============== {{{
" ==================================

try
  source ~/.vimrc.local
catch
endtry

" }}}
