"==========================================
" Plugins
"==========================================

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

" Fuzzy search
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/unite-outline'

" Utils
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-commentary'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'Townk/vim-autoclose'
NeoBundle 'edsono/vim-matchit'
NeoBundle 'Valloric/MatchTagAlways'
NeoBundle 'AndrewRadev/switch.vim'
NeoBundle 'terryma/vim-multiple-cursors'
NeoBundle 'godlygeek/tabular'
NeoBundle 'vim-scripts/Emmet.vim'

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

" Tmux
NeoBundle 'christoomey/vim-tmux-navigator'
NeoBundle 'jgdavey/tslime.vim'

NeoBundleCheck

"==========================================
" General
"==========================================

filetype plugin indent on
syntax enable
set shell=zsh                     " Use zsh as shell
set history=1000                  " Sets how many lines of history vim has to remember
set undolevels=1000               " How many steps of undo history vim should remember
set nonumber                      " Use relative line numbers
set numberwidth=1                 " The width of the number column
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
set foldlevel=99                  " Open all folds initially
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
set background=dark               " Tell Vim the color of my background
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
colorscheme solarized             " Colorscheme
match ErrorMsg '\%>100v.\+'       " Hight lines that are longer then 100 chars

"==========================================
" Auto commands
"==========================================

" Jump to last cursor position unless it's invalid or in an event handler
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif

autocmd FileType scss,sass,css,html setlocal foldmethod=indent
autocmd BufRead,BufNewFile *.scss set filetype=scss

" in sml files highlight lines that are longer than 80 chars
autocmd FileType sml match ErrorMsg '\%>80v.\+'

" in markdown files don't highlight long lines
autocmd FileType mkd match ErrorMsg '\%>99999v.\+'

" disable folding in markdown files
autocmd FileType mkd set nofoldenable

" automatically insert a semicolon in CSS files
autocmd FileType css imap : : ;<left>

" automatically insert closing bracket in HTML
autocmd FileType html imap < <><left>

"==========================================
" Mappings
"==========================================

" Disable useless and annoying keys
map Q <Nop>
map K <Nop>

" Don't wanna retrain my fingers
command! W w
command! Q q
command! Qall qall

" Make Y work as expected
nnoremap Y y$

" Intuitive movement over long lines
nmap k gk
nmap j gj

" Quickly leave insert mode, and safe at the same time
map <C-s> <esc>:w<CR>
imap <C-s> <esc>:w<CR>

" Resize windows with the arrow keys
map <up> <C-W>+
map <down> <C-W>-
map <left> 3<C-W>>
map <right> 3<C-W><

map <return> :nohlsearch<cr>

"==========================================
" Leader mappings
"==========================================

let mapleader = ','

map <leader><leader> <C-^>

"-- a --"
map <leader>aa maggVG"*y`a
vmap <leader>a :Tabularize /

"-- b --"
map <leader>b :call ToggleBackgroundColor()<cr>

"-- c --"
" comment closing HTML tag
map <leader>ct my^lyy%p/classf"v0c.f"D:s/ /./eg<cr>gcckJ:nohlsearch<cr>`y
map <leader>cd :cd %:p:h<cr>:pwd<cr>

"-- d --"
" delete wrapping HTML tag
map <leader>dt ^lma%mb'ajV'bk<'add'bdd
" convert ruby do/end to {}
map <leader>do ma^/do<cr>ciw{<esc>lxJJ$ciw}<esc>`a

"-- e --"
map <leader>ea :tabnew ~/dropbox/code/toolsharpeninglist.md<cr>
map <leader>ee :tabnew ~/dropbox/code/vimcheatsheet.md<cr>
map <leader>ev :tabnew $MYVIMRC<cr>
map <leader>es :UltiSnipsEdit<cr>

"-- f --"

"-- g --"
nnoremap <leader>gb :Gblame<cr>
nnoremap <leader>gc :Gcommit<cr>
nnoremap <leader>gd :Gdiff<cr>
nnoremap <leader>gp :Git push<cr>
nnoremap <leader>gr :Gremove<cr>
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gw :Gwrite<cr>
nnoremap <leader>gg :w<cr>:Gwrite<cr>:Gcommit -m 'update'<cr>:Git push<cr><cr>:e<cr>

"-- h --"

"-- i --"

"-- j --"

"-- k --"

"-- l --"

"-- m --"
" for quickly making markdown headings
map <leader>mh yypVr=k
map <leader>m2h yypVr-k
" formal SML comments
vmap <leader>mlc ^:s/(\*/ */g<cr>gv:s/ \*)//g<cr>A *)<esc>gvo<esc>r(gvo<esc>:nohlsearch<cr>

"-- n --"
map <leader>n :set number!<cr>

"-- o --"
map <leader>o :only<cr>

"-- p --"
map <leader>p <esc>o<esc>"*]p

"-- q --"
map <leader>q :q<cr>
map <leader>Q :qall<cr>

"-- r --"
map <leader>rn :call RenameFile()<cr>
map <leader>re :%s/\r\(\n\)/\1/eg<cr>:retab<cr>:%s/\s\+$//e<cr>
map <leader>rt :!ctags -R --exclude=.svn --exclude=.git --exclude=log --exclude=tmp --exclude=vendor *<cr>:CtrlPTag<cr>
" evaluate selection as ruby and insert the output
vmap <leader>r :!ruby<cr>

"-- s --"
map <leader>s :source $MYVIMRC<cr>:nohlsearch<cr>
map <leader>sw :Switch<cr>

"-- t --"
map <leader>t :call RunCurrentTests()<cr>
map <leader>T :call RunCurrentFile()<cr>

"-- u --"

"-- v --"

"-- w --"
map <leader>w :w<cr>
map <leader>W :wq<cr>

"-- x --"
map <leader>x :set filetype=

"-- y --"
map <leader>y "*y

"-- z --"
map <leader>z :call CorrectSpelling()<cr>

"==========================================
" Misc plugin configs
"==========================================

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

highlight link hspecDescribe Type
highlight link hspecIt Identifier
highlight link hspecDescription String

let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

highlight clear SignColumn

"==========================================
" Unite
"==========================================

call unite#filters#matcher_default#use(['matcher_fuzzy'])
let g:unite_source_history_yank_enable = 1
let g:unite_split_rule = "botright"
let g:unite_force_overwrite_statusline = 0

call unite#custom_source('file_rec,file_rec/async,file_mru,file,buffer,grep',
  \ 'ignore_pattern', join([
  \ '\.git/',
  \ '\.sass-cache/',
  \ '\vendor/',
  \ ], '\|'))

" Custom mappings for the unite buffer
autocmd FileType unite call s:unite_settings()
function! s:unite_settings()
  let b:SuperTabDisabled=1

  imap <buffer> <C-j>   <Plug>(unite_select_next_line)
  imap <buffer> <C-k>   <Plug>(unite_select_previous_line)

  imap <silent><buffer><expr> <C-s> unite#do_action('split')
  imap <silent><buffer><expr> <C-v> unite#do_action('vsplit')
  imap <silent><buffer><expr> <C-t> unite#do_action('tabopen')

  nmap <buffer> <ESC> <Plug>(unite_exit)
endfunction

nnoremap <space>f :<C-u>Unite -start-insert file_rec/async<cr>
nnoremap <space>c :<C-u>Unite -start-insert grep:%::^<cr>
nnoremap <space>g :<C-u>Unite grep:.<cr>
nnoremap <space>t :<C-u>Unite grep:.::TODO<cr>
nnoremap <space>o :<C-u>Unite -start-insert outline<cr>
nnoremap <space>b :<C-u>Unite -quick-match buffer<cr>
nnoremap <space>r :<C-u>Unite -start-insert file_mru<cr>
nnoremap <space>y :<C-u>Unite history/yank<cr>


"==========================================
" Abbreviations
"==========================================

" When typing %% expand it into the path to the current file
cnoremap %% <C-R>=expand('%:h').'/'<cr>

cabbrev gs Gstatus
cabbrev ga Gwrite
cabbrev gc Gcommit


"==========================================
" Functions
"==========================================

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

function! RunCurrentFile()
  write

  if FilenameIncludes("\.rb")
    call RunCommand("ruby", PathToCurrentFile())
  elseif FilenameIncludes("\.sml")
    call RunCommand("rlwrap mosml -P full", PathToCurrentFile())
  elseif FilenameIncludes("\.js")
    call RunCommand("node", PathToCurrentFile())
  elseif FilenameIncludes("\.sh")
    call RunCommand("sh", PathToCurrentFile())
  elseif FilenameIncludes("\.py")
    call RunCommand("python", PathToCurrentFile())
  elseif FilenameIncludes("\.hs")
    call RunCommand("ghci", PathToCurrentFile())
  elseif FilenameIncludes("\.coffee")
    call RunCommand("run_coffeescript", PathToCurrentFile())
  else
    echo "Dunno how to run such a file..."
  endif
endfunction

function! RunCurrentTests()
  write

  if FilenameIncludes("\.rb")
    if InRailsApp()
      echo "Haven't setup how to run tests in a rails app"
    else
      if FilenameIncludes("_spec")
        let g:dgp_test_file = PathToCurrentFile()
        call RunCommand("rspec", g:dgp_test_file)
      elseif exists("g:dgp_test_file")
        call RunCommand("rspec", g:dgp_test_file)
      else
        call RunCommand("rspec", PathToCurrentFile())
      endif
    endif
  elseif FilenameIncludes("\.sml")
    call RunCommand("run_sml_tests", PathToCurrentFile())
  elseif FilenameIncludes("\.js")
    call RunCommand("karma run", "")
  elseif FilenameIncludes("\.coffee")
    call RunCommand("karma run", "")
  elseif FilenameIncludes("\.hs")
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

"===============================================================================
" Local Settings
"===============================================================================

try
  source ~/.vimrc.local
catch
endtry
