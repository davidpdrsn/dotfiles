" ==== Plugins ===================== {{{
" ==================================

if has('vim_starting')
  set nocompatible
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

call neobundle#rc(expand('~/.vim/bundle/'))

" Let NeoBundle manage NeoBundle
NeoBundleFetch 'Shougo/neobundle.vim'

" Utils
NeoBundle 'https://github.com/wincent/Command-T.git'
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-commentary'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-abolish'
NeoBundle 'edsono/vim-matchit'
NeoBundle 'Valloric/MatchTagAlways'
NeoBundle 'AndrewRadev/switch.vim'
NeoBundle 'terryma/vim-multiple-cursors'
NeoBundle 'godlygeek/tabular'
NeoBundle 'vim-scripts/Emmet.vim'
NeoBundle 'ervandew/supertab'
NeoBundle 'vim-scripts/scratch.vim'
NeoBundle 'Raimondi/delimitMate/'
NeoBundle 'rking/ag.vim'
NeoBundle 'scrooloose/nerdtree'

" UI
NeoBundle 'bling/vim-airline'
NeoBundle 'airblade/vim-gitgutter'
NeoBundle 'roman/golden-ratio'

" Snippets
NeoBundle 'SirVer/ultisnips'

" Filetypes
NeoBundle 'vim-ruby/vim-ruby'
NeoBundle 'othree/html5-syntax.vim'
NeoBundle 'kchmck/vim-coffee-script'
NeoBundle 'plasticboy/vim-markdown'
NeoBundle 'cakebaker/scss-syntax.vim'

" Colors
NeoBundle 'altercation/vim-colors-solarized'

" Tmux
NeoBundle 'christoomey/vim-tmux-navigator'
NeoBundle 'jgdavey/tslime.vim'

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
set norelativenumber              " Use relative line numbers
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
set background=dark               " Tell Vim the color of my background
set grepprg=ag                    " Use Silver Searcher instead of grep
set formatoptions-=or             " Don't add the comment prefix when I hit enter or o/O on a comment line.

colorscheme default

" Some GUI specific settings
if has("gui_running")
  set guifont=Ubuntu\ Mono\ derivative\ Powerline:h16
  set guioptions-=T
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
  autocmd FileType qf match none

  autocmd FileType sml set commentstring=(*\ %s\ *)

  autocmd FileType mkd setlocal spell nofoldenable
  autocmd FileType text setlocal spell nofoldenable

  autocmd FileType vim setlocal foldmethod=marker

  autocmd FileType java call AddJavaFile(PathToCurrentFile())
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
nnoremap <up>    <C-W>+
nnoremap <down>  <C-W>-
nnoremap <left>  3<C-W>>
nnoremap <right> 3<C-W><

inoremap <left>  <nop>
inoremap <right> <nop>
inoremap <up> <esc>ddkPi
inoremap <down> <esc>ddpi

vnoremap <left>  <nop>
vnoremap <right> <nop>
vnoremap <up> xkP`[V`]
vnoremap <down> xp`[V`]

" Spell correct current word
imap <c-z> <esc>,zea

noremap <cr> ma:nohlsearch<cr><cr>`a

" }}}

" ==== Leader Mappings ============= {{{
" ==================================

let mapleader = ','
let maplocalleader = '\\'

noremap <leader><leader> <C-^>

noremap <leader>; maA;<esc>`a
noremap <leader>== magg=G`a
vnoremap <leader>= :Tabularize /
noremap <leader>/ /\v
noremap <leader>? ?\v

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
noremap <leader>do ma^/do<cr>ciw{ <esc>lxJJ$ciw}<esc>`a
noremap <leader>di :Dispatch 

"-- e --"
noremap <leader>ev :tabedit $MYVIMRC<cr>
noremap <leader>es :UltiSnipsEdit<cr>

"-- f --"
noremap <leader>f :Ag 

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

"-- h --"

"-- i --"

"-- j --"
noremap <leader>j :tabe<cr>:cd ~/hax/journal/entries<cr>:e.<cr>
noremap <leader>J :call AddJavaFile(PathToCurrentFile())<cr>

"-- k --"

"-- l --"

"-- m --"
" For quickly making markdown headings
noremap <leader>mh yypVr=k
noremap <leader>m2h yypVr-k
" Format SML comments
vnoremap <leader>mlc ^:s/(\*/ */g<cr>gv:s/ \*)//g<cr>A *)<esc>gvo<esc>r(gvo<esc>:nohlsearch<cr>

"-- n --"
noremap <leader>n :NERDTreeToggle<cr>

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
noremap <leader>re :%s/\r\(\n\)/\1/eg<cr>:retab<cr>:%s/\s\+$//e<cr>:nohlsearch<cr>
" Evaluate selection as ruby and insert the output
vnoremap <leader>r :!ruby<cr>
noremap <leader>rd :redraw!<cr>
noremap <leader>rr :w\|:call RunCurrentFile()<cr>

"-- s --"
noremap <leader>sv :source $MYVIMRC<cr>:nohlsearch<cr>:e<cr>
noremap <leader>sw :Switch<cr>

"-- t --"
noremap <leader>t :w\|:call RunCurrentTests(-1)<cr>
noremap <leader>T :w\|:call RunCurrentTests(line("."))<cr>

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

" ==== CommandT ==================== {{{
" ==================================

nnoremap [commandt] <Nop>
nmap <space> [commandt]

map [commandt]f :CommandTFlush<cr>\|:CommandT<cr>
map [commandt]s :CommandTFlush<cr>\|:CommandT spec<cr>
map [commandt]c :CommandTFlush<cr>\|:CommandT app/controllers<cr>
map [commandt]v :CommandTFlush<cr>\|:CommandT app/views<cr>
map [commandt]m :CommandTFlush<cr>\|:CommandT app/models<cr>
map [commandt]a :CommandTFlush<cr>\|:CommandT app/assets<cr>
map [commandt]l :CommandTFlush<cr>\|:CommandT app/lib<cr>

map [commandt]t :!retag<cr>\|:CommandTFlush<cr>\|:CommandTTag<cr>
map [commandt]b :CommandTBuffer<cr>

let g:CommandTCancelMap=['<C-[>', '<C-c>']
let g:CommandTWildIgnore=&wildignore . ",**/bower_components/*,**/node_modules/*,**/_site/*,**/vendor/*,**/*.class"
let g:CommandTMaxHeight=50

" }}}

" ==== Misc Plugin Configs =============== {{{
" ========================================

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

highlight link hspecDescribe Type
highlight link hspecIt Identifier
highlight link hspecDescription String

let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

highlight clear SignColumn

let g:gitgutter_enabled = 0

" }}}

" ==== Abbreviations =============== {{{
" ==================================

" When typing %% expand it into the path to the current file
cnoremap %% <C-R>=expand('%:h') . '/'<cr>

iabbrev @@ david.pdrsn@gmail.com

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
  let original_setting = &spell

  set spell
  normal 1z=

  let &spell = original_setting
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
    call RunCommand("ruby " . PathToCurrentFile())
  elseif &filetype == "sml"
    call RunCommand("rlwrap mosml -P full " . PathToCurrentFile())
  elseif &filetype == "javascript"
    call RunCommand("node " . PathToCurrentFile())
  elseif &filetype == "shell"
    call RunCommand("sh " . PathToCurrentFile())
  elseif &filetype == "python"
    call RunCommand("python " . PathToCurrentFile())
  elseif &filetype == "haskell"
    call RunCommand("ghci " . PathToCurrentFile())
  elseif &filetype == "coffee"
    call RunCommand("run_coffeescript " . PathToCurrentFile())
  elseif &filetype == "tex"
    call RunCommand("pdflatex " . PathToCurrentFile() . " && open " . substitute(expand("%"), "\.tex$", ".pdf", ""))
  elseif &filetype == "java"
    call RunCommand("javac *.java && java " . substitute(expand("%"), "\.java$", "", ""))
  else
    echo "Dunno how to run such a file..."
  endif
endfunction

function! RunCurrentTests(line_number)
  if &filetype == "ruby"
    if has("gui_running")
      let rspec = "echo \"\" && rspec --no-color"
    else
      let rspec = "rspec"
    endif

    let cmd = ""

    if InRailsApp()
      if FilenameIncludes("_spec")
        if a:line_number != -1
          let g:test_line_number = a:line_number
        endif
        let g:vimrc_test_file = PathToCurrentFile()
        let cmd = "spring " . rspec . " " . g:vimrc_test_file
      elseif exists("g:vimrc_test_file")
        let cmd = "spring " . rspec . " " . g:vimrc_test_file
      else
        let cmd = "spring " . rspec . " " . PathToCurrentFile()
      endif
    else
      if FilenameIncludes("_spec")
        if a:line_number != -1
          let g:test_line_number = a:line_number
        endif
        let g:vimrc_test_file = PathToCurrentFile()
        let cmd = rspec . " " . g:vimrc_test_file
      elseif exists("g:vimrc_test_file")
        let cmd = rspec . " " . g:vimrc_test_file
      else
        let cmd = rspec . " " . PathToCurrentFile()
      endif
    endif

    if a:line_number != -1
      let cmd = cmd . ":" . g:test_line_number
    endif

    call RunCommand(cmd)
  elseif &filetype == "sml"
    call RunCommand("run_sml_tests" . " " .  PathToCurrentFile())
  elseif &filetype == "javascript" || &filetype == "coffee"
    call RunCommand("karma run")
  elseif &filetype == "haskell"
    call RunCommand("runhaskell " . PathToCurrentFile() . " -f progress")
  else
    echo "Dunno how to test such a file..."
  endif
endfunction

function! RunCommand(cmd)
  let command = 'clear; ' . a:cmd

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

if !exists("g:java_files_in_project")
  let g:java_files_in_project = []
endif

function! AddJavaFile(path)
  if index(g:java_files_in_project, a:path) == -1 && match(a:path, "git")
    call add(g:java_files_in_project, a:path)
  endif
endfunction

" }}}

" ==== Local Settings =============== {{{
" ==================================

try
  source ~/.vimrc.local
catch
endtry

" }}}
