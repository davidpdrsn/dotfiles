"
"   /\\\        /\\\
"   \/\\\       \/\\\
"    \//\\\      /\\\   /\\\
"      \//\\\    /\\\   \///     /\\\\\  /\\\\\    /\\/\\\\\\\     /\\\\\\\\
"        \//\\\  /\\\     /\\\  /\\\///\\\\\///\\\ \/\\\/////\\\  /\\\//////
"          \//\\\/\\\     \/\\\ \/\\\ \//\\\  \/\\\ \/\\\   \///  /\\\
"            \//\\\\\      \/\\\ \/\\\  \/\\\  \/\\\ \/\\\        \//\\\
"              \//\\\       \/\\\ \/\\\  \/\\\  \/\\\ \/\\\         \///\\\\\\\\
"                \///        \///  \///   \///   \///  \///            \////////
"
" Welcome to my vimrc. If you have not already I'd recommend that you run
" `~/dotfiles/script/bootstrap` to get everything setup correctly.
"

" ========================================
" == Mandatory setup =====================
" ========================================

filetype off

" ========================================
" == Plugins =============================
" ========================================

call plug#begin('~/.vim/plugged')

Plug 'Raimondi/delimitMate'
Plug 'Shougo/vimproc.vim'
Plug 'SirVer/ultisnips'
Plug 'acarapetis/vim-colors-github'
Plug 'benekastah/neomake'
Plug 'bitc/vim-hdevtools', { 'for': 'haskell' }
Plug 'christoomey/Vim-g-dot'
Plug 'christoomey/vim-sort-motion'
Plug 'christoomey/vim-system-copy'
Plug 'christoomey/vim-tmux-navigator'
Plug 'christoomey/vim-tmux-runner'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'dag/vim2hs', { 'for': 'haskell' }
Plug 'davidpdrsn/vim-notable'
Plug 'davidpdrsn/vim-spectacular'
Plug 'elixir-lang/vim-elixir', { 'for': 'elixir' }
Plug 'jgdavey/tslime.vim'
Plug 'kana/vim-textobj-user', { 'for': 'ruby' }
Plug 'kchmck/vim-coffee-script', { 'for': 'coffee' }
Plug 'mattn/emmet-vim'
Plug 'nanotech/jellybeans.vim'
Plug 'pbrisbin/vim-mkdir'
Plug 'pbrisbin/vim-syntax-shakespeare'
Plug 'plasticboy/vim-markdown'
Plug 'rizzatti/dash.vim'
Plug 'rking/ag.vim'
Plug 'sjl/gundo.vim'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rbenv'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-vinegar'
Plug 'vim-ruby/vim-ruby'

Plug 'godlygeek/tabular'

Plug 'junegunn/seoul256.vim'

call plug#end()

" Enable built-in matchit plugin
runtime macros/matchit.vim

" ========================================
" == General config ======================
" ========================================

" misc
filetype plugin indent on         " Enable good stuff
syntax enable                     " Enable syntax highlighting
colorscheme jellybeans
set colorcolumn=81                " Highlight 81st column
set fillchars+=vert:\             " Don't show pipes in vertical splits
set grepprg=ag\ --nogroup\ --nocolor\ -i
set backspace=indent,eol,start    " Backspace over everything in insert mode
set hidden                        " Don't unload buffers when leaving them
set nospell                       " Disable spell checking
set spelllang=en_us               " Use english US for spell checking
set scrolljump=5                  " Scroll more than one line
set scrolloff=3                   " Min. lines to keep above or below the cursor when scrolling
set shell=/bin/bash
set splitbelow                    " Open splits below
set splitright                    " Open splits to the right
set tags=./tags,tags,codex.tags;$HOME        " Tell Vim where to look for tags files
set timeout                       " Lower the delay of escaping out of other modes
set visualbell                    " Disable annoying beep
set wildmenu                      " Enable command-line like completion
" set wildmode=list:longest         " List all matches and complete till longest common string
set wrap                          " Wrap long lines
set noesckeys                     " Remove delay after pressing esc
set ttimeout                      " Set behavior of when partial mappings are pressed
set ttimeoutlen=1                 " Don't delay execution of a mapping
set nojoinspaces                  " Insert only one space when joining lines that contain sentence-terminating punctuation like `.`.

" ui
set laststatus=2                  " Always show the status line
set linebreak                     " Don't break lines in the middle of words
set list                          " Show some more characters
set listchars=tab:▸\              " Char representing a tab
set listchars+=extends:❯          " Char representing an extending line
set listchars+=nbsp:␣             " Non breaking space
set listchars+=precedes:❮         " Char representing an extending line in the other direction
set listchars+=trail:·            " Show trailing spaces as dots
set nocursorcolumn                " Don't highlight the current column
set nocursorline                  " Don't highlight the current line
set number                        " Don't show line numbers
set numberwidth=4                 " The width of the number column
set relativenumber                " Show relative numbers
set guifont=Input\ Mono:h11 " Set gui font
set guioptions-=T                 " No toolbar in MacVim
set guioptions-=r                 " Also no scrollbar
set guioptions-=L                 " Really no scrollbar
set winheight=7
set winminheight=7
set winheight=999

" searching
set hlsearch                      " Highlight search matches
set ignorecase                    " Do case insensitive search unless there are capital letters
set incsearch                     " Perform incremental searching

" backups & undo
set backup
set backupdir=~/.config/nvim/tmp/backup/
set backupskip=/tmp/*,/private/tmp/*
set noswapfile
set history=1000                  " Sets how many lines of history vim has to remember
set undodir=~/.config/nvim/tmp/undo/
set undofile
set undolevels=1000               " How many steps of undo history vim should remember
set writebackup

" indentation
set expandtab                     " Indent with spaces
set shiftwidth=2                  " Number of spaces to use when indenting
set smartindent                   " Auto indent new lines
set softtabstop=2                 " Number of spaces a <tab> counts for when inserting
set tabstop=2                     " Number of spaces a <tab> counts for

" folds
set foldenable                    " Enable folds
set foldlevelstart=99             " Open all folds by default
set foldmethod=indent             " Fold by indentation

" the status line
set statusline=%f                 " Tail of the filename
set statusline+=\ %h              " Help file flag
set statusline+=%m                " Modified flag
set statusline+=%r                " Read only flag
set statusline+=%y                " Filetype
set statusline+=%=                " Left/right separator
set statusline+=col:\ %c,         " Cursor column
set statusline+=\ line:\ %l/%L    " Cursor line/total lines
set statusline+=\ %{fugitive#statusline()}

" ========================================
" == Auto commands =======================
" ========================================

augroup configureFoldsAndSpelling
  autocmd!
  autocmd FileType mkd       setlocal spell nofoldenable
  autocmd FileType markdown  setlocal spell nofoldenable
  autocmd FileType text      setlocal spell nofoldenable
  autocmd FileType gitcommit setlocal spell
  autocmd FileType vim       setlocal foldmethod=marker
augroup END

augroup resumeCursorPosition
  autocmd!
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif
augroup END

augroup CursorLineOnlyInActiveWindow
  autocmd!
  autocmd VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  autocmd WinLeave * setlocal nocursorline
augroup END

augroup miscGroup
  autocmd!

  " set comments for SML
  autocmd FileType sml set commentstring=(*\ %s\ *)

  " somehow this is required to move the gray color of the sign column
  autocmd FileType * highlight clear SignColumn

  " when in a git commit buffer go the beginning
  autocmd FileType gitcommit au! BufEnter COMMIT_EDITMSG call setpos('.', [0, 1, 1, 0])

  " save files when focus is lost
  autocmd BufLeave * silent! update

  " always have quickfix window take up all the horizontal space
  autocmd FileType qf wincmd J

  " configure indentation for python
  autocmd FileType python set expandtab tabstop=4 softtabstop=4 shiftwidth=4

  " Disable spell checking in vim help files
  autocmd FileType help set nospell

  " Fasto setup
  autocmd BufNewFile,BufRead *.fo setlocal ft=fasto

  " C setup, Vim thinks .h is C++
  autocmd BufNewFile,BufRead *.h setlocal ft=c

  " Pow setup
  autocmd BufNewFile,BufRead *.pow setlocal ft=pow
  autocmd FileType pow set commentstring={{\ %s\ }}

  autocmd BufWinEnter,WinEnter term://* startinsert
  autocmd BufLeave term://* stopinsert

  autocmd! BufWritePost *.hs Neomake
augroup END

" ========================================
" == Mappings ============================
" ========================================

" Rather than having loads of comments above my mappings I
" try to make well named functions
source ~/.config/nvim/functions.vim

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

nnoremap gjb :call JsBindFunction()<cr>

" Resize windows with the arrow keys
nnoremap <up>    <C-W>+
nnoremap <down>  <C-W>-
nnoremap <left>  3<C-W>>
nnoremap <right> 3<C-W><
nnoremap <A-k>    <C-W>+
nnoremap <A-j>  <C-W>-
nnoremap <A-h>  3<C-W>>
nnoremap <A-l> 3<C-W><

" Move text around in visual mode
vnoremap <left>  <nop>
vnoremap <right> <nop>
vnoremap <up> xkP`[V`]
vnoremap <down> xp`[V`]

" Exit insert mode and save just by hitting ESC
imap <c-s> <esc>:w<cr>
map <c-s> <esc>:w<cr>

" Quickly create ruby method definitions
imap <c-d> <esc>bidef <esc>oend<esc>ko
imap <c-f> <esc>bidef <esc>oend<esc>kA()<esc>i

" insert current file name with \f in insert mode
" inoremap \f <C-R>=expand("%:t:r")<CR>

" insert path to current file
cnoremap %% <C-R>=expand('%:h').'/'<cr>

" correct spelling from insert mode
inoremap <c-l> <esc>:call CorrectSpelling()<cr>a

" add Ex command for finding ruby Conditionals
command! FindConditionals :normal /\<if\>\|\<unless\>\|\<and\>\|\<or\>\|||\|&&<cr>

" add Ex command for removing characters sometimes present when copying from
" tex compiled PDF files
command! RemoveFancyCharacters :call RemoveFancyCharacters()

" Merge tabs
nmap <C-W>M :call MergeTabs()<CR>

" Save file with sudo by doing :w!!
cmap w!! w !sudo tee % >/dev/null

tnoremap <C-[> <C-\><C-n>
tnoremap <C-h> <C-\><C-n><C-w>h
tnoremap <C-j> <C-\><C-n><C-w>j
tnoremap <C-k> <C-\><C-n><C-w>k
tnoremap <C-l> <C-\><C-n><C-w>l
tnoremap <A-k> <C-\><C-n><C-W>+i
tnoremap <A-j> <C-\><C-n><C-W>-i
tnoremap <A-h> <C-\><C-n>3<C-W>>i
tnoremap <A-l> <C-\><C-n>3<C-W><i

" ========================================
" == Leader mappings =====================
" ========================================

let mapleader = "\<Space>"

vnoremap <leader>, :normal .<cr>
nnoremap <leader>$ :normal ds(i$ <cr>
nnoremap <leader>. :normal ds(i. <cr>

noremap <leader><leader> <C-^>

noremap <leader>; maA;<esc>`a
noremap <leader>== magg=G`a
vnoremap <leader>= :Tabularize /
noremap <leader>/ /\v
noremap <leader>? ?\v

"-- a --"
noremap <leader>a :call YankWholeBuffer(0)<cr>
noremap <leader>A :call YankWholeBuffer(1)<cr>
noremap <leader>as :call rails_test#hsplit_spec()<cr>
noremap <leader>av :call rails_test#vsplit_spec()<cr>

"-- b --"
nnoremap <leader>bp orequire 'pry'; binding.pry<esc>^
nnoremap <leader>bg :call ToggleBackground()<cr>

"-- c --"
noremap <leader>cd :cd %:p:h<cr>:pwd<cr>
noremap <leader>cm :!chmod +x %<cr>

"-- d --"
noremap <leader>do :call ToggleRubyBlockSyntax()<cr>
noremap <leader>di :Dispatch<space>
nmap <silent> gD <Plug>DashSearch

" CtrlP modes I use most often
noremap <leader>db :CtrlPBuffer<cr>
noremap <leader>dl :CtrlPLine<cr>
noremap <leader>dq :CtrlPQuickfix<cr>
noremap <leader>dc :CtrlPChange<cr>
noremap <leader>dt :CtrlPTag<cr>
noremap <leader>dT :!retag<cr>:CtrlPClearAllCaches<cr>:CtrlPTag<cr>

" Rails specific
noremap <leader>ddm :CtrlP app/models<cr>
noremap <leader>ddc :CtrlP app/controllers<cr>
noremap <leader>ddv :CtrlP app/views<cr>
noremap <leader>ddz :CtrlP app/serializers<cr>
noremap <leader>ddj :CtrlP app/jobs<cr>
noremap <leader>dds :CtrlP app/services<cr>
noremap <leader>dsm :CtrlP spec/models<cr>
noremap <leader>dsc :CtrlP spec/controllers<cr>
noremap <leader>dsv :CtrlP spec/views<cr>
noremap <leader>dsz :CtrlP spec/serializers<cr>
noremap <leader>dss :CtrlP spec/services<cr>
noremap <leader>dsj :CtrlP spec/jobs<cr>

"-- e --"
noremap <leader>ev :tabedit $MYVIMRC<cr>:lcd ~/dotfiles<cr>
noremap <leader>es :UltiSnipsEdit<cr>

"-- f --"
nnoremap <leader>F :CtrlPClearAllCaches<cr>:CtrlP<cr>
nnoremap <leader>f :call CtrlPCurrentDir()<cr>

function! CtrlPCurrentDir()
  let pwd = getcwd()
  execute "CtrlP " . pwd
endfunction

noremap <leader>o :call Zoom()<cr>
" tnoremap <leader>o <C-\><C-n>:call Zoom()<cr>i

"-- g --"
noremap <leader>g :sp term://gitsh<CR>
noremap <leader>gap :Git add -p<cr>
noremap <leader>gb :Gblame<cr>
noremap <leader>gc :Gcommit --verbose<cr>
noremap <leader>gd :Gdiff<cr>
noremap <leader>gp :Dispatch git push<cr>
noremap <leader>gr :Gremove<cr>
noremap <leader>gs :Gstatus<cr>
noremap <leader>ga :Gwrite<cr>
noremap <leader>gu :GundoToggle<cr>

"-- h --"
noremap <leader>h :nohlsearch<cr>

"-- i --"
nnoremap <leader>i :call IndentEntireFile()<cr>

"-- j --"
noremap <leader>j :call GotoDefinitionInSplit(0)<cr>
noremap <leader>J :call GotoDefinitionInSplit(1)<cr>

"-- k --"
noremap <leader>k :w<cr>:call spectacular#run_tests_with_current_line()<cr>

"-- l --"
noremap <leader>l :call MakeList()<cr>

"-- m --"
noremap <leader>mh :call MakeMarkdownHeading(1)<cr>
noremap <leader>mH :call MakeMarkdownHeading(2)<cr>
vnoremap <leader>mlc :call FormatSmlComments()<cr>
vnoremap <leader>ml :call PasteMarkdownLink()<cr>

"-- n --"
noremap <leader>ns :set spell!<cr>

"-- o --"
noremap <leader>O :!open %<cr><cr>

"-- p --"
noremap <leader>p :call PasteFromSystemClipBoard()<cr>
noremap <leader>pi :PlugInstall<cr>
noremap <leader>pc :PlugClean<cr>
noremap <leader>pu :PlugUpdate<cr>

"-- q --"
noremap <leader>q :call CloseExtraPane()<cr>

"-- r --"
noremap <leader>rbi :w\|:Dispatch bundle install<cr>
noremap <leader>rd :redraw!<cr>
noremap <leader>re :call FixFormatting()<cr>
noremap <leader>ri :RunInInteractiveShell<space>
noremap <leader>rn :call RenameFile()<cr>
noremap <leader>rr :w\|:call RunCurrentFile()<cr>

nnoremap <leader>rel :call PromoteToLet()<cr>

"-- s --"
noremap <leader>sb :call notable#open_notes_file()<cr>
noremap <leader>se :SyntasticToggleMode<cr>:w<cr>
noremap <leader>ss :w\|:SyntasticCheck<cr>
noremap <leader>sv :source $MYVIMRC<cr>:nohlsearch<cr>
noremap <leader>st :sp<cr>:term zsh<cr>

"-- t --"
map <leader>t :w<cr>:call spectacular#run_tests()<cr>
map <leader>T :w<cr>:tabe term://rspec<cr>

"-- u --"

"-- v --"
noremap <leader>v :VtrSendLinesToRunner<cr>
noremap <leader>vt :vs<cr>:term zsh<cr>

"-- w --"
noremap <leader>W :wq<cr>
noremap <leader>wip :!git-wip<cr>

"-- x --"
noremap <leader>x :set filetype=

"-- y --"

"-- z --"
noremap <leader>z :call CorrectSpelling()<cr>

" ========================================
" == Misc plugin config ==================
" ========================================

let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v(\.(git|hg|svn)|dist|haskell.docset|.stack-work|.git|plugged|deps|_build|_sass|tmp|node_modules|vendor|_site|vim\.symlink\/bundle)$',
  \ 'file': '\v\.(exe|so|dll|svg|o|hi|ui|uo|sig|scssc|png|jpg|jpeg|gif|eot|woff|ttf|pdf|aux|log|class|gz|psd)$',
  \ 'link': '',
  \ }

let g:ctrlp_match_window = 'max:20'
" let g:ctrlp_user_command = 'ag %s -l --hidden --nocolor -g ""'
let g:ctrlp_use_caching = 1

let g:UltiSnipsEditSplit = 'horizontal'
let g:UltiSnipsSnippetDirectories = ["ultisnips"]

let g:multi_cursor_exit_from_visual_mode = 0

let g:spectacular_use_neovim = 1
let g:spectacular_debugging_mode = 1

let g:notable_notes_folder = "~/notes/"

let g:haskell_conceal = 0
let g:haskell_conceal_wide = 0

let g:airline_theme='onedark'
let g:airline_powerline_fonts = 1

" ========================================
" == Test running ========================
" ========================================

" call spectacular#add_test_runner('ruby, javascript, eruby, coffee, haml, yml', 'docker-compose\ run\ web\ bin/rspec\ {spec}' , '_spec.rb', function("UsesDocker"))
" call spectacular#add_test_runner('ruby, javascript, eruby, coffee, haml, yml', 'docker-compose\ run\ web\ bin/rspec\ {spec}:{line-number}' , '_spec.rb', function("UsesDocker"))
call spectacular#add_test_runner('ruby, javascript, eruby, coffee, haml, yml', 'rspec\ {spec}', '_spec.rb')
call spectacular#add_test_runner('ruby, javascript, eruby, coffee, haml, yml', 'rspec\ {spec}:{line-number}', '_spec.rb')
