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

call plug#begin('~/.config/nvim/plugged')

Plug 'Raimondi/delimitMate'
Plug 'Shougo/vimproc.vim'
Plug 'SirVer/ultisnips'
Plug 'acarapetis/vim-colors-github'
Plug 'neomake/neomake'
Plug 'bitc/vim-hdevtools', { 'for': 'haskell' }
Plug 'christoomey/Vim-g-dot'
Plug 'christoomey/vim-sort-motion'
Plug 'christoomey/vim-system-copy'
Plug 'christoomey/vim-tmux-navigator'
Plug 'christoomey/vim-tmux-runner'
Plug 'davidpdrsn/vim-notable'
Plug 'davidpdrsn/vim-spectacular'
Plug 'elixir-lang/vim-elixir', { 'for': 'elixir' }
Plug 'godlygeek/tabular'
Plug 'jgdavey/tslime.vim'
Plug 'kana/vim-textobj-user'
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
Plug 'rust-lang/rust.vim'
Plug 'mattn/gist-vim'
Plug 'mattn/webapi-vim'
Plug 'myfreeweb/intero.nvim'
Plug 'neovimhaskell/haskell-vim'
Plug 'tek/vim-textobj-ruby' " ir, if, ic, in
Plug 'tpope/vim-rails'
Plug 'kana/vim-textobj-entire' " ae

" Plugins on trail
Plug 'junegunn/goyo.vim'
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
Plug 'radenling/vim-dispatch-neovim'

call plug#end()

" Enable built-in matchit plugin
runtime macros/matchit.vim

" Rather than having loads of comments above my mappings I
" try to make well named functions
source ~/.config/nvim/functions.vim

" ========================================
" == General config ======================
" ========================================

" misc
filetype plugin indent on         " Enable good stuff
syntax enable                     " Enable syntax highlighting

color jellybeans
set background=dark

" color github
" set background=light

set colorcolumn=81                " Highlight 81st column
set fillchars+=vert:\             " Don't show pipes in vertical splits
set grepprg=ag\ --nogroup\ --nocolor\ -i
set backspace=indent,eol,start    " Backspace over everything in insert mode
set hidden                        " Don't unload buffers when leaving them
set nospell                       " Disable spell checking
set spelllang=en_us               " Use english US for spell checking
set lazyredraw                    " Don't redraw screen while running macros
set scrolljump=5                  " Scroll more than one line
set scrolloff=3                   " Min. lines to keep above or below the cursor when scrolling
set shell=/bin/bash
set splitbelow                    " Open splits below
set splitright                    " Open splits to the right
" set tags=./tags,codex.tags;$HOME        " Tell Vim where to look for tags files
set timeout                       " Lower the delay of escaping out of other modes
set visualbell                    " Disable annoying beep
set wildmenu                      " Enable command-line like completion
set wrap                          " Wrap long lines
set noesckeys                     " Remove delay after pressing esc
set ttimeout                      " Set behavior of when partial mappings are pressed
set ttimeoutlen=1                 " Don't delay execution of a mapping
set nojoinspaces                  " Insert only one space when joining lines that contain sentence-terminating punctuation like `.`.
set path+=**

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
set winwidth=84
set winminwidth=20
set winheight=7
set winminheight=7
set winheight=999
highlight TermCursor ctermfg=red guifg=red

" searching
set hlsearch                      " Highlight search matches
set ignorecase                    " Do case insensitive search unless there are capital letters
set incsearch                     " Perform incremental searching
set smartcase

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
set statusline+=%{NeomakeStatusLine()}
set statusline+=%=                " Left/right separator
set statusline+=col:\ %c,         " Cursor column
set statusline+=\ line:\ %l/%L    " Cursor line/total lines
set statusline+=\ \|\ %{fugitive#statusline()}

function! NeomakeStatusLine()
  let acc = []
  let errors = neomake#statusline#LoclistCounts()
  for pair in items(errors)
    let key = pair[0]
    let value = pair[1]
    let str = key . ": " . value
    call add(acc, str)
  endfor
  if len(acc) == 0
    return " | ✔"
  else
    return " | ✖ " . join(acc, ", ") . " ✖ "
  endif
endfunction

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

augroup omnifuncs
  autocmd!
  autocmd FileType haskell setlocal omnifunc=intero#omnifunc

  autocmd FileType haskell nnoremap <buffer> <Leader>G viw:InteroGoto<CR>
  autocmd FileType haskell nnoremap <buffer> <Leader>T viw:InteroType<CR>
  autocmd FileType haskell nnoremap <buffer> <Leader>U viw:InteroUses<CR>
  autocmd FileType haskell vnoremap <buffer> <Leader>G :InteroGoto<CR>
  autocmd FileType haskell vnoremap <buffer> <Leader>T :InteroType<CR>
  autocmd FileType haskell vnoremap <buffer> <Leader>U :InteroUses<CR>
augroup end

augroup resumeCursorPosition
  autocmd!
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif
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

  " Janus setup
  autocmd BufNewFile,BufRead *.ja setlocal ft=janus

  " C setup, Vim thinks .h is C++
  autocmd BufNewFile,BufRead *.h setlocal ft=c

  " C setup, Vim thinks .h is C++
  autocmd BufNewFile,BufRead /private/tmp/* set filetype=markdown

  " Pow setup
  autocmd BufNewFile,BufRead *.pow setlocal ft=pow
  autocmd FileType pow set commentstring={{\ %s\ }}

  autocmd BufWinEnter,WinEnter term://* startinsert
  autocmd BufLeave term://* stopinsert

  autocmd! BufWritePost *.hs Neomake
  autocmd! BufWritePost *.tex Neomake
  autocmd! BufWritePost *.rb Neomake

  " autocmd! BufWritePost * call jobstart("reload-safari")

  autocmd! BufWritePost *.tex call CompileLatex()

  autocmd FileType haskell set colorcolumn=80
  autocmd FileType haskell let &makeprg='hdevtools check %'

  autocmd FileType markdown let &makeprg='proselint %'
augroup END

" ========================================
" == Mappings ============================
" ========================================

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
nnoremap <up> <C-W>+
nnoremap <down> <C-W>-
nnoremap <left> 3<C-W>>
nnoremap <right> 3<C-W><
nnoremap <A-k> <C-W>+
nnoremap <A-j> <C-W>-
nnoremap <A-h> 3<C-W>>
nnoremap <A-l> 3<C-W><

" Move text around in visual mode
vnoremap <left> <nop>
vnoremap <right> <nop>
vnoremap <up> xkP`[V`]
vnoremap <down> xp`[V`]

" Exit insert mode and save just by hitting ESC
imap <c-s> <esc>:w<cr>
nmap <c-s> <esc>:w<cr>

nnoremap * ma*`a

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

nnoremap <leader>$ :normal ds(i$ <cr>
nnoremap <leader>. :normal ds(i. <cr>
vnoremap <leader>, :normal .<cr>

noremap <leader>/ /\v
noremap <leader>; maA;<esc>`a
noremap <leader>== magg=G`a
noremap <leader>? ?\v
vnoremap <leader>= :Tabularize /

function! FuzzyFileFind(path)
   if filereadable(".git/HEAD")
     execute "GFiles --others --cached --exclude-standard " . a:path
   else
     execute "FZF " . a:path
   endif
endfunction

nmap <leader>gr "*gr
nnoremap <leader>A :call YankWholeBuffer(1)<cr>
nnoremap <leader>J :call GotoDefinitionInSplit(1)<cr>
nnoremap <leader>O :!open %<cr><cr>
nnoremap <leader>T :w<cr>:tabe term://rspec<cr>
nnoremap <leader>W :wq<cr>
nnoremap <leader>a :call YankWholeBuffer(0)<cr>
nnoremap <leader>ag viw:call SearchForSelectedWord()<cr>
nnoremap <leader>as :call rails_test#hsplit_spec()<cr>
nnoremap <leader>av :call rails_test#vsplit_spec()<cr>
nnoremap <leader>bg :call ToggleBackground()<cr>
nnoremap <leader>cd :cd %:p:h<cr>:pwd<cr>
nnoremap <leader>cl :set cursorcolumn!<cr>
nnoremap <leader>cm :!chmod +x %<cr>
nnoremap <leader>ddc :call FuzzyFileFind("app/controllers")<cr>
nnoremap <leader>ddj :call FuzzyFileFind("app/jobs")<cr>
nnoremap <leader>ddm :call FuzzyFileFind("app/models")<cr>
nnoremap <leader>dds :call FuzzyFileFind("app/services")<cr>
nnoremap <leader>ddv :call FuzzyFileFind("app/views")<cr>
nnoremap <leader>ddz :call FuzzyFileFind("app/serializers")<cr>
nnoremap <leader>di :Dispatch<space>
nnoremap <leader>do :call ToggleRubyBlockSyntax()<cr>
nnoremap <leader>dsc :call FuzzyFileFind("spec/controllers")<cr>
nnoremap <leader>dsj :call FuzzyFileFind("spec/jobs")<cr>
nnoremap <leader>dsm :call FuzzyFileFind("spec/models")<cr>
nnoremap <leader>dss :call FuzzyFileFind("spec/services")<cr>
nnoremap <leader>dsv :call FuzzyFileFind("spec/views")<cr>
nnoremap <leader>dsz :call FuzzyFileFind("spec/serializers")<cr>
nnoremap <leader>dt :Tags<cr>
nnoremap <leader>es :UltiSnipsEdit<cr>
nnoremap <leader>ev :tabedit $MYVIMRC<cr>:lcd ~/dotfiles<cr>
nnoremap <leader>f :call FuzzyFileFind("")<cr>
nnoremap <leader>g :sp term://gitsh<CR>
nnoremap <leader>ga :Gwrite<cr>
nnoremap <leader>gap :Git add -p<cr>
nnoremap <leader>gb :Gblame<cr>
nnoremap <leader>gc :Gcommit --verbose<cr>
nnoremap <leader>gd :Gdiff<cr>
nnoremap <leader>gp :Dispatch git push<cr>
nnoremap <leader>gr :Gremove<cr>
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gt :call rails_test#run_spec()<cr>
nnoremap <leader>gu :GundoToggle<cr>
nnoremap <leader>h :nohlsearch<cr>
nnoremap <leader>hc :HdevtoolsClear<cr>
nnoremap <leader>ht :HdevtoolsType<cr>
nnoremap <leader>i :call IndentEntireFile()<cr>
nnoremap <leader>j :call GotoDefinitionInSplit(0)<cr>
" nnoremap <leader>k :w<cr>:call spectacular#run_tests_with_current_line()<cr>
nnoremap <leader>mH :call MakeMarkdownHeading(2)<cr>
nnoremap <leader>md :set filetype=markdown<cr>
nnoremap <leader>mh :call MakeMarkdownHeading(1)<cr>
nnoremap <leader>mk :w<cr>:make<cr>
nnoremap <leader>ns :set spell!<cr>
nnoremap <leader>o orequire 'pry'; binding.pry<esc>:w<cr>
nnoremap <leader>p :call PasteFromSystemClipBoard()<cr>
nnoremap <leader>pc :PlugClean<cr>
nnoremap <leader>pi :PlugInstall<cr>
nnoremap <leader>pu :PlugUpdate<cr>
nnoremap <leader>q :call CloseExtraPane()<cr>
nnoremap <leader>rbi :w\|:Dispatch bundle install<cr>
nnoremap <leader>rd :redraw!<cr>
nnoremap <leader>re :call FixFormatting()<cr>
nnoremap <leader>rel :call PromoteToLet()<cr>
nnoremap <leader>ri :RunInInteractiveShell<space>
nnoremap <leader>rn :call RenameFile()<cr>
nnoremap <leader>rr :w\|:call RunCurrentFile()<cr>
nnoremap <leader>rrt :call ExtractTempToQuery()<cr>
nnoremap <leader>sb :sp term://stack\ build<cr>
nnoremap <leader>se :SyntasticToggleMode<cr>:w<cr>
nnoremap <leader>sr :sp term://stack\ ghci<cr>
nnoremap <leader>ss :w\|:SyntasticCheck<cr>
nnoremap <leader>st :sp<cr>:term zsh<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>:nohlsearch<cr>
nnoremap <leader>k :w<cr>:call spectacular#run_tests_with_current_line()<cr>
nnoremap <leader>t :w<cr>:call spectacular#run_tests()<cr>
" nnoremap <leader>v :VtrSendLinesToRunner<cr>
nnoremap <leader>vt :vs<cr>:term zsh<cr>
nnoremap <leader>wip :!git-wip<cr>
nnoremap <leader>wtf oputs "#" * 80<c-m>puts caller<c-m>puts "#" * 80<esc>
nnoremap <leader>x :set filetype=
nnoremap <leader>z :call CorrectSpelling()<cr>
nnoremap <silent> gD :Dash<cr>
noremap <leader>l :call MakeList()<cr>

vnoremap <leader>ml :call PasteMarkdownLink()<cr>
vnoremap <leader>mlc :call FormatSmlComments()<cr>

augroup Terminal
  au!
  au TermOpen * let g:last_terminal_job_id = b:terminal_job_id
augroup END

function! REPLSend(lines)
  call jobsend(g:last_terminal_job_id, add(a:lines, ''))
endfunction

noremap <leader>v :call REPLSend([getline('.')])<cr>

" ========================================
" == Misc plugin config ==================
" ========================================

" let g:ctrlp_custom_ignore = {
"   \ 'dir':  '\v(\.(git|hg|svn)|dist|haskell.docset|.stack-work|.git|plugged|deps|_build|_sass|tmp|node_modules|vendor|_site|vim\.symlink\/bundle)$',
"   \ 'file': '\v\.(exe|so|dll|svg|o|hi|ui|uo|sig|scssc|png|jpg|jpeg|gif|eot|woff|ttf|pdf|aux|log|class|gz|psd)$',
"   \ 'link': '',
"   \ }

" let g:ctrlp_user_command = 'ag -Q -l --nocolor --hidden -g "" %s'
" let g:ctrlp_use_caching = 0
" let g:ctrlp_match_window = 'bottom,order:btt,min:1,max:20,results:20'

let g:UltiSnipsEditSplit = 'horizontal'
let g:UltiSnipsSnippetDirectories = ["ultisnips"]

let g:multi_cursor_exit_from_visual_mode = 0

let g:spectacular_use_terminal_emulator = 1
let g:spectacular_debugging_mode = 0

let g:notable_notes_folder = "~/notes/"

let g:haskell_conceal = 0
let g:haskell_conceal_wide = 0

let g:airline_theme='onedark'
let g:airline_powerline_fonts = 1

let g:gist_clip_command = 'pbcopy'

let g:vitality_fix_cursor = 1
let g:vitality_fix_focus = 1
let g:vitality_always_assume_iterm = 1

let g:gist_post_anonymous = 1

let g:haskell_enable_quantification = 1
let g:haskell_enable_recursivedo = 1
let g:haskell_enable_arrowsyntax = 1
let g:haskell_enable_pattern_synonyms = 1
let g:haskell_enable_typeroles = 1
let g:haskell_enable_static_pointers = 1

" Hack to make CTRL-H work in Neovim
nnoremap <silent> <BS> :TmuxNavigateLeft<cr>

let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-s': 'split',
  \ 'ctrl-v': 'vsplit' }

let g:neomake_haskell_hlint_maker = {
  \ 'args': ['-i "Redundant do"'],
  \ }

let g:neomake_ruby_rubocop_maker = {
  \ 'errorformat': '%f:%l:%c: %t: %m',
  \ 'postprocess': function('neomake#makers#ft#ruby#RubocopEntryProcess'),
  \ 'exe': 'check-style',
  \ 'args': ['%:d']
  \ }
let g:neomake_ruby_enabled_makers = ['rubocop']

" ========================================
" == Test running ========================
" ========================================

function! TernimalRun(cmd)
  execute "new"
  call termopen(a:cmd, {
        \ 'on_exit': function('s:OnExit'),
        \ 'buf': expand('<abuf>')
        \})
endfunction

function! s:OnExit(job_id, exit_code, event) dict
  if a:exit_code == 0
    execute "bd! " . s:test_buffer_number
    wincmd =
  else
    wincmd =
    call search("Failures:")
    normal zt
  endif
endfunction

function! s:OnTermClose(buf)
  let s:test_buffer_number = a:buf
endfunction

augroup neorun
  autocmd!
  autocmd TermClose * :call s:OnTermClose(0+expand('<abuf>'))
augroup end

call spectacular#add_test_runner('ruby, javascript, eruby, coffee, haml, yml', ':call TernimalRun("script/test {spec}")' , '_spec.rb')
call spectacular#add_test_runner('ruby, javascript, eruby, coffee, haml, yml', ':call TernimalRun("script/test {spec}:{line-number}")' , '_spec.rb')
