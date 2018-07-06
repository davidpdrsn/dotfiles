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

Plug '/usr/local/opt/fzf'
Plug 'Raimondi/delimitMate'
Plug 'Shougo/vimproc.vim'
Plug 'SirVer/ultisnips'
Plug 'acarapetis/vim-colors-github'
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
Plug 'junegunn/fzf.vim'
Plug 'junegunn/goyo.vim'
Plug 'kana/vim-textobj-entire' " ae
Plug 'kana/vim-textobj-user'
Plug 'kchmck/vim-coffee-script', { 'for': 'coffee' }
Plug 'mattn/emmet-vim'
Plug 'mattn/gist-vim'
Plug 'mattn/webapi-vim'
Plug 'myfreeweb/intero.nvim'
Plug 'nanotech/jellybeans.vim'
Plug 'neomake/neomake'
Plug 'neovimhaskell/haskell-vim'
Plug 'pbrisbin/vim-mkdir'
Plug 'pbrisbin/vim-syntax-shakespeare'
Plug 'plasticboy/vim-markdown'
Plug 'rizzatti/dash.vim'
Plug 'rking/ag.vim'
Plug 'rust-lang/rust.vim'
Plug 'sjl/gundo.vim'
Plug 'tek/vim-textobj-ruby' " ir, if, ic, in
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-rbenv'
Plug 'tpope/vim-bundler'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-vinegar'
Plug 'vim-ruby/vim-ruby'
Plug 'vim-scripts/CursorLineCurrentWindow'

" Plugins on trail
Plug 'benmills/vimux'
Plug 'herrbischoff/cobalt2.vim'

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

" color spring-night
" Remove underline for cursor line
hi CursorLine term=bold cterm=bold guibg=Grey40

" color solarized
" set background=dark

color jellybeans
set background=dark

" color github
" set background=light

" set background=dark         " for the light version
" let g:one_allow_italics = 1 " I love italic for comments
" colorscheme one

set colorcolumn=81                " Highlight 81st column
set fillchars+=vert:\             " Don't show pipes in vertical splits
set grepprg=ag\ --nogroup\ --nocolor\ -i
set backspace=indent,eol,start    " Backspace over everything in insert mode
set hidden                        " Don't unload buffers when leaving them
set nospell                       " Disable spell checking by default
set spelllang=en_us               " Use English US for spell checking
set lazyredraw                    " Don't redraw screen while running macros
set scrolljump=5                  " Scroll more than one line
set scrolloff=3                   " Minimum lines to keep above or below the cursor when scrolling
set shell=/bin/bash
set splitbelow                    " Open splits below
set splitright                    " Open splits to the right
" set tags=./tags,codex.tags;$HOME        " Tell Vim where to look for tags files
set timeout                       " Lower the delay of escaping out of other modes
set visualbell                    " Disable annoying beep
set wildmenu                      " Enable command-line like completion
set wrap                          " Wrap long lines
" set noesckeys                     " Remove delay after pressing escape
set ttimeout                      " Set behavior of when partial mappings are pressed
set ttimeoutlen=1                 " Don't delay execution of a mapping
set nojoinspaces                  " Insert only one space when joining lines that contain sentence-terminating punctuation like `.`.
set path+=**

" UI
set laststatus=2                  " Always show the status line
set linebreak                     " Don't break lines in the middle of words
set list                          " Show some more characters
set listchars=tab:▸\              " Char representing a tab
set listchars+=extends:❯          " Char representing an extending line
set listchars+=nbsp:␣             " Non breaking space
set listchars+=precedes:❮         " Char representing an extending line in the other direction
set listchars+=trail:·            " Show trailing spaces as dots
set nocursorcolumn                " Don't highlight the current column
set cursorline                    " Highlight the current line
set number                        " Don't show line numbers
set numberwidth=4                 " The width of the number column
set relativenumber                " Show relative numbers
set guifont=Input\ Mono:h11       " Set GUI font
set guioptions-=T                 " No tool bar in MacVim
set guioptions-=r                 " Also no scrollbar
set guioptions-=L                 " Really no scrollbar
set winwidth=84
try
  set winminwidth=20
catch
endtry
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
set history=1000                  " Sets how many lines of history Vim has to remember
set undodir=~/.config/nvim/tmp/undo/
set undofile
set undolevels=1000               " How many steps of undo history Vim should remember
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

augroup miscGroup
  autocmd!

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

  " configure indentation for python
  autocmd FileType rust set expandtab tabstop=4 softtabstop=4 shiftwidth=4

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

  autocmd! BufWritePost *.rs Neomake! cargo

  autocmd! BufWritePost *.tex call CompileLatex()

  autocmd FileType haskell set colorcolumn=80
  autocmd FileType haskell let &makeprg='hdevtools check %'

  autocmd FileType markdown let &makeprg='proselint %'
augroup END

augroup neorun
  autocmd!
  autocmd TermClose * :call TerminalOnTermClose(0+expand('<abuf>'))
augroup end

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

" Move text around in visual mode
vnoremap <left> <nop>
vnoremap <right> <nop>
vnoremap <up> xkP`[V`]
vnoremap <down> xp`[V`]

" Exit insert mode and save just by hitting CTRL-s
imap <c-s> <esc>:w<cr>
nmap <c-s> <esc>:w<cr>

" Don't jump around when using * to search for word under cursor
" Often I just want to see where else a word appears
nnoremap * ma*`a

" Insert current file name with \f in insert mode
" Useful when writing rake tasks or java classes
inoremap \f <C-R>=expand("%:t:r")<CR>

" insert path to current file
" in command (:) mode
cnoremap %% <C-R>=expand('%:h').'/'<cr>

" correct spelling from insert mode by hitting CTRL-l
inoremap <c-l> <esc>:call CorrectSpelling()<cr>a

" add Ex command for finding Ruby Conditionals
command! FindConditionals :normal /\<if\>\|\<unless\>\|\<and\>\|\<or\>\|||\|&&<cr>

command! Cons :sp tmp/console.rb

" add Ex command for removing characters sometimes present when copying from
" tex compiled PDF files
command! RemoveFancyCharacters :call RemoveFancyCharacters()

command! UpdateTranslations :Dispatch rails i18n:update_translations

function! FormatRubyCodeFn(line1_num, line2_num)
  let filename = expand('%:p')
  echom filename
  echom a:line1_num
  echom a:line2_num
endfunction

command! -range FormatRubyCode :call FormatRubyCodeFn(<line1>, <line2>)

" Merge tabs
nmap <C-W>M :call MergeTabs()<CR>

" Save file with sudo by doing :w!!
cmap w!! w !sudo tee % >/dev/null

" Make terminal mode behave more like any other mode
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

" Useful mappings when writing Haskell
nnoremap <leader>$ :normal ds(i$ <cr>
nnoremap <leader>. :normal ds(i. <cr>

" PCRE search
noremap <leader>/ /\v
noremap <leader>? ?\v

" Quickly insert semicolon at end of line
noremap <leader>; maA;<esc>`a

noremap <leader>, ^/\v(\)$\|,\|\([^(]+\)$)<cr>li<cr><esc>

vnoremap <leader>= :Tabularize /

nmap <leader>gr "*gr

nnoremap <leader>A :call YankWholeBuffer(1)<cr>
nnoremap <leader>J :call GotoDefinitionInSplit(1)<cr>
nnoremap <leader>O :!open %<cr><cr>
nnoremap <leader>T :sp term://zsh<cr>

" vnoremap <leader>v :VtrSendLinesToRunner<cr>
" nnoremap <leader>V <Plug>SetTmuxVars
" nnoremap <leader>v <Plug>SendSelectionToTmux

" vmap <C-c><C-c> <Plug>SendSelectionToTmux
" nmap <C-c><C-c> <Plug>NormalModeSendToTmux
" nmap <C-c>r <Plug>SetTmuxVars

nmap <leader>v :normal V<cr><Plug>SendSelectionToTmux
vmap <leader>v <Plug>SendSelectionToTmux
nmap <leader>V <Plug>SetTmuxVars

nnoremap <leader>W :wq<cr>
nnoremap <leader>a :call YankWholeBuffer(0)<cr>
nnoremap <leader>ag viw:call SearchForSelectedWord()<cr>
nnoremap <leader>as :call rails_test#hsplit_spec("spec")<cr>
nnoremap <leader>av :call rails_test#vsplit_spec("spec")<cr>
" nnoremap <leader>as :call rails_test#hsplit_spec("test")<cr>
" nnoremap <leader>av :call rails_test#vsplit_spec("test")<cr>
nnoremap <leader>b :Buffers<cr>
nnoremap <leader>cc :Dispatch script/check-style<cr>
nnoremap <leader>cd :cd %:p:h<cr>:pwd<cr>
nnoremap <leader>cl :set cursorcolumn!<cr>
nnoremap <leader>cm :!chmod +x %<cr>
nnoremap <leader>dc :call FuzzyFileFind("app/controllers")<cr>
nnoremap <leader>dj :call FuzzyFileFind("app/jobs")<cr>
nnoremap <leader>dm :call FuzzyFileFind("app/models")<cr>
nnoremap <leader>ds :call FuzzyFileFind("app/services")<cr>
nnoremap <leader>dv :call FuzzyFileFind("app/views")<cr>
nnoremap <leader>dz :call FuzzyFileFind("app/serializers")<cr>
nnoremap <leader>di :Dispatch<space>
nnoremap <leader>do :call ToggleRubyBlockSyntax()<cr>
nnoremap <leader>dt :Tags<cr>
nnoremap <leader>ee vip:s/rspec //g<cr>vip:s/:.*//g<cr>gsipvip:!uniq<cr>
nnoremap <leader>es :UltiSnipsEdit<cr>
nnoremap <leader>ev :tabedit $MYVIMRC<cr>:lcd ~/dotfiles<cr>
nnoremap <leader>f :call FuzzyFileFind("")<cr>
nnoremap <leader>ga :Gwrite<cr>
nnoremap <leader>gc :Gcommit -v<cr>
nnoremap <leader>gp :Gpush<cr>
nnoremap <leader>gs :Gstatus<cr>:30winc +<cr>
nnoremap <leader>gu :GundoToggle<cr>
nnoremap <leader>h :nohlsearch<cr>
nnoremap <leader>hc :HdevtoolsClear<cr>
nnoremap <leader>ht :HdevtoolsType<cr>
nnoremap <leader>i :call IndentEntireFile()<cr>
nnoremap <leader>j :call GotoDefinitionInSplit(0)<cr>
nnoremap <leader>k :w<cr>:call spectacular#run_tests_with_current_line()<cr>
nnoremap <leader>ln :lnext<cr>
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
nnoremap <leader>pr :call branch_notes#open()<cr>
nnoremap <leader>q :call CloseExtraPane()<cr>
nnoremap <leader>rbi :w\|:Dispatch bundle install<cr>
nnoremap <leader>rd :redraw!<cr>
nnoremap <leader>re :call FixFormatting()<cr>
nnoremap <leader>rel :call PromoteToLet()<cr>
nnoremap <leader>ri :RunInInteractiveShell<space>
nnoremap <leader>rn :call RenameFile()<cr>
nnoremap <leader>rr :sp term://cargo run<cr>
nnoremap <leader>rt :!retag<cr>
nnoremap <leader>rf :vs ~/.rspec_failures<cr>
nnoremap <leader>sb :call notable#open_notes_file()<cr>
nnoremap <leader>se :SyntasticToggleMode<cr>:w<cr>
nnoremap <leader>sr :sp term://stack\ ghci<cr>
nnoremap <leader>ss :w\|:SyntasticCheck<cr>
nnoremap <leader>st :sp<cr>:term zsh<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>:nohlsearch<cr>
nnoremap <leader>t :w<cr>:NeomakeCancelJobs<cr>:call spectacular#run_tests()<cr>
nnoremap <leader>wip :!git-wip<cr>
nnoremap <leader>wtf oRails.logger.debug "#" * 80<c-m>Rails.logger.debug caller<c-m>Rails.logger.debug "#" * 80<esc>
nnoremap <leader>x :set filetype=
nnoremap <leader>z :call CorrectSpelling()<cr>
nnoremap <silent> gD :Dash<cr>
noremap <leader>l :call MakeList()<cr>

vnoremap <leader>ml :call PasteMarkdownLink()<cr>
vnoremap <leader>mlc :call FormatSmlComments()<cr>

" ========================================
" == Misc plugin config ==================
" ========================================

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

" Hack to make CTRL-h work in Neovim
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

let g:rustfmt_autosave = 1

let g:racer_cmd = "/Users/davidpdrsn/.cargo/bin/racer"
let g:racer_experimental_completer = 1


" ========================================
" == Test running ========================
" ========================================

call spectacular#reset()

" call spectacular#add_test_runner('ruby, javascript, eruby, coffee, haml, yml, sql', ':call FifoRun("bin/rspec --fail-fast {spec}")' , '_spec.rb')
" call spectacular#add_test_runner('ruby, javascript, eruby, coffee, haml, yml, sql, eruby.yaml, yaml', ':call FifoRun("bin/rspec {spec}:{line-number}")' , '_spec.rb')

call spectacular#add_test_runner('ruby, javascript, eruby, coffee, haml, yml', ':call TerminalRun("bin/rspec --fail-fast {spec}")' , '_spec.rb')
call spectacular#add_test_runner('ruby, javascript, eruby, coffee, haml, yml', ':call TerminalRun("bin/rspec --fail-fast {spec}:{line-number}")' , '_spec.rb')

" call spectacular#add_test_runner('ruby, javascript, eruby, coffee, haml, yml', ':call TerminalRun("rake test {spec}")' , '_test.rb')

call spectacular#add_test_runner('rust, toml', ':call TerminalRun("cargo test")' , '.rs')

call spectacular#add_test_runner('haskell', ':call TerminalRun("stack build")' , '.hs')
