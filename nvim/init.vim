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
Plug 'andymass/vim-matchup'
Plug 'cespare/vim-toml'
Plug 'chriskempson/base16-vim'
Plug 'christoomey/Vim-g-dot'
Plug 'christoomey/vim-sort-motion'
Plug 'christoomey/vim-system-copy'
Plug 'christoomey/vim-tmux-navigator'
Plug 'christoomey/vim-tmux-runner'
Plug 'davidpdrsn/vim-notable'
Plug 'davidpdrsn/vim-spectacular'
Plug 'derekwyatt/vim-scala'
Plug 'ekalinin/Dockerfile.vim'
Plug 'google/vim-jsonnet'
Plug 'itchyny/lightline.vim'
Plug 'jgdavey/tslime.vim'
Plug 'jparise/vim-graphql'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'justinmk/vim-sneak'
Plug 'kana/vim-textobj-entire' " ae
Plug 'kana/vim-textobj-user'
Plug 'machakann/vim-highlightedyank'
Plug 'maximbaz/lightline-ale'
Plug 'nanotech/jellybeans.vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'pbrisbin/vim-mkdir'
Plug 'pest-parser/pest.vim'
Plug 'plasticboy/vim-markdown'
Plug 'rking/ag.vim'
Plug 'rust-lang/rust.vim'
Plug 'tek/vim-textobj-ruby' " ir, if, ic, in
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-bundler'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-rbenv'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-vinegar'
Plug 'uarun/vim-protobuf'
Plug 'vim-ruby/vim-ruby'

call plug#end()

let $FZF_DEFAULT_COMMAND = "rg --files --no-ignore-vcs --hidden | rg -v \"(^|/)(target|\.git)/\" | rg -v \".DS_Store\""
let g:fzf_preview_window = ''

command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case -- '.shellescape(<q-args>), 1,
  \   fzf#vim#with_preview(), <bang>0)

inoremap <expr> <c-x><c-f> fzf#vim#complete#path('rg --files')

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

let base16colorspace=256
colorscheme base16-irblack
set termguicolors

set fillchars+=vert:\             " Don't show pipes in vertical splits
set grepprg=rg\ --color=never
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
set timeout                       " Lower the delay of escaping out of other modes
set visualbell                    " Disable annoying beep
set wildmenu                      " Enable command-line like completion
set wrap                          " Wrap long lines
set ttimeout                      " Set behavior of when partial mappings are pressed
set ttimeoutlen=1                 " Don't delay execution of a mapping
set nojoinspaces                  " Insert only one space when joining lines that contain sentence-terminating punctuation like `.`.
set path+=**
set updatetime=100
set mouse=nv

" UI
set noshowmode
set laststatus=2                  " Always show the status line
set linebreak                     " Don't break lines in the middle of words
set list                          " Show some more characters
set listchars=tab:▸\
set listchars+=extends:❯
set listchars+=nbsp:␣
set listchars+=precedes:❮
set listchars+=trail:·
set nocursorcolumn                " Don't highlight the current column
set cursorline                    " Highlight the current line
set number                        " Don't show line numbers
set numberwidth=3                 " The width of the number column
set relativenumber                " Show relative numbers
set guifont=Input\ Mono:h11       " Set GUI font
set guioptions-=T                 " No tool bar in MacVim
set guioptions-=r                 " Also no scrollbar
set guioptions-=L                 " Really no scrollbar
set signcolumn=yes
set cmdheight=1
set conceallevel=0

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

highlight TermCursor ctermfg=red guifg=red
highlight CocHighlightText guibg=#4d4d4d

" ========================================
" == Auto commands =======================
" ========================================

" hide/show the cursor line when leaving/entering a window
augroup CursorLine
    au!
    au VimEnter * setlocal cursorline
    au WinEnter * setlocal cursorline
    au BufWinEnter * setlocal cursorline
    au WinLeave * setlocal nocursorline
augroup END

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

  " when in a git commit buffer go the beginning
  autocmd FileType gitcommit au! BufEnter COMMIT_EDITMSG call setpos('.', [0, 1, 1, 0])

  " save files when focus is lost
  autocmd BufLeave * silent! update

  " always have quickfix window take up all the horizontal space
  autocmd FileType qf wincmd J

  " configure indentation for python
  autocmd FileType python set expandtab tabstop=4 softtabstop=4 shiftwidth=4

  " configure indentation for rust
  autocmd FileType rust set expandtab tabstop=4 softtabstop=4 shiftwidth=4

  " disable spell checking in vim help files
  autocmd FileType help set nospell

  autocmd BufWinEnter,WinEnter term://* startinsert
  autocmd BufLeave term://* stopinsert

  autocmd! BufWritePost *.tex call CompileLatex()

  autocmd BufEnter,FocusGained * checktime

  " run rustfmt by pressing enter
  autocmd FileType rust nnoremap <buffer> <cr> :w<cr>:RustFmt<cr>:w<cr>

  " show vertical cursor line in yaml
  autocmd FileType yaml setlocal cursorcolumn

  autocmd CursorHold * silent call CocActionAsync('highlight')

  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
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

" Don't wanna retrain my fingers
command! W w
command! Q q
command! Qall qall

" Close all buffers without quitting vim
command! Killall bufdo bwipeout

command! ImportBuild :call ImportBuild()

" Make Y work as expected
nnoremap Y y$

" Intuitive movement over long lines
nnoremap k gk
nnoremap j gj

" Resize windows with the shift+arrow keys
nnoremap <s-up> 10<C-W>+
nnoremap <s-down> 10<C-W>-
nnoremap <s-left> 3<C-W>>
nnoremap <s-right> 3<C-W><

" Move text around in visual mode
vnoremap <left> <nop>
vnoremap <right> <nop>
vnoremap <up> xkP`[V`]
vnoremap <down> xp`[V`]

" Exit insert mode and save just by hitting CTRL-s
imap <c-s> <esc>:w<cr>
nmap <c-s> :w<cr>

" Don't jump around when using * to search for word under cursor
" Often I just want to see where else a word appears
nnoremap * :let @/ = '\<'.expand('<cword>').'\>'\|set hlsearch<C-M>

" Insert current file name with \f in insert mode
inoremap \f <C-R>=expand("%:t:r")<CR>

" insert path to current file in command mode
cnoremap %% <C-R>=expand('%:h').'/'<cr>

" correct spelling from insert mode by hitting CTRL-l
inoremap <c-l> <esc>:call CorrectSpelling()<cr>a

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

" Quickly insert semicolon at end of line
noremap <leader>; maA;<esc>`a

" Quickly insert comma at end of line
noremap <leader>, maA,<esc>`a

nnoremap <leader>T :call <SID>run_rust_tests()<cr>

function! s:run_rust_tests()
  if &modified
    write
  end
  call SmartRun("cargo test --all")
endfunction

nmap <leader>v :normal V<cr><Plug>SendSelectionToTmux
vmap <leader>v <Plug>SendSelectionToTmux
nmap <leader>V <Plug>SetTmuxVars

nnoremap <leader>b :Buffers<cr>
nnoremap <leader>cd :cd %:p:h<cr>:pwd<cr>
nnoremap <leader>cm :!chmod +x %<cr>
nnoremap <leader>di :Dispatch<space>
nnoremap <leader>es :UltiSnipsEdit<cr>
nnoremap <leader>ev :tabedit $MYVIMRC<cr>:lcd ~/dotfiles<cr>
nnoremap <leader>f :call FuzzyFileFind("")<cr>
nnoremap <leader>h :nohlsearch<cr>
nnoremap <leader>k :w<cr>:call spectacular#run_tests_with_current_line()<cr>
nnoremap <leader>ll :BLines<cr>
nnoremap <leader>ns :set spell!<cr>
nnoremap <leader>p :call PasteFromSystemClipBoard()<cr>
nnoremap <leader>pc :PlugClean<cr>
nnoremap <leader>pi :PlugInstall<cr>
nnoremap <leader>pr :call branch_notes#open()<cr>
nnoremap <leader>pu :PlugUpdate<cr>
nnoremap <leader>q :call CloseExtraPane()<cr>
nnoremap <leader>rd :redraw!<cr>
nnoremap <leader>re :call FixFormatting()<cr>
nnoremap <leader>rn :call RenameFile()<cr>
nnoremap <leader>sb :call notable#open_notes_file()<cr>
nnoremap <leader>st :sp term://zsh<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>:nohlsearch<cr>
nnoremap <leader>t :w<cr>:call spectacular#run_tests()<cr>
nnoremap <leader>w :Windows<cr>
nnoremap <leader>x :set filetype=
nnoremap <leader>z :call CorrectSpelling()<cr>

" coc
nmap <leader>la <Plug>(coc-codeaction-selected)<cr>
nmap <leader>ls :CocList symbols<cr>
nmap <leader>ld :CocList diagnostics<cr>
nmap <leader>lc :CocList commands<cr>
nmap <leader>le :CocCommand explorer<cr>
nmap <leader>lr <Plug>(coc-rename)
nmap <leader>lx <Plug>(coc-fix-current)

inoremap <silent><expr> <c-j> coc#refresh()

nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

nmap <silent> gd :call CocAction('jumpDefinition', 'vsplit')<cr>
nmap <silent> gD <Plug>(coc-definition)

nmap <silent> gy :call CocAction('jumpTypeDefinition', 'vsplit')<cr>
nmap <silent> gY <Plug>(coc-type-definition)

nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

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

let g:rustfmt_autosave = 0

let g:rufo_auto_formatting = 0

let g:toggle_list_no_mappings = 0

let g:elm_setup_keybindings = 0

let g:lightline = {
  \ 'colorscheme': 'jellybeans',
  \ 'enable': {
  \   'statusline': 1,
  \   'tabline': 0
  \ },
  \ 'active': {
  \   'right': [[ 'lineinfo' ]],
  \   'left': [[ 'mode', 'paste' ],
  \            [ 'readonly', 'relativepath', 'gitbranch', 'modified', 'cocstatus' ]]
  \ },
  \ 'component_function': {
  \   'gitbranch': 'fugitive#head',
  \   'cocstatus': 'coc#status',
  \ },
  \ 'component_expand': {
  \   'linter_checking': 'lightline#ale#checking',
  \   'linter_warnings': 'lightline#ale#warnings',
  \   'linter_errors': 'lightline#ale#errors',
  \   'linter_ok': 'lightline#ale#ok',
  \ },
  \ 'component_type': {
  \   'linter_checking': 'left',
  \   'linter_warnings': 'warning',
  \   'cocstatus': 'warning',
  \   'linter_errors': 'error',
  \   'linter_ok': 'left',
  \ }
  \ }

let g:rustfmt_command = "rustfmt"

let g:highlightedyank_highlight_duration = 170

let g:diminactive_enable_focus = 1
let g:diminactive_use_colorcolumn = 1
let g:diminactive_use_syntax = 0

" ========================================
" == Test running ========================
" ========================================

call spectacular#reset()

call spectacular#add_test_runner(
      \ 'ruby, javascript, eruby, coffee, haml, yml',
      \ ':call SmartRun("rspec {spec}")',
      \ ''
      \ )

call spectacular#add_test_runner(
      \ 'ruby, javascript, eruby, coffee, haml, yml',
      \ ':call SmartRun("rspec {spec}:{line-number}")',
      \ ''
      \ )

source ~/.vimrc.local.vim
