" ========================================
" == Mandatory setup =====================
" ========================================

filetype off

" ========================================
" == Plugins =============================
" ========================================

call plug#begin('~/.config/nvim/plugged')

" Plug 'Raimondi/delimitMate'
Plug 'SirVer/ultisnips'
Plug 'andymass/vim-matchup'
Plug 'cespare/vim-toml'
Plug 'chriskempson/base16-vim'
Plug 'christoomey/Vim-g-dot'
Plug 'christoomey/vim-sort-motion'
Plug 'christoomey/vim-system-copy'
Plug 'christoomey/vim-tmux-navigator'
Plug 'davidpdrsn/vim-spectacular'
Plug 'ekalinin/Dockerfile.vim'
Plug 'google/vim-jsonnet'
Plug 'kana/vim-textobj-entire'
Plug 'kana/vim-textobj-user'
Plug 'machakann/vim-highlightedyank'
Plug 'mg979/vim-visual-multi'
Plug 'pbrisbin/vim-mkdir'
Plug 'plasticboy/vim-markdown'
Plug 'rust-lang/rust.vim'
Plug 'rust-lang/rust.vim'
Plug 'stephpy/vim-yaml'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-vinegar'
Plug 'uarun/vim-protobuf'

" new and shiny
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/nvim-cmp'
Plug 'windwp/nvim-autopairs'
Plug 'simrat39/rust-tools.nvim'
Plug 'nvim-lua/popup.nvim'
Plug 'saecki/crates.nvim'
Plug 'lewis6991/gitsigns.nvim'
Plug 'beauwilliams/focus.nvim'

Plug 'itchyny/lightline.vim'
Plug 'spywhere/lightline-lsp'

call plug#end()

" ========================================
" == General config ======================
" ========================================

" misc
filetype plugin indent on         " Enable good stuff
syntax enable                     " Enable syntax highlighting

let base16colorspace=256

colorscheme base16-irblack
" colorscheme base16-github

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
set textwidth=80

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
set number                        " Don't show line numbers
set numberwidth=3                 " The width of the number column
set relativenumber                " Show relative numbers
set guifont=Input\ Mono:h11       " Set GUI font
set guioptions-=T                 " No tool bar in MacVim
set guioptions-=r                 " Also no scrollbar
set guioptions-=L                 " Really no scrollbar
set cursorline

set signcolumn=yes
" set signcolumn=number

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
highlight SpecialComment guifg=#6c6c66

" This is nice but breaks floating windows
" set winwidth=84
" try
"   set winminwidth=20
" catch
" endtry
" set winheight=7
" set winminheight=7
" set winheight=999

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

  " show vertical cursor line in yaml
  autocmd FileType yaml setlocal cursorcolumn
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

command! Toml :call <SID>open_cargo_toml()<CR>

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

function! PythonUnmaps()
    silent! nunmap <buffer> <c-s>
endfunction
augroup my_python_overrides
    au!
    autocmd FileType TelescopePrompt call PythonUnmaps()
augroup END

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

function! s:run_rust_tests()
  if &modified
    write
  end
  call SmartRun("cargo test --all --all-features")
endfunction

nnoremap gr <cmd>Telescope lsp_references<cr>

nnoremap <leader>la <cmd>Telescope lsp_code_actions<cr>
nnoremap <leader>ld <cmd>Telescope lsp_workspace_diagnostics<cr>
nnoremap <leader>ls <cmd>Telescope lsp_dynamic_workspace_symbols<cr>
nnoremap <leader>b  <cmd>Telescope buffers<cr>
nnoremap <leader>f  <cmd>Telescope find_files<cr>

nnoremap <leader>T :call <SID>run_rust_tests()<cr>
nnoremap <leader>cm :!chmod +x %<cr>
nnoremap <leader>eV :tabedit ~/.vimrc.local.vim<cr>
nnoremap <leader>es :UltiSnipsEdit<cr>
nnoremap <leader>ev :tabedit $MYVIMRC<cr>:lcd ~/dotfiles<cr>
nnoremap <leader>h :nohlsearch<cr>
nnoremap <leader>ns :set spell!<cr>
nnoremap <leader>p :call PasteFromSystemClipboard()<cr>
nnoremap <leader>pc :PlugClean<cr>
nnoremap <leader>pi :PlugInstall<cr>
nnoremap <leader>pu :PlugUpdate<cr>
nnoremap <leader>q :call CloseExtraPane()<cr>
nnoremap <leader>rd :redraw!<cr>
nnoremap <leader>re :call FixFormatting()<cr>
nnoremap <leader>rn :call RenameFile()<cr>
nnoremap <leader>st :sp term://zsh<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>:nohlsearch<cr>
nnoremap <leader>t :w<cr>:call spectacular#run_tests()<cr>
nnoremap <leader>x :set filetype=
nnoremap <leader>z :call CorrectSpelling()<cr>

" ========================================
" == Misc plugin config ==================
" ========================================

let g:UltiSnipsEditSplit = 'horizontal'
let g:UltiSnipsSnippetDirectories = ["ultisnips"]

let g:spectacular_use_terminal_emulator = 1
let g:spectacular_debugging_mode = 0

let g:notable_notes_folder = "~/notes/"

" Hack to make CTRL-h work in Neovim
nnoremap <silent> <BS> :TmuxNavigateLeft<cr>

let g:rustfmt_command = "rustfmt"
let g:rustfmt_autosave = 0

let g:highlightedyank_highlight_duration = 170

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

let g:lightline = {}
let g:lightline.colorscheme = 'jellybeans'
let g:lightline.enable = { 'statusline': 1, 'tabline': 0 }
let g:lightline.active = {
      \ 'right': [[ 'lineinfo' ],
      \           [ 'linter_errors',
      \             'linter_warnings',
      \             'linter_infos',
      \             'linter_hints',
      \             'linter_ok'
      \           ]],
      \ 'left': [[ 'mode', 'paste' ],
      \          [ 'readonly', 'relativepath', 'modified' ]]
      \ }
let g:lightline.component_function = {
      \ }
let g:lightline.component_expand = {
      \ 'linter_hints': 'lightline#lsp#hints',
      \ 'linter_infos': 'lightline#lsp#infos',
      \ 'linter_warnings': 'lightline#lsp#warnings',
      \ 'linter_errors': 'lightline#lsp#errors',
      \ 'linter_ok': 'lightline#lsp#ok',
      \ }
let g:lightline.component_type = {
      \ 'linter_infos': 'right',
      \ 'linter_warnings': 'warning',
      \ 'linter_errors': 'error',
      \ 'linter_ok': 'right',
      \ }

let g:lightline#lsp#indicator_hints = "💡 "
let g:lightline#lsp#indicator_infos = "💁 "
let g:lightline#lsp#indicator_warnings = "🤔 "
let g:lightline#lsp#indicator_errors = "❌ "
let g:lightline#lsp#indicator_ok = "👍"

" ========================================
" == LSP setup ===========================
" ========================================

" LSP configuration
lua << END
require('nvim-autopairs').setup{}

vim.api.nvim_set_keymap('n', 'K', ':lua show_documentation()<cr>', { noremap = true, silent = true })
function show_documentation()
    local filetype = vim.bo.filetype
    if vim.tbl_contains({ 'vim','help' }, filetype) then
        vim.cmd('h '..vim.fn.expand('<cword>'))
    elseif vim.tbl_contains({ 'man' }, filetype) then
        vim.cmd('Man '..vim.fn.expand('<cword>'))
    elseif vim.fn.expand('%:t') == 'Cargo.toml' then
        require('crates').show_popup()
    else
        vim.lsp.buf.hover()
    end
end

local cmp = require('cmp')

local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  -- Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap=true, silent=true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'gy', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  -- buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  -- buf_set_keymap('n', 'K', '<cmd>lua show_documentation()<CR>', opts)
  buf_set_keymap('n', '<leader>lr', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<leader>k', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '[g', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']g', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap("n", "<leader>lf", "<cmd>RustFmt<CR>", opts)
end

cmp.setup {
  snippet = {
    expand = function(args)
      vim.fn["UltiSnips#Anon"](args.body)
    end,
  },
  mapping = {
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<C-y>'] = cmp.mapping.confirm({ select = true }),
  },
  sources = {
    { name = 'nvim_lsp' },
    { name = 'ultisnips' },
    { name = 'buffer' },
    { name = 'crates' },
  }
}

local lspconfig = require('lspconfig')

require('rust-tools').setup {
  tools = {
    hover_with_actions = false,
  },
  server = {
    on_attach = on_attach,
    flags = {
      debounce_text_changes = 150,
    },
    capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities()),
    settings = {
      ["rust-analyzer"] = {
        cargo = {
          allFeatures = true,
          autoreload = true,
          runBuildScripts = true,
        },
        checkOnSave = {
          command = "clippy",
          enable = true,
          extraArgs = { "--target-dir", "/Users/davidpdrsn/rust-analyzer-check" },
        },
        completion = {
          autoimport = {
            enable = true,
          },
        },
        diagnostics = {
          disabled = {"macro-error"},
        },
        inlayHints = {
          chainingHints = false,
          chainingHintsSeparator = "‣ ",
          enable = false,
          typeHints = false,
          typeHintsSeparator = "‣ ",
        },
        procMacro = {
          enable = true,
        },
      },
    },
  }
}

require('crates').setup {
  text = {
      loading    = "  - Loading",
      version    = "  - %s",
      prerelease = "  - %s (pre)",
      yanked     = "  - %s (yanked)",
      nomatch    = "  - No match",
      update     = "  - %s (outdated)",
      error      = "  - Error",
  },
  popup = {
      text = {
          title   = " # %s ",
          version = " %s ",
          yanked  = " %s yanked ",
          feature = " %s ",
      },
  },
}

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
    virtual_text = false,
    underline = true,
    signs = true,
  }
)

require('gitsigns').setup()
require("focus").setup()
END

" autocmd CursorHold * lua vim.lsp.diagnostic.show_line_diagnostics()

" Set completeopt to have a better completion experience
" menuone: popup even when there's only one match
" noinsert: Do not insert text until a selection is made
" noselect: Do not select, force user to select one from the menu
set completeopt=menuone,noinsert,noselect

set shortmess+=c " don't give |ins-completion-menu| messages.

let g:completion_enable_snippet = 'UltiSnips'
let g:completion_matching_strategy_list = ['exact', 'substring', 'fuzzy', 'all']
let g:completion_matching_smart_case = 1

" ========================================
" == Functions ===========================
" ========================================

function! FixFormatting()
  %s/\r\(\n\)/\1/eg
  retab
  %s/\s\+$//e
  nohlsearch
endfunction

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
  normal ma
  let word_before_correction = expand("<cword>")
  let original_setting = &spell

  set spell
  normal 1z=

  let word_after_correction = expand("<cword>")

  if tolower(word_after_correction) == word_before_correction
    undo
  endif

  normal `a
  let &spell = original_setting
endfunction

function! PasteFromSystemClipboard()
  let os = system("uname")
  if os == "Linux"
    read !xclip -selection clipboard -out
  else
    execute "normal! \<esc>o\<esc>\"+]p"
  end
endfunction

function! RemoveFancyCharacters()
  let typo = {}
  let typo["“"] = '"'
  let typo["”"] = '"'
  let typo["‘"] = "'"
  let typo["’"] = "'"
  let typo["–"] = '--'
  let typo["—"] = '---'
  let typo["…"] = '...'
  :exe ":%s/".join(keys(typo), '\|').'/\=typo[submatch(0)]/ge'
endfunction

function! CloseExtraPane()
  if &filetype == "gundo"
    execute ":GundoToggle"
  else
    execute ":cclose"
    execute ":pclose"
  end
endfunction

function! MergeTabs()
 if tabpagenr() == 1
    return
  endif
  let bufferName = bufname("%")
  if tabpagenr("$") == tabpagenr()
    close!
  else
    close!
    tabprev
  endif
  vsplit
  execute "buffer " . bufferName
endfunction

function! FuzzyFileFind(path)
  execute "Files " . a:path
endfunction

" <test-running-functions>
  " Functions used to run tests in a terminal split and automatically closing
  " the split if the tests are green. If they're red, jump forward to the
  " word 'Failure'
  function! TerminalRun(cmd)
    execute "new"
    call termopen(a:cmd, {
          \ 'on_exit': function('TerminalOnExit'),
          \ 'buf': expand('<abuf>')
          \})
    execute "normal i"
  endfunction

  function! TerminalOnExit(job_id, exit_code, event) dict
    if a:exit_code == 0
      execute "bd! " . s:test_buffer_number
      wincmd =
    else
      wincmd =
    endif
  endfunction

  function! TerminalOnTermClose(buf)
    let s:test_buffer_number = a:buf
  endfunction
" </test-running-functions>

function! FifoRun(cmd)
  let pwd = getcwd()
  execute "silent !runner --pwd " . pwd . " --cmd '" . a:cmd . "'"
endfunction

function! SmartRun(cmd)
  silent! let output = system('runner --check')

  if output == "Found at least one instance running\n"
    call FifoRun(a:cmd)
  else
    call TerminalRun(a:cmd)
  endif
endfunction

" ========================================
" == Local overrides =====================
" ========================================

source ~/.vimrc.local.vim
