Bundle 'Raimondi/delimitMate'
Bundle 'SirVer/ultisnips'
Bundle 'altercation/vim-colors-solarized'
Bundle 'christoomey/vim-colors-ctoomey'
Bundle 'christoomey/vim-tmux-navigator'
Bundle 'davidpdrsn/vim-spectacular'
Bundle 'ecomba/vim-ruby-refactoring'
Bundle 'ervandew/supertab'
Bundle 'gmarik/vundle'
Bundle 'godlygeek/tabular'
Bundle 'https://github.com/wincent/Command-T.git'
Bundle 'jgdavey/tslime.vim'
Bundle 'kana/vim-textobj-user'
Bundle 'kchmck/vim-coffee-script'
Bundle 'nelstrom/vim-textobj-rubyblock'
Bundle 'rking/ag.vim'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/syntastic'
Bundle 'terryma/vim-multiple-cursors'
Bundle 'tpope/vim-abolish'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-rails'
Bundle 'tpope/vim-surround'
Bundle 'travitch/hasksyn'
Bundle 'vim-ruby/vim-ruby'
Bundle 'wikitopian/hardmode'

" Enable built-in matchit plugin
runtime macros/matchit.vim

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
let g:CommandTWildIgnore=&wildignore . "**/tmp/*,"
      \."**/bower_components/*,"
      \."**/node_modules/*,"
      \."**/_site/*,"
      \."**/vendor/*,"
      \."**/*.class,"
      \."**/*.giff,"
      \."**/*.png,"
let g:CommandTMaxHeight=20

let g:UltiSnipsEditSplit = 'horizontal'
let g:UltiSnipsSnippetDirectories = ["snippets"]

let g:multi_cursor_exit_from_visual_mode = 0

let g:ruby_refactoring_map_keys = 0

let g:syntastic_ruby_checkers=['rubylint']

let NERDTreeIgnore = ['\.class$']
