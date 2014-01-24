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
noremap <leader>bi :BundleInstall<cr>
noremap <leader>bc :BundleClean<cr>
noremap <leader>bu :BundleUpdate<cr>

"-- c --"
" comment closing HTML tag
noremap <leader>ct my^lyy%p/classf"v0c.f"D:s/ /./eg<cr>gcckJ:nohlsearch<cr>`y
noremap <leader>cd :cd %:p:h<cr>:pwd<cr>

"-- d --"
" delete wrapping HTML tag
noremap <leader>dt ^lma%mb'ajV'bk<'add'bdd
noremap <leader>do :call ToggleRubyBlockSyntax()<cr>
noremap <leader>di :Dispatch<space>

"-- e --"
noremap <leader>ev :tabedit $MYVIMRC<cr>
noremap <leader>es :UltiSnipsEdit<cr>

"-- f --"
noremap <leader>f :Ag -i ""<left>

"-- g --"
noremap <leader>g :Git<space>
noremap <leader>gb :Gblame<cr>
noremap <leader>gc :Gcommit<cr>
noremap <leader>gd :Gdiff<cr>
noremap <leader>gp :Git push<cr><cr>
noremap <leader>gr :Gremove<cr>
noremap <leader>gs :Gstatus<cr>
noremap <leader>ga :Gwrite<cr>
noremap <leader>gg :w<cr>:Gwrite<cr>:Gcommit -m 'update'<cr>:Git push<cr><cr>:e<cr>
noremap <leader>gu :GundoToggle<cr>

"-- h --"

"-- i --"

"-- j --"
noremap <leader>j :tabe<cr>:cd ~/hax/journal/entries<cr>:e.<cr>
noremap <leader>J :call AddJavaFile(PathToCurrentFile())<cr>

"-- k --"

"-- l --"
noremap <leader>l :tabe ~/Documents/tool_sharpening_list.markdown<cr>

"-- m --"
" For quickly making markdown headings
noremap <leader>mh yypVr=k
noremap <leader>m2h yypVr-k
" Format SML comments
vnoremap <leader>mlc ^:s/(\*/ */g<cr>gv:s/ \*)//g<cr>A *)<esc>gvo<esc>r(gvo<esc>:nohlsearch<cr>

"-- n --"
noremap <leader>N :set number!<cr>
noremap <leader>n :NERDTreeToggle<cr>

"-- o --"
noremap <leader>o :only<cr>
noremap <leader>O :!open %<cr><cr>

"-- p --"
" Paste from system clipboard
noremap <leader>p <esc>o<esc>"+]p

"-- q --"
noremap <leader>q :cclose<cr>

"-- r --"
noremap <leader>rn :call RenameFile()<cr>
noremap <leader>re :%s/\r\(\n\)/\1/eg<cr>:retab<cr>:%s/\s\+$//e<cr>:nohlsearch<cr>
vnoremap <leader>r :!ruby<cr>
noremap <leader>rd :redraw!<cr>
noremap <leader>rr :w\|:call RunCurrentFile()<cr>

nnoremap <leader>rap  :RAddParameter<cr>
nnoremap <leader>rcpc :RConvertPostConditional<cr>
nnoremap <leader>rel :call PromoteToLet()<cr>
vnoremap <leader>rec  :RExtractConstant<cr>
vnoremap <leader>relv :RExtractLocalVariable<cr>
nnoremap <leader>rit  :RInlineTemp<cr>
vnoremap <leader>rrlv :RRenameLocalVariable<cr>
vnoremap <leader>rriv :RRenameInstanceVariable<cr>
vnoremap <leader>rem  :RExtractMethod<cr>

"-- s --"
noremap <leader>ss :vsp<cr>:A<cr>
noremap <leader>sv :source $MYVIMRC<cr>

"-- t --"
map <leader>t :w<cr>:call spectacular#run_tests()<cr>

"-- u --"

"-- v --"
noremap <leader>v :vsplit<cr>

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
