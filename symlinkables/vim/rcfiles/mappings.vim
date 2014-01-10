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
