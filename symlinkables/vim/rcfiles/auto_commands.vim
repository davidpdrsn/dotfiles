augroup highlightingLongLines
  autocmd!
  autocmd FileType * match ErrorMsg '\%>100v.\+'
  autocmd FileType sml match ErrorMsg '\%>80v.\+'
  autocmd FileType markdown match none
  autocmd FileType text match none
  autocmd FileType html match none
  autocmd FileType xhtml match none
  autocmd FileType eruby match none
  autocmd FileType unite match none
  autocmd FileType vimfiler match none
  autocmd FileType plaintex match none
  autocmd FileType conf match none
  autocmd FileType tex match none
  autocmd FileType qf match none
  autocmd FileType php match none
  autocmd FileType java match none
  autocmd FileType help match none
  autocmd FileType vim match none
  autocmd FileType calendar match none
augroup END

augroup configureFolds
  autocmd!
  autocmd FileType mkd setlocal spell nofoldenable
  autocmd FileType text setlocal spell nofoldenable
  autocmd FileType text setlocal spell nofoldenable
  autocmd FileType vim setlocal foldmethod=marker
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
  " set comments for SML
  autocmd FileType sml set commentstring=(*\ %s\ *)
  " insert current file name with \f in insert mode in java files
  autocmd FileType java inoremap <buffer> \f <C-R>=expand("%:t:r")<CR>
  " jump the top in git commit messages
  autocmd FileType gitcommit normal gg
  autocmd BufLeave * silent! write
augroup END
