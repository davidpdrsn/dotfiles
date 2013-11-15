Abolish adn and
Abolish soem some
Abolish witdh width
Abolish tehn then
Abolish waht what
Abolish Im I'm
Abolish ot to
Abolish havin having
Abolish vim Vim
Abolish hightlight highlight
Abolish ypo typo
Abolish ype type
Abolish alot a lot
Abolish aswell as well

augroup abbreviationGroup
  autocmd!

  autocmd FileType mkd :Abolish -buffer dont don't
  autocmd FileType mkd :Abolish -buffer wasnt wasn't
  autocmd FileType mkd :Abolish -buffer isnt isn't
  autocmd FileType mkd :Abolish -buffer i I
augroup END
