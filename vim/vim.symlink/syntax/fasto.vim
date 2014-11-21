" Vim syntax file for Fasto.
" Created by Oleksandr Shturmov <oleks@oleks.info> on November 1, 2014.

if exists("b:current_syntax")
  finish
end

syn keyword fastoKeyword fun if then else let in
syn keyword fastoType int char bool
syn keyword fastoFunction read write iota replicate map reduce

syn match fastoString "\"\([ -!#-&(-[\]-~]\|\\[\x0-\x7f]\)*\""

syn match fastoComment "//.*$"

highlight link fastoKeyword Keyword
highlight link fastoType Type
highlight link fastoFunction Function

highlight link fastoString String

highlight link fastoComment Comment
