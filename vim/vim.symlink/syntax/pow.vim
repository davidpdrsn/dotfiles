" Vim syntax file for Fasto.
" Created by Oleksandr Shturmov <oleks@oleks.info> on November 1, 2014.

if exists("b:current_syntax")
  finish
end

syn keyword powKeyword static dynamic
" syn keyword powType int char bool
" syn keyword powFunction read write iota replicate map reduce

syn match powString "\|\([ -!#-&(-[\]-~]\|\\[\x0-\x7f]\)*\|"

syn match powComment "{{.*}}"

highlight link powKeyword Keyword
" highlight link powType Type
" highlight link powFunction Function

highlight link powString String

highlight link powComment Comment
