" Vim syntax file
" Language:	Janus
" Maintainer:	Michael Budde <mbudde@gmail.com>
" Last Change:	March 2013


if exists("b:current_syntax")
  finish
endif

syn keyword janusKeyword local delocal true false nil skip
syn keyword janusKeyword call uncall
syn keyword janusKeyword push pop error show print printf
syn keyword janusStatement procedure nextgroup=janusProcedure skipwhite
syn keyword janusConditional if then else fi
syn keyword janusRepeat from do loop until
syn keyword janusOperator empty top size

syn keyword janusType int stack

syn match   janusProcedure "\w\+" contained


syn match   janusSpecialChar      contained "\\\([0-9]\+\|o[0-7]\+\|x[0-9a-fA-F]\+\|[\"\\'&\\abfnrtv]\|^[A-Z^_\[\\\]]\)"
syn match   janusSpecialChar      contained "\\\(NUL\|SOH\|STX\|ETX\|EOT\|ENQ\|ACK\|BEL\|BS\|HT\|LF\|VT\|FF\|CR\|SO\|SI\|DLE\|DC1\|DC2\|DC3\|DC4\|NAK\|SYN\|ETB\|CAN\|EM\|SUB\|ESC\|FS\|GS\|RS\|US\|SP\|DEL\)"
syn match   janusSpecialCharError contained "\\&\|'''\+"
syn region  janusString	          start=+"+  skip=+\\\\\|\\"+  end=+"+  contains=janusSpecialChar

syn match   janusNumber	"\<\%([1-9]\d*\|0\)\>"

syn keyword janusCommentTodo   TODO FIXME XXX TBD contained
syn match   janusLineComment   "\/\/.*" contains=janusCommentTodo
syn region  janusComment       start="/\*" end="\*/" contains=janusCommentTodo


hi def link janusString         String
hi def link janusSpecialChar    SpecialChar
hi def link janusNumber         Number
hi def link janusProcedure      Function
hi def link janusStatement      Statement
hi def link janusConditional    Conditional
hi def link janusRepeat         Repeat
hi def link janusOperator       Operator
hi def link janusKeyword        Keyword
hi def link janusType           Type
hi def link janusComment        Comment
hi def link janusLineComment    Comment
hi def link janusCommentTodo    Todo


let b:current_syntax = "janus"
