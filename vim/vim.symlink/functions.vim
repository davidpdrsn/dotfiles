" ========================================
" == Functions ===========================
" ========================================

function! FixFormatting()
  %s/\r\(\n\)/\1/eg
  retab
  %s/\s\+$//e
  nohlsearch
endfunction

function! FormatSmlComments()
  normal ^
  s/(\*/ */g
  normal gv
  s/ \*)//g
  normal A *)
  normal gvo
  normal r(gvo
  nohlsearch
endfunction

function! YankWholeBuffer(to_system_clipboard)
  if a:to_system_clipboard
    normal maggVG"*y`a
  else
    normal maggyG`a
  endif
endfunction

function! MakeMarkdownHeading(level)
  if a:level == 1
    normal! yypVr=k
  elseif a:level == 2
    normal! yypVr-k
  endif
endfunction

function! PromoteToLet()
  normal Ilet(:
  normal f=hi)
  normal f=s{
  normal lxA }
  execute "normal ddma?\\v(let|describe|context)\<cr>p=="

  if getline(line(".") + 1) != ""
    normal o
  end

  normal `a
endfunction

function! ToggleRubyBlockSyntax()
  if match(getline('.'), "do") != -1
    execute "normal! ^/do\<cr>ciw{"
    execute "normal! lxma"
    execute "normal! jjdd`aJA }"
  else
    execute "normal! ^f{sdo"
    execute "normal! /\|\<cr>nli\<cr>"
    execute "normal! $xxoend"
    execute "normal! kk"
  end
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
  let original_setting = &spell

  set spell
  normal 1z=

  let &spell = original_setting
endfunction

function! ToggleBackgroundColor()
  tabedit ~/.vimrc.local
  normal gg/\v(light|dark)
  normal n
  if match(getline('.'), "light") != -1
    normal ciwdark
  else
    normal ciwlight
  endif
  write
  tabclose
endfunction

function! RunCurrentFile()
  if &filetype == "ruby"
    call RunCommand("ruby " . PathToCurrentFile())
  elseif &filetype == "sml"
    call RunCommand("rlwrap mosml -P full " . PathToCurrentFile())
  elseif &filetype == "javascript"
    call RunCommand("node " . PathToCurrentFile())
  elseif &filetype == "shell"
    call RunCommand("sh " . PathToCurrentFile())
  elseif &filetype == "python"
    call RunCommand("python " . PathToCurrentFile())
  elseif &filetype == "haskell"
    call RunCommand("ghci " . PathToCurrentFile())
  elseif &filetype == "coffee"
    call RunCommand("run_coffeescript " . PathToCurrentFile())
  elseif &filetype == "tex"
    call RunCommand("pdflatex " . PathToCurrentFile() . " && open " . substitute(expand("%"), "\.tex$", ".pdf", ""))
  elseif &filetype == "java"
    call RunCommand("javac *.java && java " . substitute(expand("%"), "\.java$", "", ""))
  else
    echo "Dunno how to run such a file..."
  endif
endfunction

function! RunCommand(cmd)
  exec '!clear & ' . a:cmd
endfunction

function! PathToCurrentFile()
  return expand('%:p')
endfunction

function! InTmux()
  silent exec '!in_tmux'
  exec "redraw!"

  if v:shell_error
    return 0
  else
    return 1
  endif
endfunction

function! NumberOfTmuxPanes()
  return system('number_of_tmux_panes')
endfunction

function! FilenameIncludes(pattern)
  return match(expand('%:p'), a:pattern) != -1
endfunction

function! ReadFileAsString(path)
  return join(readfile(a:path), "\n")
endfunction

function! InRailsApp(...)
  return filereadable("app/controllers/application_controller.rb")
endfunction

function! TestsInRails(filepath)
  return InRailsApp() && match(ReadFileAsString(a:filepath), 'spec_helper') != -1
endfunction

function! WithRspecFocusTag(filepath)
  return match(ReadFileAsString(a:filepath), 'focus: true') != -1
endfunction

function! WithCucumberFocusTag(filepath)
  return match(ReadFileAsString(a:filepath), '@focus') != -1
endfunction
if has("gui_running")
  set guifont=Ubuntu\ Mono\ derivative\ Powerline:h14
  set guioptions-=r
  set guioptions-=L
  set guioptions-=T
  set cursorline
  set nonumber
  set relativenumber
  colorscheme macvim
  set background=light
endif
