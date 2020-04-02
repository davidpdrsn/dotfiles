function! s:path_to_current_file()
  return expand("%")
endfunction

function! s:splitv(file_name)
  execute "vsplit " . a:file_name
endfunction

function! s:splith(file_name)
  execute "split " . a:file_name
endfunction

function! s:spec_file_path(filename, spec_identifier)
  if match(a:filename, a:spec_identifier . ".scala") != -1
    " File is a test
    let folder_name = substitute(a:filename, "test", "main", "")
    let path = substitute(folder_name, a:spec_identifier . ".scala", ".scala", "")
    return path
  else
    " File is _not_ a test
    let folder_name = substitute(a:filename, "main", "test", "")
    let path = substitute(folder_name, "\\.scala", a:spec_identifier . ".scala", "")
    return path
  endif
endfunction

function! scala_test#vsplit_spec()
  let path = s:spec_file_path(s:path_to_current_file(), "Spec")
  call s:splitv(path)
endfunction

function! scala_test#hsplit_spec()
  let path = s:spec_file_path(s:path_to_current_file(), "Spec")
  call s:splith(path)
endfunction
