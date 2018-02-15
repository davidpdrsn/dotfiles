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
  if match(a:filename, "_" . a:spec_identifier) != -1
    let folder_name = substitute(a:filename, a:spec_identifier . "/", "app/", "")
    return substitute(folder_name, "_" . a:spec_identifier . ".rb", ".rb", "")
  else
    let folder_name = substitute(a:filename, "app", a:spec_identifier, "")
    return substitute(folder_name, ".rb", "_" . a:spec_identifier . ".rb", "")
  endif
endfunction

function! rails_test#vsplit_spec(spec_identifier)
  let path = s:spec_file_path(s:path_to_current_file(), a:spec_identifier)
  call s:splitv(path)
endfunction

function! rails_test#hsplit_spec(spec_identifier)
  let path = s:spec_file_path(s:path_to_current_file(), a:spec_identifier)
  call s:splith(path)
endfunction
