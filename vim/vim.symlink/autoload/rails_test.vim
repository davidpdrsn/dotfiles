function! s:path_to_current_file()
  return expand("%")
endfunction

function! s:splitv(file_name)
  execute "vsplit " . a:file_name
endfunction

function! s:splith(file_name)
  execute "split " . a:file_name
endfunction

function! s:spec_file_path(filename)
  let folder_name = substitute(a:filename, "app", "spec", "")
  let folder_name = substitute(folder_name, "lib", "spec/lib", "")
  return substitute(folder_name, ".rb", "_spec.rb", "")
endfunction

function! rails_test#vsplit_spec()
  let path = s:spec_file_path(s:path_to_current_file())
  call s:splitv(path)
endfunction

function! rails_test#hsplit_spec()
  let path = s:spec_file_path(s:path_to_current_file())
  call s:splith(path)
endfunction
