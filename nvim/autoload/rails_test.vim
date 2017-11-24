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
  if match(a:filename, "_spec") != -1
    let folder_name = substitute(a:filename, "spec/", "app/", "")
    return substitute(folder_name, "_spec.rb", ".rb", "")
  else
    let folder_name = substitute(a:filename, "app", "spec", "")
    return substitute(folder_name, ".rb", "_spec.rb", "")
  endif
endfunction

function! rails_test#run_spec()
  let path = s:spec_file_path(s:path_to_current_file())
  execute "Dispatch rspec " . path
endfunction

function! rails_test#vsplit_spec()
  let path = s:spec_file_path(s:path_to_current_file())
  call s:splitv(path)
endfunction

function! rails_test#hsplit_spec()
  let path = s:spec_file_path(s:path_to_current_file())
  call s:splith(path)
endfunction

nnoremap <leader>as :call rails_test#hsplit_spec()<cr>
nnoremap <leader>av :call rails_test#vsplit_spec()<cr>
