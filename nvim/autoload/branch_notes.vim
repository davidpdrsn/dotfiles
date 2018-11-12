function! s:current_git_branch()
  return system("git current-branch")
endfunction

function! branch_notes#open()
  let path = "tmp/pr/" . s:current_git_branch() . ".md"
  execute "topleft sp " . path
endfunction
