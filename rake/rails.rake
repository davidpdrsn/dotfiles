desc 'Setup pow symlink for the current folder'
task "pow" do
  pwd = Dir.pwd
  system "cd ~/.pow && ln -s #{pwd} && cd - > /dev/null"
end
