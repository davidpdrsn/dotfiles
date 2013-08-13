def new_ruby_project_named name
  cmds = []
  cmds << "mkdir #{name}"
  cmds << "cd #{name}"
  cmds << "git init"
  cmds << "cp ~/dotfiles/gitignore .gitignore"
  cmds << "git add --all"
  cmds << "git commit -m 'initial commit'"
  cmds << "rspec --init"
  cmds << "mkdir lib"
  cmds << "mkdir spec/integration"
  cmds << "mkdir spec/unit"
  cmds << "mkdir spec/support"
  cmds << "git add --all"
  cmds << "git commit -m 'init rspec'"
  cmds << "touch Rakefile"
  cmds << "git add --all"
  cmds << "git commit -m 'add Rakefile'"
  cmds << "touch Rakefile"
  cmds << "git add --all"
  cmds << "git commit -m 'add empty Rakefile'"
  cmds << "touch Gemfile"
  cmds << "git add --all"
  cmds << "git commit -m 'add Gemfile'"
  cmds << "touch README.md"
  cmds << "git add --all"
  cmds << "git commit -m 'add readme'"

  system cmds.join("; ")

  File.open "#{name}/README.md", "w" do |f|
    f.puts "# #{name}"
  end
  system "cd #{name} && git add --all && git commit -m 'Update readme'"

  File.open "#{name}/Gemfile", "w" do |f|
    f.puts "source 'https://rubygems.org'"
    f.puts "\ngem 'rspec'"
  end
  puts "Running bundle install"
  system "cd #{name} && bundle install"
  system "cd #{name} && git add --all && git commit -m 'add gemfile'"

  `cd #{name} && bundle install`
  File.open "#{name}/Rakefile", "w" do |f|
    f.puts "task default: :test
task :test do
  system 'rspec spec'
end"
  end
  system "cd #{name} && git add --all && git commit -m 'add default rake task'"
end
