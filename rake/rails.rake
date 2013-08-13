namespace :rails do
  namespace :pow do
    desc 'Restart pow'
    task :restart do
      system "touch ~/.pow/restart.txt"
    end

    desc 'Setup pow symlink for the current folder'
    task :setup do
      system "cd ~/.pow && ln -s #{Dir.pwd} && cd - > /dev/null"
    end
  end

  desc 'run tests and generate code coverage review'
  task 'coverage' do
    system "COVERAGE=true bin/rake"
  end

  desc 'migrate development and test db'
  task :migrate do
    system "bin/rake db:migrate && bin/rake db:test:prepare"
  end
end
