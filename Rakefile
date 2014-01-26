require 'fileutils'

USER = `whoami`.chomp

desc 'Install dotfiles'
task :install => ['symlink', 'update', 'configure_osx'] do
  puts ''
  puts 'Installed!'
  puts 'From now on you can update the dotfiles with "rake -g dotfiles:update"'
end

desc "Update"
task :update do
  system 'cd ~/dotfiles'
  system 'git pull'
  unless File.exists? "/Users/#{USER}/dotfiles/symlinkables/vim/bundle/vundle"
    system "git clone https://github.com/gmarik/vundle.git ~/.Vim/bundle/vundle"
  end
  system 'vim +BundleInstall +BundleUpdate +BundleClean! +qall'
  system 'source ~/.zshrc'
end

desc "Run OS X configure script"
task :configure_osx do
  system 'sh ~/dotfiles/script/configure_osx.sh'
end
