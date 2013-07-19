require 'fileutils'

USER = `whoami`

task :default => :update

desc 'Install dotfiles'
task :install => ['symlink', 'update', 'configure_osx'] do
  puts 'Installed!'
  puts 'From now on you can update the dotfiles with "rake -g update"'
end

desc "Setup symlinks"
task :symlink do
  %w(
    vim
    vimrc
    gitconfig
    irbrc
    zshrc
    jasmine-headless-webkit
    jshintrc
    rake
    tmux.conf
    pryrc
  ).each do |file|
    system "rm -rf ~/.#{file}"
    system "ln -s ~/dotfiles/#{file} ~/.#{file}"
  end
end

desc "Update"
task :update do
  system 'cd ~/dotfiles'
  system 'git pull'
  unless File.exists? "/Users/#{`whoami`.chomp}/dotfiles/vim/bundle/vundle"
    system "git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle"
  end
  system 'vim +BundleInstall +BundleUpdate! +BundleClean! +qall'
  system 'source ~/.zshrc'
end

desc "Run OS X configure script"
task :configure_osx do
  system 'sh ~/dotfiles/osx'
end
