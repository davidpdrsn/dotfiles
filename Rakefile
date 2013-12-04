require 'fileutils'

USER = `whoami`.chomp

task :default => :update

desc 'Install dotfiles'
task :install => ['symlink', 'update', 'configure_osx'] do
  puts ''
  puts 'Installed!'
  puts 'From now on you can update the dotfiles with "rake -g dotfiles:update"'
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
    rspec
    emacs.d
    screenrc
  ).each do |file|
    system "rm -rf ~/.#{file}"
    system "ln -s ~/dotfiles/#{file} ~/.#{file}"
  end
end

desc "Update"
task :update do
  system 'cd ~/dotfiles'
  system 'git pull'
  unless File.exists? "/Users/#{USER}/dotfiles/vim/bundle/neobundle.vim"
    system "git clone git://github.com/Shougo/neobundle.vim ~/.vim/bundle/neobundle.vim"
  end
  system 'vim +NeoBundleInstall +NeoBundleUpdate +NeoBundleClean! +qall'
  system 'brew update && brew upgrade'
  system 'source ~/.zshrc'
end

desc "Run OS X configure script"
task :configure_osx do
  system 'sh ~/dotfiles/osx'
end
