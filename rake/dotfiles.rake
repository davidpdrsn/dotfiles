namespace :dotfiles do
  desc 'Update dotfiles'
  task :update do
    system "cd ~/dotfiles && rake update"
  end
end
