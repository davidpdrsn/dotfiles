require_relative 'helpers'

namespace :new do
  desc 'Setup a new ruby project'
  task :ruby do
    print 'Name of project: '
    name = STDIN.gets.chomp
    new_ruby_project_named name
  end

  desc 'New rails app'
  task :rails do
    print 'Name of app: '
    name = STDIN.gets.chomp
    system "gem update rails"
    system "rails new #{name}"
  end
end
