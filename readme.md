These are my dotfiles.

# Installation
1. Switch to ZSH.

2. To install run the following commands from a terminal:

```shell
git clone https://github.com/davidpdrsn/dotfiles.git ~/dotfiles
sh ~/dotfiles/setup.sh
git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
vim +BundleInstall +qall
```

You may wanna restart your terminal after doing this

# Updating

```shell
sh ~/dotfiles/script/update.sh
```

# Bootstrapping a new Mac

This script will run a lot of bootstrapping stuff. It will:

- Switch to using zsh
- Make sure Xcode and gcc is installed
- Install Homebrew
- Install git and make sure its installed correctly
- Configure git to use my name and email (will be skipped if you are not me)
- Install command line apps I use
- Install MacVim
- Setup and install these dotfiles
- Run the OS X configure script
- Install RVM and ruby 2.0.0 (this one is a little rough)
- Install gems I use
- Install GUI apps I use

Run bootstrapping script with

```shell
curl https://raw.github.com/davidpdrsn/dotfiles/master/script/bootstrap.rb | ruby
```
