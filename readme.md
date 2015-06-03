dotfiles
========

Installation
------------

```shell
curl https://raw2.github.com/davidpdrsn/dotfiles/master/script/bootstrap | sh
```

You may wanna restart your terminal after doing this

Things you should steal
-----------------------

These are things that I think are cool. You may wanna steal these for your own dotfiles.

### Shell

- `reload` will source the zsh config.
- All my git aliases. I can't use git without these.
- `l` alias for seeing a more sane listing of files.
- `extract` which will extract any tar ball/zip file/whatever archive.
- `server` for firing up a quick HTTP server.
- A bunch of useful git functions I've stolen from various people.

### Vim

- A rather long but structured vimrc.
- Clear overview of which leader mappings mapped and which aren't.
- Loads of awesome plugins.
- Almost no overriding of default Vim behavior.
- Vim mappings that call out to well named Vim functions. I like this approach rather than writing comments above everything.
- Highlighting of longs lines except in files where it doesn't make sense (like markdown).
- Big [vim-spectacular](https://github.com/davidpdrsn/vim-spectacular) setup for runnings tests in a whole bunch of languages.
- Ruby snippet for inserting class definition based on the location and name of the current file. Really useful in Rails apps.
- Insert mode mapping for fixing spelling mistakes.
- `<leader>as` and `<leader>av` to open/create tests for current file in Rails.
- `<leader>ml` to make markdown link for first Google search result.
- `<leader>l` to make numbered list.

### Scripts

- `calc` which takes an expression on stdin and evaluates and prints the result. Awesome for quick calculations.
- `retag` for rebuilding ctags file with common folders excluded.
- `bootstrap` which sets everything up. Nice when putting the dotfiles on a new machine.
- `search <word>` which returns link to first result of Google search.
- `markdown_link_for <word>` which makes markdown link for first result of Google search.
