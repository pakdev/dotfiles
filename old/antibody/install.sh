#!/bin/sh
if [ ! -f /usr/local/bin/antibody ]; then 
	curl -sL https://git.io/antibody | sh -s
fi
antibody bundle < "$DOTFILES/antibody/bundles.txt" >~/.zsh_plugins.sh
antibody update
