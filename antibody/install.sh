#!/bin/sh
curl -sL https://git.io/antibody | sh -s
antibody bundle <"$DOTFILES/antibody/bundles.txt" >~/.zsh_plugins.sh
antibody update
