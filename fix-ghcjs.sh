#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash

# https://github.com/ghcjs/ghcjs/issues/556
sed -i '0,/function h$listProps(o)/! {0,/function h$listProps(o)/ s/function h$listProps(o)/function h$listProps_removed(o)/}' dist/build/jsmain/jsmain.jsexe/lib.js
