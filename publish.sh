#! /bin/bash
set -eu

cabal configure --ghcjs
cabal build jsmain
./fix-ghcjs.sh

cp dist/build/jsmain/jsmain.jsexe/* www/
