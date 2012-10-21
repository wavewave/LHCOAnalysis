#!/bin/bash 

sudo apt-get install libgsl0-dev

cabal install hmatrix 

ghc-pkg list
mkdir deps
git clone https://github.com/wavewave/devadmin.git deps/devadmin
cd deps/devadmin ; cabal install --force-reinstalls ; cd ../../
$HOME/.cabal/bin/build cloneall --config=build.conf
$HOME/.cabal/bin/build bootstrap --config=build.conf
cabal install --enable-tests

