#/usr/bin/env bash

fd ".*\.hs" --exec ormolu --mode=inplace
cabal-fmt --inplace pastebin.cabal
