# Contribution Guide

## Bootstrapping

The Warp tool relies on a series of smaller tools called Tricorders to make
sense of ecosystems and derive build signatures. Those tricorders should be
buildable _without_ needing themselves to have been built yet, so Warp provides
a specific command that is only useful to do this: `warp bootstrap`.

Before `warp bootstrap` is ran, make sure to go over the different tricorders
and run the accompanying `bootstrap.sh` scripts. These scripts may require some
globally installed dependencies (like `protoc`, or a running Elixir
installation).
