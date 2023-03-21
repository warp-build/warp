---
sidebar_position: 1
---

# Installation

warp works on macOS and Linux. warp is a single binary executable. It
has no external dependencies.

On macOS and Linux, both M1 (arm64) and Intel (x64) executables are provided.

## Download and install

warp binaries can only be installed manually, by downloading a zip file at
[github.com/warp-build/warp/releases](https://github.com/warp-build/warp/releases).
These packages contain just a single executable file. You will have to set the
executable bit on macOS and Linux.

## Testing your installation

To test your installation, run `warp --version`. If this prints the warp version
to the console the installation was successful.

Use `warp help` to see help text documenting warp's flags and usage. 

## Setup your environment

Once you have tested that `warp` works, you must run `warp setup` to let warp
prepare your machine.

On macOS this currently requires `sudo`, since we will set up a custom volume mounted on the root directory (`/warp`).

## Building from source

Information about how to build from source can be found in the
[`Contributing`](../references/contributing/build-from-source.md) chapter.
