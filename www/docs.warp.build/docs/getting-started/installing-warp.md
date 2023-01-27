---
sidebar_position: 1
---

# Installing Warp

Once you are let into the Early-Access Program you will get access
to the Warp binaries for your architecture over our Discord
`#releases` channel.

When you download one of the release tarballs / zipfiles, you'll find there's only a single binary called `warp` within them. Place this binary somewhere in your `PATH` and you'll be good to go!

Once you've done this, you can run `warp setup` to make sure your machine is configured correctly to use Warp.

### Special considerations for macOS

When you run `warp setup` on a Mac, you'll be asked to enter your `sudo` password. This is because to make builds completely relocatable on Mac, we need to do a few things to create a root-level folder in your file system.

This is harmless, and it will leave you with a new virtual disk listed in your Disk Utility program called `warp Store`.

After `warp setup` is complete, you should be able to see a folder called `/warp` from within your terminal. Everything related to `warp` will be installed, downloaded, and saved there.
