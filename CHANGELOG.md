# Fri Jul  1 01:04:00 CEST 2022

We've set up https://api.warp.build and we've configured the cli to talk to it.
First global cached builds happened!

# Thu Jun 30 10:16:36 CEST 2022

We've added support for aliases, so now you can run `warp build @api`, which is
more convenient than long target names for deeply nested workspaces.

# Wed Jun 29 16:41:39 CEST 2022

It's been a while but today we finally got the remote cache building nicely.
We are also renaming the project from Zap, to Warp.

This is a homage to Star Trek's primary method for faster-than-light travel,
where ships bend the space-time continuum _around_ them to move faster. This is
exactly what Warp does to get your builds to be instant.

Warp doesn't speed up your compiler, it bends space-time _around it_ so
compilation goes faster.

# Mon May  2 00:36:33 CEST 2022

A lot has happened since last Monday, so I figured I'd recap the week here. For
the most part, I'm dodging burnout by resting a lot, reading, and doing shit
that gives me energy. Like Warp. Warp is cool.

In the past few days we've added support for:

* getting `info` out of targets
* `clean`ing targets
* `run`ning interactive targets that fully pipe stdin/out (like repls)

We've added also support for _a bunch_ of build rules and toolchains:

* You can now build libraries for the following build systems:
  * ReScript using BuckleScript Build (the official but hidden build system)
  * Erlang.mk 
  * Mix
  * Gleam Build

There's also been several improvements to the hashing used for caching and the
access to transitive dependencies in the build graph. These helped build the
rules above.

But of course none of those rules are worth a damn if we can't build anything
with them, so I'm proud to announce we managed to run an Interactive Elixir
Shell with an entire application tree loaded, and started up automatically. It
was wild!

Next up, I'll be building the actual API used to receive Github Webhooks. Not
sure what that's gonna look like, but for the time being it'll just be a little
Elixir+Plug.

Sometime this week Warp Cloud will be building itself with Warp Cloud.

# Mon Apr 25 01:18:42 CEST 2022

Managed to get the thing up and running.

Moved the Erlang and Elixir rules and toolchains into this repo, so I can
evolve them separately from the binary itself.

Time to start building the app itself! Next steps:
https://linear.app/abstractmachines/issue/ZAP-3/set-up-tiny-elixir-api


# Sun Apr 24 11:16:52 CEST 2022

The flow should go a little like this:

1. `warp lift` for an existing project will try to identify the parts of the
   project that need to be lifted, and the tools that they are using.

   It creates a `Workspace.toml` with the detected toolchains, and the
   appropriate `Build.toml` files.

   In the average scenario, this should be enough to do a build.


# Old Notes
───────┬────────────────────────────────────────────────────────────────────────────────────
       │ File: /Users/ostera/warp.stuff
───────┼────────────────────────────────────────────────────────────────────────────────────
   1   │ why?
   2   │ * nobody wants to work on ci pipelines
   3   │ * everyone wants fast ci pipelines
   4   │ * time spent waiting on CI is just money down the drain
   5   │ 
   6   │ management
   7   │ - github integration
   8   │ 
   9   │ org
  10   │ - projects
  11   │   - commits
  12   │     - artifacts
  13   │ 
  14   │ 0. warp.build
  15   │ 1. login with github
  16   │ 2. when you open a PR, you see a status update:
  17   │     * code checks
  18   │     * build
  19   │     * tests (small, medium, large)
  20   │ 
  21   │ $ warp lift -- mix.exs
  22   │ **Build.toml
  23   │ Workspace.toml
  24   │ 
  25   │ $ warp login
  26   │ <open github>
  27   │ 
  28   │ $ warp build
  29   │ <reuse the build cache>
  30   │ 
  31   │ get the build system to be prod ready
