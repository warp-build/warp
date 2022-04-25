# Sun Apr 24 11:20:11 CEST 2022

Catching up with what's written and moving stuff around so I can keep going
without losing it.

The main thing that happens is `structopt` kicking in in `bin/main.rs`'s Zap
struct, where we select a _goal_. A goal here can be to build something, test
something, etc.

We're gonna dig through the Build goal.

This runs `bin/goals/build.rs@BuildGoal::run`, which:

0. Parsers the goal target into a Label
1. Creates a new `ZapWorker`
2. Loads the current path
  1. the worker will `scan` the root directory
  2. we'll configure the BuildScript context
  3. load the default toolchains and rules
  4. load the local toolchains and rules
3. Builds the dependency graph
  1. the worker asks the workspace to collect all targets
  2. extract all the targets from the toolchains
  3. build a `DepGraph` from this list of targets
4. Creates a new `BuildRunner` from the worker
  1. Build a new local cache 
5. Executes the build runner using the goal target as Label
  1. scope the build graph
  2. create a topological walk over the graph, and for every node:
    1. skip it if it's already in the cache
    2. create a new Sandbox
    2. run the Sandbox
    3. validate the outputs


Definitely some stuff to keep separate is:

1. Dependency Graph -- given a list of Targets, compute a dependency graph
   between them, provide a topological sort over them, provide scoping down the
   whole graph.

2. Content-Addressable Cache -- given a Target, check if it exists in the cache,
   else populate the cache with this new Target.

2. Sandboxed Runner -- given a Target and a Cache, execute this target in
   complete isolation.
