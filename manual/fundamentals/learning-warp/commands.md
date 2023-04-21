# Commands 

## Setup
The `warp setup` command prepares your system for using warp and will only need to be run once per machine. Running the command creates a separate **warp folder** which ensures warp stuff is kept in a separate bubble. 

Running the command is idempotent and if the machine is already warp-ready no working work is done.   

## Build
The `build` command takes the a path to a directory with a warp file, followed by a colon and the name of the target to be built. 

```console
$ warp build ./my/library/mix.exs 
Finished ./my/library/mix.exs in 6ms (0 built, 1 cached)
```

Use @all to build the entire project.

## Test
The `test` command helps you do incremental testing with incredible granularity, down to individual tests in a file.

You can therefore specify a specific file or folder (target) to be tested, and even specify a test or group of tests within the target.

Running a test suite.
```console
$ warp test ./test/test_SUITE.erl
     Started https://store.warp.build/tricorder/beam/manifest.json
    Readying https://store.warp.build/tricorder/beam/manifest.json
       Ready https://store.warp.build/tricorder/beam/manifest.json
   Cache-hit https://rules.warp.build/toolchains/openssl  
   Cache-hit https://rules.warp.build/toolchains/erlang  
   Cache-hit src/test_parser.erl  
   Cache-hit src/test.erl  
        PASS ./test/test_SUITE.erl parse_test (CACHED)
        PASS ./test/test_SUITE.erl compare_test (CACHED)
        PASS ./test/test_SUITE.erl between_test (CACHED)
        PASS ./test/test_SUITE.erl compile_requirement_test (CACHED)
        PASS ./test/test_SUITE.erl ./test/test_SUITE.erl (CACHED)
        PASS ./test/test_SUITE.erl lte_test (CACHED)
        PASS ./test/test_SUITE.erl lt_test (CACHED)
        PASS ./test/test_SUITE.erl gt_test (CACHED)
        PASS ./test/test_SUITE.erl is_match_test (CACHED)
        PASS ./test/test_SUITE.erl parse_requirement_test (CACHED)
        PASS ./test/test_SUITE.erl eq_test (CACHED)
        PASS ./test/test_SUITE.erl gte_test (CACHED)
    Finished ./test/test_SUITE.erl in 865ms (11 passed, 0 errors, 5 built, 16 cached)
```

Running a specific test in a suite.
```console
$ warp test ./test/test_SUITE.erl parse_test
     Started https://store.warp.build/tricorder/beam/manifest.json
    Readying https://store.warp.build/tricorder/beam/manifest.json
       Ready https://store.warp.build/tricorder/beam/manifest.json
   Cache-hit https://rules.warp.build/toolchains/openssl  
   Cache-hit https://rules.warp.build/toolchains/erlang  
   Cache-hit src/test_parser.erl  
   Cache-hit src/test.erl  
        PASS ./test/test_SUITE.erl parse_test (CACHED)
    Finished ./test/test_SUITE.erl in 60ms (1 passed, 0 errors, 5 built, 1 cached)
```

## Run
The `run <runnable-target>` executes a runnable target. It takes a path to a directory with a warp file, followed by the name of a target to be run. If run within a warp-ready directory, you can also use the alias notation `@`. 


```console
$ warp run @python
Python 3.11.2 (main, Mar 24 2023, 00:16:47) [Clang 14.0.0 (clang-1400.0.29.202)] on darwin
Type "help", "copyright", "credits" or "license" for more information.
>>> 
```

{% hint style="note" %}
**Not all targets are runnable.** Non-runnable targets will build their dependencies and exit.
{% endhint %}