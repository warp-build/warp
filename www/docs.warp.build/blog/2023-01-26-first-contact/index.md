---
slug: engage
title: Build Tooling is Stuck in the Past
authors: [leostera]
tags: [warp, devtools, intro, software-lifecycle]
---

It's been 46 years since the original release of Make, and the ways in which we
build, test, and ship software are fundamentally unchanged. 

You change some text files, find the right tool for the combination of
ecosystem and task you want to run, then you run your tool.

It checks for timestamps, maybe writes some intermediary files, and spits out a
report. You had an error. On a good day, it says OK! and you can push to
Github. Then your tool runs again, and does the same thing, again.

Really, the only folks who seemed to have their eye on the future were the
Smalltalk people. Their image based approach made enough semantic information
available to developer tooling to make them transcend the ways of Make.

### There's too many damn tools

We've followed the UNIX philosophy for better or worse, and we've come up with
thousands of tiny tools that we need to remember how to pipe and stitch and run
in the right order.

We use brew, nix, pacman, aptitutde, and choco to install system level
dependencies. Everything that is normally "below your application" is installed
with this. Sometimes globally, sometimes in sandboxes. Here we install Elixir,
and OpenJDK, and Python, and Postgres, and Kubernetes.

We use mix, pip, yarn, gradle, bundler, composer, and conan to install and manage version of our application-level dependencies. All the libraries and ecosystem specific tools are installed with this. This is where you get your web framework, your test runner, your Sendgrid integration library, your leftpad.

And then on top of this, we have to learn those ecosystem specific tools. You have to learn how Jest, Karma, JUnit, RSpec, Pytest, and NUnit are run: their flags, their guarantees, how to partition workloads, how to skip or focus specific tests.

I'm writing this on Docusaurus right now, and I had to learn how to configure and use the `docusaurus` tool. You will too once you have to write docs for your company.

When we're ready to deploy, we have to learn how to package or containerize our application with Docker, push it to some repository, and how to tell Kubernetes to update manifests.

And if this wasn't enough, we have to learn higher-level task runners that we use to orchestrate workflows across these tools: `yarn start`, `make deploy`, `./script/publish-blog.sh`.

Really, all we wanted to do was to answer a few questions across all of the code I have within a single company, regardless of the languages or platforms or versions of tools they need.

1. Build -- is the code I'm writing valid?
2. Test -- is the code I'm writing doing what I expect it to do?
3. Ship -- is the code I wrote the current live version?

### From Tasks to Goals

Just like we moved from Imperative to Declarative programming because it helped
us focus on _what_ we wanted to accomplish (which was way easier to think
about) than _how_ it was going to happen, I think its time we move up this
ladder in the software build cycle.

Right now, we are stuck in the Imperative land: run this command, move this file, check this exit status. The order in which I run the commands matters, so I have to be careful of installing Elixir with `brew` before running `mix test`.

I want to be in Declarative Land. I want to give my build tooling a goal like _"ensure the changes I
made are safe to ship"_ and let it figure out how to do this in the most efficient way possible.

I want to move from remembering and manually running a dozen or so tools on every change, to just invoking the exact goals I have:

* `warp build` -- figure out what has to be built, and build it as fast as it can, without redoing any work

* `warp test` -- figure out what code needs to be re-tested, and re-test it as as we can, without redoing any work

* `warp ship` -- figure out exactly what needs to be re-deployed, and follow a strategy to deploy it

And I want these commands to work for absolutely everything in my project. Website, web app, API, cli tools, documentation, mobile app.

I want the computer to do the work, so I can focus on shipping value for customers.

### But what about <FAANG TOOL\>?

I'd love it if you were right, but if you're thinking about Bazel, Pants, Buck, or other FAANG/MANGA tools, _think again_.

Those tools are amazing, but you've got to have a budget the size of Google's
to spend on a team to maintain and run the build system for the rest of the
organization.

For the rest of us, we're left with Makefiles and Bash scripts. While those
are perfectly fine as you start hacking on your product, they very quickly become hairballs that "everybody maintains". Just like your CI pipeline, this means that nobody maintains them.

### Introducing Warp, a build system from the future

I'm a trekkie, and like Zefram Cochrane's first Warp drive design, I expect the
right technological development to push us to an entire new age. For us
software developers at least, tho I don't see any Vulcan's greeting your team
anytime soon.

So I set out to build the equivalent of a warp drive for the whole software lifecycle, and I am calling it **warp**.

**Warp** is built on the shoulders of giants, but it makes some radically
different and novel decisions.

For starters, **warp** operates on the _entire software lifecycle graph_ at
once. This means that everything from low-level dependencies all the way up to
the definition of custom high-level goals is handled within the same tool. 

**warp** treats source code like one big semantic database, borrowing from the
Smalltalk line of software development. We have so much room to explore here
what it really means for a code database to be semantic, and what we can do
with it. Right now it allows us to answer with incredible granularity questions
like "should I run this test?" and "what is the impact of this refactor?".

It is built to help teams start fast and _continue_ fast while they grow. From a founding team of 3 people up to an org of 100, the principles that we're building with scale with your entire team.


### Join us!

It's been a crazy ride from dusting off the original prototype I had built years ago, to figuring out what really needed to be built, to rebuilding it.

But as we approach the end of Q1 2023, the team and I are absolutely thrilled to welcome y'all to our <a
href="http://waitlist.warp.build/">early-access waitlist</a>!

Expect a small email every now and then with some updates on how things are going :)

We are starting this journey to faster-than-light builds for the Erlang and Elixir ecosystem so we are more than happy to hear what you've got to say!

First Contact is near.

🖖





