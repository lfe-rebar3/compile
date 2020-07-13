# DEPRECATED / ARCHIVED

This functionality is now available in the far more streamlined project https://github.com/lfe-rebar3/rebar3_lfe

# lfe-compile

[![Build Status][travis badge]][travis] [![LFE Versions][lfe badge]][lfe] [![Erlang Versions][erlang badge]][versions] [![Tags][github tags badge]][github tags] [![Downloads][hex downloads]][hex package]

*The LFE rebar3 compiler plugin*

[![][lr3-logo]][lr3-logo]


#### Contents

* [About](#about-)
* [Build](#build-)
* [Use](#use-)


## About [&#x219F;](#contents)

Of all the [rebar3 plugins for LFE][org], this one is the most important (it's
also the only one not written in LFE itself). All other [LFE plugins][org]
depend upon this one.

Note that the [LFE rebar3 plugins][org] are not intended to be used as projects
or tools in their own right -- they need to be incorporated into another
project. You can add any [LFE rebar3 plugin][org] to your project, of course,
but the intent is for a new tool to wrap all of them. This tool is simply called
[ltool]. The hope is that it will replace all the functionality that currently
is built into [lfetool].

If you would like to use this plugin in your own project, without a wrapping
tool, see the ["Use"](#use-) section below.


## Build [&#x219F;](#contents)

```bash
$ rebar3 compile
```

## Use [&#x219F;](#contents)

Add the plugin to your ``rebar.config``:

Using [GitHub][github]:

```erlang
{plugins, [
   {'lfe-compile',
    {git, "git://github.com/lfe-rebar3/compile.git",
      {tag, "0.7.0"}}}
  ]}.
```

Using [Gitlab][gitlab]:

```erlang
{plugins, [
   {'lfe-compile',
    {git, "git://gitlab.com/lfe-rebar3/compile.git",
      {tag, "0.7.0"}}}
  ]}.
```

Using [Hex][hex package]:

```erlang
{plugins, [
   {'lfe-compile', "0.7.0", {pkg, rebar3_lfe_compile}}
  ]}.
```

Then let ``rebar3`` know that you want to call ``lfe compile`` after the
rebar3 ``compile`` task. Do this by adding the following provider hook in
your ``rebar.config`` file:

```erlang
{provider_hooks, [
   {post, [{compile, {lfe, compile}}]}
  ]}.
```

Then just compile from your project directory:

```bash
$ rebar3 compile
...
```

This will first download and build all your project dependencies, then compile
all Erlang-related files your project may have, and finally it will compile the
``.lfe`` files in your project.

If you would just like to to compile the ``.lfe`` files, you can use the
following command:

```bash
$ rebar3 lfe compile
...
```

For a more detailed description of how to use the [rebar3 LFE plugins][org],
refer to this [blog post]. Note that the version numbers in the post are
outdated now, but the general ideas are still valid.

<!-- Named page links below: /-->

[lr3-logo]: priv/images/logo.png
[org]: https://github.com/lfe-rebar3
[github]: https://github.com/lfe-rebar3/compile
[gitlab]: https://gitlab.com/lfe-rebar3/compile
[travis]: https://travis-ci.org/lfe-rebar3/compile
[travis badge]: https://img.shields.io/travis/lfe-rebar3/compile.svg
[lfe]: https://github.com/rvirding/lfe
[lfe badge]: https://img.shields.io/badge/lfe-1.3.0-blue.svg
[erlang badge]: https://img.shields.io/badge/erlang-17.5%20to%2022.0-blue.svg
[versions]: https://github.com/lfe-rebar3/compile/blob/master/.travis.yml
[github tags]: https://github.com/lfe-rebar3/compile/tags
[github tags badge]: https://img.shields.io/github/tag/lfe-rebar3/compile.svg
[github downloads]: https://img.shields.io/github/downloads/atom/atom/total.svg
[hex badge]: https://img.shields.io/hexpm/v/rebar3_lfe_compile.svg?maxAge=2592000
[hex package]: https://hex.pm/packages/rebar3_lfe_compile
[hex downloads]: https://img.shields.io/hexpm/dt/rebar3_lfe_compile.svg
[ltool]: https://github.com/lfe-rebar3/ltool
[blog post]: http://blog.lfe.io/tutorials/2016/03/25/0858-lfe-and-rebar3/
[lfetool]: https://github.com/lfex/lfetool
