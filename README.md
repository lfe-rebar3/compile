# lfe-compile

[![Hex.pm][hex badge]][hex package]

[hex badge]: https://img.shields.io/hexpm/v/rebar3_lfe_compile.svg?maxAge=2592000
[hex package]: https://hex.pm/packages/rebar3_lfe_compile

*The LFE rebar3 compiler plugin*

[lr3-logo]: resources/images/logo.png

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

[org]: https://github.com/lfe-rebar3
[ltool]: https://github.com/lfe-rebar3/ltool
[lfetool]: https://github.com/lfex/lfetool

## Build [&#x219F;](#contents)

```bash
$ rebar3 compile
```


## Use [&#x219F;](#contents)

Add the plugin to your ``rebar.config``:

Via ``git``:

```erlang
{plugins, [
   {'lfe-compile',
    {git, "git://github.com/lfe-rebar3/compile.git",
     {tag, "0.6.0"}}}
  ]}.
```

Via [Hex][hex package]:

```erlang
{plugins, [
   {'lfe-compile', "0.6.0", {pkg, rebar3_lfe_compile}}
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
