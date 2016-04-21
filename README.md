# lfe-compile

*The LFE rebar3 compiler plugin*

[lr3-logo]: resources/images/logo.png

[![][lr3-logo]][lr3-logo]


#### Contents

* [About](#about-)
* [Build](#build-)
* [Use](#use-)


## About [&#x219F;](#contents)

Of all the rebar3 plugins for LFE, this one is the most important (it's also
the only one not written in LFE itself). All other LFE plugins depend upon this
one.

Note that the LFE rebar3 plugins are not intended to be used as projects or
tools in their own right -- they need to be incorporated into another project.
You can add any LFE rebar3 plugin to your project, of course, but the intent is
for a new tool to wrap all of them. This tool is simply called [ltool][]. The
hope is that it will replace all the functionality that currently is built into
[lfetool][].

If you would like to use this plugin in your own project, without a wrapping
tool, see the [Use](#use-) section below.

[ltool]: https://github.com/lfe-rebar3/ltool
[lfetool]: https://github.com/lfex/lfetool

## Build [&#x219F;](#contents)

```bash
$ rebar3 compile
```


## Use [&#x219F;](#contents)

Add the plugin to your ``rebar.config``:

```erlang
{plugins, [
  rebar3_lfe_compile
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

If you would just like to compile the ``.lfe`` files, you can use the
following command:

```bash
$ rebar3 lfe compile
...
```
