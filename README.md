lfe-compile
=====

*The LFE rebar3 compiler plugin*

<img src="resources/images/logo.png" />


Build
-----

```bash
    $ rebar3 compile
```


Use
---

Add the plugin to your ``rebar.config``:

```erlang
{plugins, [
  {'lfe-compile',
    {git, "https://github.com/oubiwann/rebar3-lfe-compile.git"}}
]}.
```

Then just call your plugin directly in an existing application:


```bash
$ rebar3 help lfe compile
...
```

```bash
$ rebar3 lfe compile
...
```
