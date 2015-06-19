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

Add the plugin to your rebar config:

```erlang
    {plugins, [
    {'lfe-compile', ".*",
      {git, "https://github.com/oubiwann/lfe-compile.git", {tag, "0.1.0"}}}
    ]}.
```
    
Then just call your plugin directly in an existing application:

```bash
    $ rebar3 lfe compile
    ===> Fetching lfe-compile
    ===> Compiling lfe-compile
    <Plugin Output>
```
