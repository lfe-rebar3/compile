lfe-compile
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {'lfe-compile', ".*", {git, "https://github.com/oubiwann/lfe-compile.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 lfe-compile
    ===> Fetching lfe-compile
    ===> Compiling lfe-compile
    <Plugin Output>
