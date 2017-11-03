hrl_plugin
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { hrl_plugin, ".*", {git, "git@host:user/hrl_plugin.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 hrl_plugin
    ===> Fetching hrl_plugin
    ===> Compiling hrl_plugin
    <Plugin Output>
