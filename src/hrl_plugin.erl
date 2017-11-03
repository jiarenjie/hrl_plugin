%%%-------------------------------------------------------------------
%%% @author jiarj
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. 十一月 2017 10:39
%%%-------------------------------------------------------------------
-module(hrl_plugin).
-author("jiarj").

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [{default, compile}]).


%% ===================================================================
%% Public API
%% ===================================================================

%% Called when rebar3 first boots, before even parsing the arguments
%% or commands to be run. Purely initiates the provider, and nothing
%% else should be done here.
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->

  Options=[{name, ?PROVIDER},
    {module, ?MODULE},
    {namespace, hrl_plugin},
    {bare, false},
    {deps, ?DEPS},
    {example, "rebar3 erlydtl compile"},
    {short_desc, "Compile erlydtl templates."},
    {desc, "Compile erlydtl templates."},
    {opts, []}],

  Provider = providers:create(Options),
  {ok, rebar_state:add_provider(State, Provider)}.

%% Run the code for the plugin. The command line argument are parsed
%% and dependencies have been run.
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  rebar_api:info("hrl_plugin run", []),
  {ok, State}.

%% When an exception is raised or a value returned as
%% `{error, {?MODULE, Reason}}` will see the `format_error(Reason)`
%% function called for them, so a string can be formatted explaining
%% the issue.
-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).
