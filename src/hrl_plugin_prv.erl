-module(hrl_plugin_prv).
-behaviour(provider).
-export([init/1, do/1, format_error/1]).

-define(PROVIDER, echo).
-define(DEPS, [{default, compile}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Provider = providers:create([
    {name, ?PROVIDER},            % The 'user friendly' name of the task
    {module, ?MODULE},            % The module implementation of the task
    {namespace, hrl_plugin},
    {bare, false},                 % The task can be run by the user, always true
    {deps, ?DEPS},                % The list of dependencies
    {example, "rebar3 hrl_plugin"}, % How to use the plugin
    {opts, []},                   % list of options understood by the plugin
    {short_desc, "A rebar plugin"},
    {desc, "A rebar plugin"}
  ]),
  {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  rebar_api:info("Running hrl_plugin...", []),
  erlydtl:compile_file("/priv/templates/test.dtl",test_dtl),
  Option = [{tableName,<<"test">>},{fields,[<<"a">>,<<"b">>,<<"c">>]}],
  {ok,Result} = test_dtl:render(Option),
  file:write_file("src/include/test.hrl",Result),
  rebar_api:info("Running hrl_plugin...end", []),
  {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).
