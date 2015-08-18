%% vim: ts=4 sw=4 et ft=erlang
-module(rise_sup).
-behaviour(supervisor).
-export([
    start_link/0,
    init/1
]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->  % {{{1
    application:start(nprocreg),
    application:start(simple_bridge),
    {ok, { {one_for_one, 5, 10}, [{receiver, {receiver, start_link, []}, permanent, 2, worker, dynamic}]}}. 

