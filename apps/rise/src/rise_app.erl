-module(rise_app).
-behaviour(application).
-export([start/2, stop/1]).
-include_lib("bitmessage/include/bm.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:load(mnesia),
    application:load(sasl),
    application:stop(lager),
    RiseDir = case os:type() of
        {win32, _} ->
            os:getenv("APPDATA") ++ "/RISE";
        {unix, linux} ->
                      os:getenv("HOME") ++ "/.config/RISE";
        _ ->
            os:getenv("HOME") ++ "/Library/RISE"
    end,
    application:set_env(rise, workdir, RiseDir),
    file:make_dir(RiseDir),
    file:make_dir(RiseDir ++ "/data"),
    file:make_dir(RiseDir ++ "/scratch"),
    file:make_dir(RiseDir ++ "/log"),
    file:make_dir(RiseDir ++ "/log/sasl"),
    application:stop(mnesia),
    application:set_env(mnesia, dir, RiseDir ++ "/data"),
    application:set_env(simple_bridge, scratch_dir, RiseDir ++ "/scratch"),
    application:set_env(sasl, sasl_error_logger, {file, RiseDir ++ "/log/sasl/sasl-error.log"}),
    application:set_env(lager, handlers,[
                                         {lager_file_backend,
                                          [{file, RiseDir ++ "/log/error.log"},
                                           {level, debug},
                                           {size, 10485760},
                                           {date, "$D0"},
                                           {count, 5}
                                           ]},
                                         {lager_file_backend,
                                          [{file, RiseDir ++ "/log/console.log"},
                                           {level, debug},
                                           {size, 10485760},
                                           {date, "$D0"},
                                           {count, 5}
                                           ]},
                                         {lager_file_backend,
                                          [{file, RiseDir ++ "/log/debug.log"},
                                           {level, debug},
                                           {size, 10485760},
                                           {date, "$D0"},
                                           {count, 5}
                                           ]}
                                        ]),
    application:set_env(lager, crash_log, RiseDir ++ "/log/crash.log"),
    application:start(mnesia),
    application:ensure_all_started(lager),
    application:start(bitmessage),
    % application:start(eminer),
    rise_sup:start_link().

stop(_State) ->
    ok.
