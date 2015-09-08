-module(sugar).
-compile([export_all]).
-include_lib("bitmessage/include/bm.hrl").
-include("protokol.hrl").

ttl_to_readable(TTL) ->  % {{{1
    date_format(calendar:universal_time_to_local_time(ttl_to_datetime(TTL))).

ttl_to_timestamp(undefined) ->  % {{{1
    bm_types:timestamp();
ttl_to_timestamp(TTL) when is_tuple(TTL) ->  % {{{1
    ttl_to_timestamp(datetime_to_timestamp(TTL));
ttl_to_timestamp(TTL) ->  % {{{1
     TTL - application:get_env(bitmessage, message_ttl, 2419200).

ttl_to_datetime(TTL) ->  % {{{1
    TS = ttl_to_timestamp(TTL),
    timestamp_to_datetime(TS).

ttl_from_string(Str) ->  % {{{1
    TS = timestamp_from_string(Str),
    TS + application:get_env(bitmessage, message_ttl, 2419200).

timestamp_to_ttl(undefined) ->  % {{{1
    timestamp_to_ttl(bm_types:timestamp());
timestamp_to_ttl(Timstamp) ->  % {{{1
     Timstamp + application:get_env(bitmessage, message_ttl, 2419200).

time_string(DT) ->  % {{{1
    O = date_format(DT),
    case string:tokens(O, " ") of
        [_D, T] ->
            T;
        _ ->
            "9:00:00"
    end.

date_string(DT) ->  % {{{1
    O = date_format(DT),
    case string:tokens(O, " ") of
        [D, _T] ->
            D;
        _ ->
            O
    end.

date_format(Str) when is_list(Str)->  %{{{1
    Str;
date_format({{Y, M, D}, {H, Mi, S}}) ->  %{{{1
	%% might be worth replacing with qdate:to_string()
   	lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", [Y, M, D, H, Mi, S]));
date_format({_Y, _M, _DD}=D) ->  %{{{1
    date_format({D, {9, 0, 0}});
date_format(Any) ->  % {{{1
    date_format(calendar:local_time()).

date_from_string("") ->  % {{{1
    "";
date_from_string(Str) when is_list(Str) ->  % {{{1
    case string:tokens(Str, " ") of
        [Date] ->
            [Y, M, D] = string:tokens(Date, "-"),
            {H, Mi, S} = {9, 0, 0},
            {{wf:to_integer(Y), wf:to_integer(M), wf:to_integer(D)},
             {wf:to_integer(H), wf:to_integer(Mi), wf:to_integer(S)}};
        [Date, Time] ->
            [Y, M, D] = string:tokens(Date, "-"),
            case string:tokens(Time, ":") of
               [H, Mi, S] -> 
                    {{wf:to_integer(Y), wf:to_integer(M), wf:to_integer(D)},
                     {wf:to_integer(H), wf:to_integer(Mi), wf:to_integer(S)}};
               [H, Mi] -> 
                    {{wf:to_integer(Y), wf:to_integer(M), wf:to_integer(D)},
                     {wf:to_integer(H), wf:to_integer(Mi), 0}};
               [H] -> 
                    {{wf:to_integer(Y), wf:to_integer(M), wf:to_integer(D)},
                     {wf:to_integer(H), 0, 0}}
            end;
        _ ->
            Str
    end;
date_from_string(Any) ->  % {{{1
    Any.


datetime_to_timestamp(undefined) ->  %{{{1
    bm_types:timestamp();
datetime_to_timestamp(DateTime) when is_integer(DateTime) ->  %{{{1
    DateTime;
datetime_to_timestamp(DateTime) ->  %{{{1
    calendar:datetime_to_gregorian_seconds(DateTime) -
    calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).

timestamp_to_datetime(TS) when is_integer(TS) -> % {{{1
    calendar:gregorian_seconds_to_datetime(TS +
    calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}));
timestamp_to_datetime(TS) -> % {{{1
    TS.

timestamp_from_string(Str) ->  % {{{1
    datetime_to_timestamp(date_from_string(Str)).
format_file_size(S) when S > 1000 * 1000 * 1000 ->  %{{{1
    wf:f("~.2fG", [ S  / bm_types:pow(1024, 3)]);
format_file_size(S) when S >  1000 * 1000 ->  %{{{1
    wf:f("~.2fM", [S / bm_types:pow(1024, 2)]);
format_file_size(S) when S > 1000 ->  %{{{1
    wf:f("~.2fK", [S / 1024]);
format_file_size(S) ->  %{{{1
    wf:f("~pb", [ S ]). 

format_timedelta(TD) when TD <  3600 ->  %{{{1
    wf:f("~p mins ago", [wf:to_integer(TD/60)]);
format_timedelta(TD) when TD < 24 * 3600 ->  %{{{1
    wf:f("~p hrs ago", [wf:to_integer(TD/3600)]);
format_timedelta(TD) ->  %{{{1
    wf:f("~p days ago", [wf:to_integer(TD/(24 * 3600))]).

sort_by_timestamp(Updates) ->  %{{{1
    lists:reverse(lists:keysort(#message.time, Updates)).

maybe_wrap_list(AddressList) when is_list(AddressList) ->  % {{{1
    AddressList;
maybe_wrap_list(Address) when is_binary(Address) ->  % {{{1
    [Address].

once_join(L) ->  % {{{1
    string:join(sets:to_list(sets:from_list(L)), "; ").
