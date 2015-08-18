%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_row_fluid).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

%% Move the following record definition to records.hrl:
-record(row_fluid, {?ELEMENT_BASE(element_row_fluid),
        attr1 :: any(),
        attr2 :: any()
    }).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, row_fluid).

-spec render_element(#row_fluid{}) -> body().
render_element(_Record = #row_fluid{}) ->
    "<b>Hello from row_fluid</b>".
