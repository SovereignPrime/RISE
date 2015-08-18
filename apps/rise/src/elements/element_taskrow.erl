%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_taskrow).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, taskrow).

-spec render_element(#taskrow{}) -> body().
render_element(_Record = #taskrow{type=Type, name=Name, due=Due}) ->
    #tablerow{ cells=[
            #tablecell{style="width:200px;", text=Type},
            #tablecell{text=Name},
            #tablecell{text=Due, class="cell-right"}
            ]}.
