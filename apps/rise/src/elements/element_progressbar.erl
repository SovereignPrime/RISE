%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_progressbar).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

%% Move the following record definition to records.hrl:

-spec reflect() -> [atom()].
reflect() -> record_info(fields, progressbar).

-spec render_element(#progressbar{}) -> body().
render_element(_Record = #progressbar{id=Id,
                                      progress=Progress,
                                      border_color=Border}) ->
    #panel{id=Id, 
           class="progress", 
           body=[#panel{class="bar",
                        style=wf:f("width:~s%;background-color:#000;background-image:none;",
                                   [Progress])}]}.
