%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_popup).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

-export([
    reflect/0,
    render_element/1,
    event/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, popup).

-spec render_element(#popup{}) -> body().
render_element(Record = #popup{id=Id, header=Title, body=Body}) ->  % {{{1
    TId = wf:temp_id(),
    #panel{id=Id,
           class=["wfid_" ++ TId, "modal", "fade"],
           body=[
                 #panel{ class="modal-header",
                         body=[
                               #button{class="btn-link pull-right",
                                       text="x",
                                       postback={close, Id},
                                       delegate=?MODULE},
                               #h3{text=Title}
                              ]},
                 #panel{style="text-align:center;",
                        class="modal-body",
                        body=Body}
                ]}.

event({close, Id}) ->  % {{{1
    wf:wire(#script{script="$(obj('" ++ wf:to_list(Id) ++ "')).modal('hide')"}),
    wf:remove(Id);
event({show, Id}) ->  % {{{1
    wf:wire(#script{script="$(obj('" ++ wf:to_list(Id) ++ "')).modal('show')"});
event(E) ->  % {{{1
    wf:console_log("Event ~p occured in ~p~n", [E, ?MODULE]).


