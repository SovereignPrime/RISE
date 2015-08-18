%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_rise_upload).
-include("records.hrl").
-export([
    reflect/0,
    render_element/1,
    event/1
]).


-spec reflect() -> [atom()].
reflect() -> record_info(fields, rise_upload).

-spec render_element(#rise_upload{}) -> body().
render_element(_Record = #rise_upload{
                           id=Id,
                           class=Class,
                           tag=Tag,
                           droppable_text=Text,
                           delegate=Delegate
                           }) ->
    PathId = wf:to_atom("upload_path_" ++ wf:to_list(Tag)),
    wf:wire(PathId, #event{type=change,
                           delegate=?MODULE,
                           postback={rise_upload_event, Tag, Delegate}
                          }),
    wf:wire(#script{script= "init_upload('" ++ 
                    wf:to_list(Id) ++ 
                    "', '" ++ 
                    wf:to_list(PathId) ++ 
                    "');"}),
    [
     
     #panel{id=Id,
           class=["rise_upload", "upload_drop" | Class],
           text=wf:html_encode(Text)
          },
     #hidden{id=PathId}
    ].

event({rise_upload_event, Tag, Delegate}) ->
    PathId = wf:to_atom("upload_path_" ++ wf:to_list(Tag)),
    Path = wf:q(PathId),
    wf:info("Upload PathId: ~p", [PathId]),
    Delegate:finish_upload_event(Tag, Path);
event(E) ->
    error_logger:info_msg("Event ~p occured in ~p~n", [E, ?MODULE_STRING]).
