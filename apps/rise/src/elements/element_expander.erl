%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_expander).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1,
    event/1
]).

-spec reflect() -> [atom()].  % {{{1
reflect() -> record_info(fields,expander).

-spec render_element(#expander{}) -> body().  % {{{1
render_element(_Record = #expander{data_fields=Data,
                                   parent=ParentId,
                                   class=Class,
                                   target=Target}) ->
    Id = wf:temp_id(),  
    State = wf:session_default(ParentId, open),
    #span{id=Id,
          data_fields=Data,
          class=[expander, Class, open_or_closed(State, Target)],
          style="width:10px; display:inline-block",
          actions=#event{type=click,
                         postback={state, ParentId},
                         delegate=?MODULE,
                         actions=[
                                  wf:f("var me = objs('~s');",[Id]),
                                  "if(me.hasClass('icon-caret-down')) {",
                                  "me.removeClass('icon-caret-down').addClass('icon-caret-right');",
                                  wf:f("objs('~s').hide();",[Target]),
                                  "}else{",
                                  "me.removeClass('icon-caret-right').addClass('icon-caret-down');",
                                  wf:f("objs('~s').show();",[Target]),
                                  "}"
                                 ]}}.
    


open_or_closed(open, Target) ->  % {{{1
    wf:wire(wf:f("objs('~s').show();", [Target])),
    'icon-caret-down';
open_or_closed(closed, Target) ->  % {{{1
    wf:wire(wf:f("objs('~s').hide();", [Target])),
    'icon-caret-right'.
    
event({state, Id}) ->  % {{{1
    State = wf:session_default(Id, open),
    case State of
        open ->
            wf:session(Id, closed);
        closed ->
            wf:session(Id, open)
    end.
