%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_contact_li).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

%% Move the following record definition to records.hrl:

-spec reflect() -> [atom()].
reflect() -> record_info(fields, contact_li).

-spec render_element(#contact_li{}) -> body().
render_element(_Record = #contact_li{uid=Id, name=Name, checked=Checked}) ->
    #draggable{tag={contact, Id}, group=contacts, clone=false, body=[
        #listitem{class="clearfix", body=[
            #checkbox{id=wf:f("cb~p",[Id]), class="pull-left", style="margin-right: 15px;",  postback={check_contact, Id}, checked=Checked},
            #span{text=Name, actions=#event{type=click, postback={contact, Id}}}
        ]}
    ]}.
