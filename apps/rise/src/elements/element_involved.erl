%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_involved).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").
-export([
    reflect/0,
    render_element/1,
    role_dropdown/2,
    role_dropdown/3
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, involved).

-spec render_element(#involved{}) -> body().
render_element(_Record = #involved{person = Person, role = Text}) ->
    {ok, Users, _} = db:get_users(10), % Move from here
    #panel{ class="row-fluid", body=[
            #panel{ class="input-prepend span9", body=[
                    #span{class="add-on",
                          body=[
                           #span{html_encode=false,
                                 text="<i class='icon-user'></i>"}
                           ]},
                    #textbox_autocomplete{id=person,
                                          tag=contact,
                                          next=responsible,
                                          class="span11",
                                          text = Person,
                                          delegate=common}
                    ]},
            #panel{class="dropdown span3 input-append", 
                   style="border: #000 1px solid",
                   body=[
                    role_dropdown(responsible, Text, Users)
            ]}
    ]}.

role_dropdown(Id, Value) -> % {{{1
    role_dropdown(Id, Value, []).

role_dropdown(Id, Value, Users) -> % {{{1
    [
        #dropdown{
            id=Id,
            style="border-right: #fff 0 solid;",
            value=Value,
            class="span11",
            data_fields=[
                {provide, "typeahead"}, 
                {source, Users}
            ],
            options=[#option{text=T, value=V} || {T, V} <- ?ROLES]
        },
        #span{ class="add-on",
               style="background-color: #fff; border: #fff 0px solid;",
               body="<i class='icon-caret-down'></i>"}
    ].
