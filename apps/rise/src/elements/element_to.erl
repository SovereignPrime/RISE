%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_to).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, involved).

-spec render_element(#to{}) -> body().
render_element(_Record = #to{text=Value}) ->
    #panel{class="row-fluid",
           body=[
                 #panel{class="input-prepend span12",
                        body=[
                              #span{class="add-on",
                                    body=[
                                          #span{html_encode=false,
                                                text="<i class='icon-user'></i>"}
                                         ]},
                              #textbox_autocomplete{id=person,
                                                    tag=contact,
                                                    next=responsible,
                                                    class="span11",
                                                    text=Value,
                                                    delegate=common}
                             ]}
                ]}.
