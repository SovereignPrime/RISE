%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_group_item).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").
-export([
    reflect/0,
    render_element/1,
    event/1
]).

%% Move the following record definition to records.hrl:

-spec reflect() -> [atom()].
reflect() -> record_info(fields, group_item).

-spec render_element(#group_item{}) -> body().
render_element(_Record = #group_item{gid=Id, name=Name, sub=Sub, archive=Archive}) ->
    #listitem{class="", body=[
        #draggable{
            tag={group, Id, Archive},
            group=groups,
            distance=20,
            options=[{delay, 300}],
            clone=false,
            body=[
                case Sub of
                    [] ->
                        render_group(Id, Name, "", Archive);
                    _ ->
                        SubId = wf:f("subgroup~p", [Id]),
                        [
                            render_group(Id, Name, #expander{parent=Sub,
                                                             target=SubId
                                                            }, Archive),
                            #list{id=SubId,
                                  numbered=false,
                                  style='margin-left:15px;',
                                  body=[
                                        lists:map(fun(#db_group{id=I,
                                                                name=N,
                                                                subgroups=S}) ->
                                                          #group_item{gid=I,
                                                                      name=N,
                                                                      sub=S,
                                                                      archive=Archive}
                                                  end,
                                                  Sub)
                            ]}
                        ]
                end
        ]}
    ]}.

render_group(Id, Name, Icon, Archive) ->
    #span{id=wf:f("group~p", [Id]), style="position:relative", body=[
        #panel{style="position:absolute; left:-12px; height:15px;top:2px;", body=Icon},
        #droppable{ style="position:relative;padding-right:8px", tag={subgroup, Id}, class='relationship-group', accept_groups=[groups, contacts], body=[
                    #span{id=wf:f("group_view~p", [Id]),  text=Name, actions=[
                            #event{type=click, postback={group, Id, Archive}}
                            ]},
                    #span{id=wf:f("group_edit~p", [Id]), body=[
                            #textbox{id=wf:f("group_name~p", [Id]), text=Name, class="pull-left"}, 
                            #link{ class="btn btn-link", text="<i class='icon-ok'></i>", html_encode=false, postback={save, Id, Archive}, delegate=?MODULE},
                            #link{ class="btn btn-link", text="<i class='icon-remove'></i>", html_encode=false, actions=#event{type=click, actions=[
                                        #event{target=wf:f("group_view~p", [Id]),  actions=#show{}},
                                        #event{target=wf:f("group_edit~p", [Id]),  actions=#hide{}}
                                        ]}}
                            ], actions=#hide{}},
                    #span{class="btn-group", style="position:absolute; right:-12px; top:0", body=[
                            #link{class="btn  btn-link dropdown-toggle",data_fields=[{toggle, "dropdown"}], body=[
                                    "<i class='icon-ellipsis-vertical'></i>"
                                    ], url="#", new=false},
                            #list{numbered=false, class="dropdown-menu",
                                  body=[
                                    #listitem{body=[
                                            #link{body=[
                                                    "<i class='icon-trash'></i> Delete"
                                                    ], postback={group_delete, Id, Archive}, new=false}
                                            ]},
                                    #listitem{body=[
                                            #link{body=[
                                                    "<i class='icon-pencil'></i> Rename"
                                                    ],  actions=#event{type=click, actions=[
                                                        #event{target=wf:f("group_view~p", [Id]),  actions=#hide{}},
                                                        #event{target=wf:f("group_edit~p", [Id]),  actions=#show{}}
                                                        ]}, new=false}
                                            ]}
                                    ]}
                            ]}
                    ]}]}.

event({save, Id, Archive}) ->
    Name = wf:q(wf:f("group_name~p", [Id])),
    db:save(#db_group{id=Id, name=Name}),
    wf:update(group_list, relationships:render_group_list(Archive));
    
event(E) ->
    io:format("Event ~p in ~p~n", [E, ?MODULE]).
