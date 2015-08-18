%% -*- mode: nitrogen -*-
-module (edit_expense).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

main() -> common:main().

title() -> "Welcome to Nitrogen".

icon() -> "<i class='icon-usd icon-2x'></i>".

buttons() ->
    #panel{class='row-fluid', body=[
            #panel{class='span9 offset3', body=[
                    #panel{class="row-fluid", body=[
                           #button{ class='btn btn-link span2', body="<i class='icon-remove'></i> Discard", 
   					click=#script{script="window.history.back();"}},
                            #button{ class='btn btn-link span2', body="<i class='icon-ok'></i> Save", postback=save, delegate=?MODULE}
                            ]}
                    ]}
            ]}.

left() ->
    #panel{id=left, class="span3", body=[
            render_left()
            ]}.
render_left() ->
    #db_expense{id=Id, name=Name, date=Due, text=Text, amount=Amount, type=Type, status=Status} = wf:session(current_expense),
    {ok, Tasks} = db:get_expense_tasks(Id),
    [
        #panel{ class="row-fluid", body=
               case Type of
                expense ->
                    [
                        #panel{ class="span6", body=[
                                #button{text="Log an expense", class="btn span12 active", postback=add_expense}

                                ]},
                        #panel{ class="span6", body=[
                                #button{class="btn span12",  body="Log an assert", postback=add_assert}
                                ]}
                        ];
                assert ->
                    [
                        #panel{ class="span6", body=[
                                #button{text="Log an expense", class="btn span12 ", postback=add_expense}

                                ]},
                        #panel{ class="span6", body=[
                                #button{class="btn span12 active",  body="Log an assert", postback=add_assert}
                                ]}
                        ]
            end
              },
            #panel{ class="row-fluid", body=[
                    #panel{ class="span12", body=[
                            #textbox{id="amount", placeholder="Amount", text=Amount, next=order, class="input-block-level"}
                            ]}
                    ]},
            #panel{ class="row-fluid", body=[
                    case Type of
                        expense ->
                            #panel{ class="span12", body=[
                                    #dropdown{id=status, class="span12", value=Status, options=[
                                            #option{text="created", value="created"},
                                            #option{text="logged", value="logged"},
                                            #option{text="on hold", value="on hold"},
                                            #option{text="reimbursement requested", value="reimbursement requested"},
                                            #option{text="reimbursement approval", value="reimbursement approval"},
                                            #option{text="paied", value="paied"},
                                            #option{text="due", value="due"}
                                            ]}

                                    ]};
                        assert ->
                            #panel{ class="span12", body=[
                                    #dropdown{id=status, class="span12", value=Status, options=[
                                            #option{text="unassigned", value="unassigned"},
                                            #option{text="in use", value="in use"},
                                            #option{text="broken", value="broken"}
                                            ]}

                                    ]}
                        end
                    ]},
            #panel{ class="row-fluid", style="margin: 10% 0;", body=[
                    #panel{ class="span12", body=[
                            "<i class='icon-tasks'></i> For the tasks", #br{},
                            lists:map(fun(#db_task{name=Name}) ->
                                        Name, #br{}
                                end, Tasks),
                                        #link{url="/tasks", body="<i class='icon-th-large'></i> Edit/View task tree"}, #br{}
                            ]}
                    ]},
            #panel{ class="row-fluid", body=[
                    #panel{ class="span12", body=[
                        common:render_files(),
                            "<i class='icon-th-large'></i> Select from my files", #br{}
                            ]}
                    ]}
            ].

body() ->
    #db_expense{id=Id, name=Name, date=Due, text=Text, amount=Amount, type=Type, status=Status} = wf:session(current_expense),
    #panel{id=body, class="span9", body=[
            #panel{ class="row-fluid", body=[
                    #panel{ class="input-prepend span12", body=[
                            #span{ class="add-on", body=[
                                    "<i class='icon-usd'></i>"
                                    ]},
                            #textbox{id=name, placeholder="Name",text=Name, next=due, class="span11"}
                            ]}
                    ]},
            #panel{ class="row-fluid", body=[
                    case Type of
                        expense ->
                            #panel{ class="input-prepend input-append span12", body=[
                                    #span{ class="add-on", body=[
                                            #span{html_encode=false, text="<i class='icon-calendar'></i>"}
                                            ]},
                                    #datepicker_textbox{id=due, placeholder="Due", text=Due, class="span10"},
                                    #span{ class="add-on", body=[
                                            #span{ text="Calendar | Make recurring"}
                                            ]}
                                    ]};
                        assert ->
                            {B, E} = Due,
                            [
                            #panel{ class="input-prepend span6", body=[
                                    #span{ class="add-on", body=[
                                            #span{html_encode=false, text="<i class='icon-calendar'></i>"}
                                            ]},
                                    #datepicker_textbox{id=purchase, placeholder="Purchase date", text=B, class="span10"}
                                    ]},
                            #panel{ class="input-prepend span6", body=[
                                    #span{ class="add-on", body=[
                                            #span{html_encode=false, text="<i class='icon-calendar'></i>"}
                                            ]},
                                    #datepicker_textbox{id=due, placeholder="End of life", text=E, class="span10"}
                                    ]}
                                ]
                    end
                    ]},
            #addable_row{id=roles, body= #involved{}},
            #panel{ class="row-fluid", body=[
                    #panel{class="span12", body=[
                            #textarea{text=Text, class="input-block-level",rows=15, placeholder="Some text here", id=text}
                            ]}

                    ]},
            #panel{ class="row-fluid", body=[
                    #panel{class="span12", body=[
                            #checkbox{id=notice,class="pull-left", text=" Send notice about this update to everyone involved",  checked=true}

                            ]}

                    ]}
            ]}.

event(add_assert) ->
    CE = wf:session(current_expense),
    wf:session(current_expense, CE#db_expense{type=assert}),
    wf:update(left, render_left()),
    wf:replace(body, body());
event(add_expense) ->
    CE = wf:session(current_expense),
    wf:session(current_expense, CE#db_expense{type=expense}),
    wf:update(left, render_left()),
    wf:replace(body, body());
event(save) ->
    Name = wf:q(name),
    Due = wf:q(due),
    Payable = wf:qs(payable),
    Amount = wf:q(amount),
    Text = wf:q(text),
    Status = wf:q(status),
    #db_expense{id=Id, type=Type} = CE = wf:session(current_expense),
    #db_contact{id=UID} = wf:user(),
    NCE = case Type of
        expense ->
            CE#db_expense{name=Name, amount=Amount, text=Text, status=Status, date=Due, from=UID};
        assert ->
            Purchase = wf:q(purchase),
            CE#db_expense{name=Name, amount=Amount, text=Text, status=Status, date={Purchase, Due}, from=UID}
    end,
    db:save(NCE),
    db:save_attachments(NCE, wf:session_default(attached_files, [])),
    common:save_involved(db_expense, Id),
    wf:redirect("/finances");

event(Ev) ->
    io:format("Event ~p in module ~p~n", [Ev, ?MODULE]).
