%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (finances).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

main() -> common:main().

title() -> "Hello from relationships.erl!".

icon() -> "<i class='icon-usd icon-2x'></i>".

buttons() ->
    #panel{class='row-fluid', body=[

    #panel{class='span9 offset2', body=[
                    #panel{class="row-fluid", body=[
                            #panel{ class='span2 btn-group', body=[
                                    #link{class="btn btn-link dropdown-toggle",data_fields=[{toggle, "dropdown"}], 
                                          body=["<i class='icon-reorder'></i> More options"], url="#", new=false},
                                    #list{class="dropdown-menu", numbered=false,
                                          body=[
                                            #listitem{body=#link{body=["<i class='icon-edit'></i> Edit selected"], postback=edit, new=false}},
                                            #listitem{body=#link{body=["<i class='icon-list-alt'></i> Archive selected"], postback=edit, new=false}},
                                            #listitem{body=#link{body=["<i class='icon-external-link'></i> Export as CSV"], postback=edit, new=false}},
                                            #listitem{body=#link{body=["<i class=''></i> Pay now"], postback=edit, new=false}},
                                            #listitem{body=#link{body=["<i class=''></i> Request payment"], postback=edit, new=false}}
                                            ]}

                                    ]},
                            #panel{ class='span2 btn-group', body=[
                                    #link{class="btn btn-link dropdown-toggle",data_fields=[{toggle, "dropdown"}], 
                                          body=["<i class='icon-user'></i> All accounts"], url="#", new=false},
                                    #list{class="dropdown-menu", numbered=false,
                                          body=[
                                            #listitem{body=#link{body=["<i class='icon-edit></i> Edit selected"], postback=edit, new=false}},
                                            #listitem{body=#link{body=["<i class='icon-list-alt'></i> Archive selected"], postback=edit, new=false}},
                                            #listitem{body=#link{body=["<i class='icon-external-link'></i> Export as CSV"], postback=edit, new=false}},
                                            #listitem{body=#link{body=["<i class=''></i> Pay now"], postback=edit, new=false}},
                                            #listitem{body=#link{body=["<i class=''></i> Request payment"], postback=edit, new=false}}
                                            ]}
                                    ]},
                    #panel{ class='span2', body="<i class='icon-filter'></i> Smart filter"},
                    #panel{ class='span2', body="<i class='icon-sort'></i> Sort"},
                    #panel{ class='span2', body="<i class='icon-list-alt'></i> Archive"}
                    ]}
            ]}]}.

left() ->
    [].

body() -> 
    {ok, Payments} = db:all_expenses(),
    [
        #table{ rows=[
                #tablerow{ cells=[
                        #tablecell{body=[
                                #checkbox{id=check_all,  postback=check_all, checked=false, delegate=common}
                                ], class=""},
                        #tableheader{text="Payable", class=""},
                        #tableheader{text="Chargable", class=""},
                        #tableheader{text="Tasks", class=""},
                        #tableheader{text="Due", class=""},
                        #tableheader{text="Status", class=""},
                        #tableheader{text="Amount", class=""},
                        #tableheader{text="Curr.", class=""},
                        #tableheader{text="Type", class=""}
                        ]},
                lists:map(fun(#db_expense{id=Id, name=Name, date=Date, type=Type, text=Text, amount=Amount, status=Status, to=To, from=From}) ->
                            {ok, #db_contact{name=FromS}} = db:get_contact(From),
                            {ok, #db_contact{name=ToS}} = db:get_contact(To),
                            {ok, Tasks} = db:get_expense_tasks(Id),
                            #payment_row{
                                pid=Id,
                                from=FromS,
                                to=ToS,
                                tasks=lists:map(fun(#db_task{name=TN}) ->
                                            [ TN, ";" ] 
                                    end, Tasks),
                                due=case Date of
                                    {Statrt, Due} ->
                                        [Statrt, " - ", Due];
                                    D ->
                                        D
                                end,
                                status=Status,
                                amount=Amount,
                                %currency=Currency,
                                type=Type}
                    end, Payments)
                ]}

        ].    

event({check, Id}) ->
    Checked = wf:session_default(checked, sets:new()),
    io:format("~p ~p~n", [wf:q(wf:f("check~p", [Id])), sets:to_list(Checked)]),
    case wf:q(wf:f( "check~p", [ Id ] )) of
        undefined ->
                wf:session(checked, sets:del_element(Id, Checked));
            "on" ->
                wf:session(checked, sets:add_element(Id, Checked))
    end;
event(edit) ->
    Checked = wf:session_default(checked, sets:new()),
    case sets:size(Checked) of
        1 ->
            [Cur] = sets:to_list(Checked),
            {ok, [ Expense ]} = db:get_expense(Cur),
            wf:session(current_expense, Expense),
            wf:redirect("/edit_expense");
        N->
            io:format("~p~n", [N]),
            wf:wire(#alert{text="Can't edit more than one expense at a time"})
    end;
event(Click) ->
    io:format("~p~n",[Click]).
