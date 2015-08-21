%% -*- mode: nitrogen -*-
-module (edit_task).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

main() -> common:main().

title() -> "Welcome to Nitrogen".

icon() -> #image{image="/img/tasks.svg", class="icon", style="height: 32px;margin-top:-5px;"}.

buttons(left) ->  % {{{1
    "";
buttons(main) ->  % {{{1
    #panel{class='row-fluid', body=[
            #panel{class='span9 offset3', body=[
                    #panel{class="row-fluid", body=[
                            #button{class='btn btn-link span2',
                                    body="<i class='icon-remove'></i> Discard", 
                                    postback=discard,
                                    delegate=?MODULE},
                            #button{class='btn btn-link span2',
                                    body="<i class='icon-ok'></i> Save",
                                    postback=save,
                                    delegate=?MODULE}
                            ]}
                    ]}
            ]}.

left() ->  % {{{1
    #db_task{id=CId, parent=PId} = Task = wf:session_default(current_task, #db_task{}),
    wf:session(current_task, Task),
    PName = case db:get_task(PId) of
        {ok, [ #db_task{name=P} ]} ->
            P;
        _ ->
            undefined
    end,
    Children = case db:get_tasks(CId) of
        {ok, C} ->
            C;
        _ ->
            []
    end,
    #panel{ class="span3", body=[
            #panel{ class="row-fluid", style="margin: 10% 0;", body=[
                    #panel{ class="span12", body=[
                            "<i class='icon-tasks'></i> Linked tasks", #br{},
                            case PName of
                               undefined ->
                                   "";
                               N -> 
                                    [
                                        "Subtask of: ",wf:html_encode(N), #br{}
                                    ]
                            end,
                            case CId of
                                undefined ->
                                   "";
                                _ ->
                                    lists:map(fun(#db_task{name=N}) ->
                                                      [
                                                       "Subtask: ",wf:html_encode(N), #br{}
                                                      ]
                                              end, Children)
                            end
                            ]}
                    ]},
            #panel{id=files, class="row-fluid", body=[
                    common:render_files()
                    ]}
            ]}.


body() ->  % {{{1
	%fix_addable_rows(),
	Statuses = db:task_status_list(),
    #db_task{id=Id, name=Name, due=Due, text=Text, status=Status} = wf:session(current_task),
    #db_contact{id=MID, name=Me} = wf:user(),
    wf:session(<<"Me">>, MID),
    #panel{ class="span9", body=[
        #panel{ class="row-fluid", body=[
            #panel{ class="input-prepend span12", body=[
                #span{ class="add-on", body=[
                    #image{image="/img/tasks.svg", class="icon", style="height: 20px;"}
                ]},
                #textbox{id=name, placeholder="Task name", text=Name,  next=due, class="span11"}
            ]}
        ]},
        #panel{ class="row-fluid", body=[
            #panel{ class="input-prepend span9 duedate-wrapper", body=[
                #span{ class="add-on", body=[
                    #span{body="<i class='icon-calendar'></i>"}
                ]},
                #datepicker_textbox{id=due,  next=due, text=Due, class="span9"}
            ]},
			#panel{ class="input-prepend span3 status-wrapper", body=[
                #span{ class="add-on", body=[
                    #span{body="<i class='icon-fire'></i>"}
                ]},
				Status,
				#dropdown{id=status, value=Status, options=Statuses, class=span9},
				#span{class='add-on', body="<i class='icon-caret-down'></i>"}
			]}
        ]},
        tasks:render_roles(Id),
        %#addable_row{id=roles, body= #involved{person=Me, role=accountable}},
        %add_existing_rows(Id),
        #panel{ class="row-fluid", body=[
            #panel{class="span12", body=[
                #textarea{class="input-block-level",rows=15, placeholder="Some text here", id=text, text=Text}
            ]}
        ]}
    ]}.

fix_addable_rows() ->
    %% This is a bit of a hack, but it solves the problem, however, if the window is resized, it does not immediately take effect
    %% This can be solved by watching for an onresize event.
	Str = [
		"var reference_width = $(\".input-prepend\").width();",
        "var reference_left = $(\".input-prepend\").position().left;",
		"var reference_right =  + reference_width + reference_left;",
		"var plus_left = $(\".row_plus\").position().left;",
        "var plus_width = 30;",
        %"var plus_width = $(\".row_plus\").width();",
        "var plus_right = plus_left + plus_width;",
        "var differential = plus_right - reference_right;",
        "var current_width = $(\".addable-row > .span11\").width();",
        "var new_width = current_width - differential;",
		"$(\".addable-row > .span11\").width(new_width);",
		"var duedate_width = $(\".addable-row > .span11 > .row-fluid > .span9\").width();",
		"$(\".duedate-wrapper\").width(duedate_width+2);"
    ],
    wf:defer(Str).


add_existing_rows(Id) ->  % {{{1
    {ok, Involved} = db:get_involved(Id),
    Involved1 = wf:session_default(involved, []),
    Contacts = [{C, R} || {_, R, C}  <- Involved ++ Involved1],
    wf:session(involved, []),
    Tos = lists:zip(Contacts, lists:seq(1, length(Contacts))),
    lists:foreach(fun({ {#db_contact{id=I, name=C}, R  }, N }) ->
                wf:session(wf:to_binary(C), I),
                element_addable_row:event({add, #addable_row{id=roles, num= N - 1, body=#involved{person=C, role=R} }})
        end, Tos),
    case length(Tos) of
        0 ->
            element_addable_row:event({add, #addable_row{id=roles, num= length(Tos), body=#involved{} }});
        _ ->
            element_addable_row:event({del, #addable_row{id=roles, num= 0}}),
            element_addable_row:event({add, #addable_row{id=roles, num= length(Tos), body=#involved{} }})
    end,
    [].
    
event(add_file) ->  % {{{1
    TaskName = wf:to_binary(wf:q(name)),
    Due = wf:q(due),
    Text = wf:to_binary(wf:q(text)),
    Task = wf:session(current_task),
    NTask = Task#db_task{name= TaskName , due=Due, text=Text},
    wf:session(current_task, NTask),
    wf:redirect("/files/?from=task");
event(discard) ->  % {{{1
    wf:session(current_task, undefined),
    wf:session(current_task_id, undefined),
    wf:redirect("/tasks");
event(save) ->  % {{{1
    TaskName = wf:to_binary(wf:q(name)),
    Due = wf:q(due),
    Text = wf:to_binary(wf:q(text)),
	Status = db:sanitize_task_status(wf:q(status)),
    #db_task{id=Id} = Task = wf:session(current_task),
    UID = case Id of
              undefined ->
                  crypto:hash(sha512, <<TaskName/bytes, Text/bytes>>);
              I ->
                  I
    end,
    NTask = Task#db_task{name= TaskName , id=UID, due=Due, text=Text, status=Status},
    db:save(NTask),
    wf:session(current_task, NTask),
    db:save_attachments(wf:session(current_task), wf:session_default(attached_files, sets:new())),
    save_payments(TaskName),
    common:save_involved(db_task, UID),
    common:send_messages(NTask),
    wf:session(task_attached_files, undefined),
    wf:redirect("/tasks");

event(Ev) ->  % {{{1
    io:format("Event ~p in module ~p~n", [Ev, ?MODULE]).

incoming() -> ok.  % {{{1

%%%
%% Helpers
%%%
save_payments(TaskName) ->  % {{{1
    Payable = wf:qs(payable),
    Amounts = wf:qs(amount),
    #db_contact{id=UID} = wf:user(),
    Payments = [ #db_expense{name=TaskName, from=wf:session(wf:to_binary(Pay)), to=UID, amount=Am, status=new, type=expense} || {Pay, Am} <- lists:zip(Payable, Amounts)], 
    lists:foreach(fun(P) -> 
                {ok, NPId} = db:next_id(db_expense),
                db:save(P#db_expense{id=NPId}),
                db:save(#db_expense_tasks{task=wf:session(current_task_id), expense=NPId})
        end, Payments).
