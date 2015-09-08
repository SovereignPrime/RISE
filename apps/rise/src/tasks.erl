%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (tasks).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("bitmessage/include/bm.hrl").
-include("records.hrl").
-include("db.hrl").
-include("protokol.hrl").

-define(UPDATE_CURRENT(Field, Val),
            update_current_task(fun(T) ->
                T#db_task{Field=Val}
            end)
       ).

main() -> common:main().

title() -> "Hello from relationships.erl!".

icon() -> #image{image="/img/tasks.svg", class="icon", style="height: 32px;"}.


buttons(main) ->  % {{{1
    #list{numbered=false,
          class="nav nav-pills",
          style="display:inline-block",
          body=[
                #listitem{body=[
                                %#panel{ class='span2',
                                %body="<i class='icon-user'></i> All accounts"},
                               ]},
        #listitem{body=[
            #button{
                id=hide_show,
                class="btn btn-link",
                body="<i class='icon-angle-left'></i> Hide tasks",
                click=[
                    #hide{trigger=hide_show,target=tasks}, 
                    #event{postback=hide}
                ]
            }
       ]},
        #listitem{body=[
                        calendar_button(calendar)
       ]},
        #listitem{body=[
            common:render_filters()
        ]},
        %#listitem{body=[
        %    #panel{ class='span2', body="<i class='icon-sort'></i> Sort"},
        %]},
        #listitem{body=[
            #link{id=archive,
                  body="<i class='icon-list-alt'></i> Archive",
                  postback={show_archive, true},
                  delegate=?MODULE}
        ]},
        #listitem{body=[
            common:settings_menu()
        ]},
        #listitem{body=[
            common:render_help()
        ]}
    ]}.

left() ->  % {{{1
    #db_task{id=CId} = wf:session_default(current_task, #db_task{}),
    #panel{id=left,
           class="span4 scrollable",
           body=case wf:session(filter) of
                    undefined ->
                        wf:session(task_tree_mode, task_tree),
                        #panel{id=tasks,
                               body=[
                                     render_task_tree()
                                    ]};
                    D ->
                        wf:session(task_tree_mode, filter),
                        #panel{id=tasks,
                               body=[
                                     render_task_tree()
                                    ]}
                end}.


render_task_tree_buttons(Selected) ->  % {{{1
    Buttons = [
    %%  { Label, postback, width }
        {"Tree", task_tree, 2},
        {"Today", tasks_today, 2},
        {"Next", tasks_soon, 1},
        {"Overdue", tasks_overdue, 2},
        {"No Deadline", tasks_no_deadline, 3},
        {"Complete", tasks_complete, 2}
    ],
    #panel{body=lists:map(fun({Label, Postback, Size}) ->
        SizeClass = wf:to_atom(["span",wf:to_list(Size)]),
        #link{
           class=[SizeClass, 'task-tree-button',
                  ?WF_IF(Postback==Selected,
                         'task-tree-button-selected',
                         'task-tree-button-unselected')],
           text=Label,
           postback={change_mode, Postback}
        }
    end, Buttons)}.

update_task_tree() ->  % {{{1
    update_task_tree(false).

update_task_tree(Archive) ->  % {{{1
    wf:update(tasks, render_task_tree(Archive)).

render_task_tree() ->  % {{{1
    render_task_tree(false).

render_task_tree(Archive) ->  % {{{1
    Mode = wf:session_default(task_tree_mode, task_tree),
    Buttons = render_task_tree_buttons(Mode),
    Tree = case Mode of
        task_tree ->
            render_task_tree(undefined, Archive, true);
        _ ->
            render_task_list(Mode, Archive)
    end,
    highlight_selected(),
    [Buttons, Tree].

render_task_tree(ParentId, Archive, First) ->  % {{{1
    Body = case db:get_tasks(ParentId, Archive) of
        {ok, []} ->
             [];
        {ok, Tasks} ->
            #list{
               id=wf:temp_id(),
               numbered=false,
               data_fields=[{list, md5(ParentId)}],
               style=["padding-left: 10px; "],
               body=[render_subtask(T, Archive) || T <- Tasks]
            };
        {ok, [], undefined} ->
             []
    end,
    case First of
        true ->
            #droppable{tag=task_root,
                       accept_groups=[task_groups],
                       style="",
                       body=[
                             #panel{body=["Tasks",Body]}
                            ]};
        false ->
            Body
    end.

md5(undefined) ->  % {{{1
    md5("");
md5(Data) ->  % {{{1
    MD5 = crypto:hash(sha, Data),
    lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= MD5]).

render_subtask(Task = #db_task{name=Name, status=Status, due=Due, id=Id}, Archive) ->  % {{{1
    case Status==complete andalso db:are_all_child_tasks_complete(Id) of
        true -> [];
        false ->
            ThisTaskIdMd5 = md5(Id),
            {Expander, Subtree} = case render_task_tree(Id, Archive, false) of
                [] -> {#span{style="width:10px;display:inline-block"}, []};
                Tree -> 
                    Sublistid = Tree#list.id,
                    {#expander{target=Sublistid, parent=ThisTaskIdMd5, data_fields=[{parent,ThisTaskIdMd5}]},Tree}
            end,
            
            #listitem{body=[
                #droppable{tag={subtask, Id},
                           accept_groups=[task_groups],
                           body=[
                                 #draggable{tag={task, Id},
                                            group=task_groups,
                                            clone=false,
                                            distance=20,
                                            options=[{delay, 300}],
                                            body=[
                                                  #panel{style="display:block",
                                                         body=[
                                                               Expander,
                                                               render_task_link(Task)
                                                              ]}
                                                 ]}
                ]},
                Subtree
            ]}
    end.

render_task_link(Task = #db_task{name=Name, due=Due, id=Id}) -> % {{{1
    HasAttachments = does_task_have_attachments(Task),
    render_task_link(Id, Name, HasAttachments, Due).

render_task_link(Id, Name, HasAttachments, Due) -> % {{{1
    ThisTaskIdMd5 = md5(Id),
    [
        #link{postback={task_chosen, Id}, data_fields=[{link, ThisTaskIdMd5}], body=[
            #image{style="width:16px; height:16px", image="/img/tasks.svg"},
            #span{text=Name},
            "&nbsp;",
            ?WF_IF(HasAttachments, "<i title='This task has files attached' class='icon-paperclip'></i>")

        ]},
        #span{style="font-size:0.8em; white-space:nowrap",body=[
            " (",
            ?WF_IF(Due,["Due: ",sugar:date_format(Due)],"No due date"),
            ")",
            "&nbsp;",
            #link{body="<i class='icon-plus' style='font-size:10px'></i>",
                  title="Add New Sub-Task",
                  postback={add, Id}
            }
        ]}
    ].

does_task_have_attachments(Task) -> % {{{1
    {ok, Attachments} = db:get_attachments(Task),
    _HasAttachments = length(Attachments) >= 1.

expand_to_task(Taskid) ->  % {{{1
    case db:get_task(Taskid) of
        {ok, [#db_task{parent=undefined}]} ->
            ok;
        {ok, [#db_task{parent=Parentid}]} ->
            expand_task(Parentid),
            expand_to_task(Parentid);
        _ ->
            ok
    end.

expand_task(Taskid) ->  % {{{1
    Hashed = md5(Taskid),
    wf:wire(["$(\".expander[data-parent='",Hashed,"']\").addClass('icon-caret-down').removeClass('icon-caret-right')"]),
    wf:wire(["$(\".list[data-list='",Hashed,"']\").show();"]).

render_task_list(Mode, Archive) ->  % {{{1
    Function = case Mode of
        tasks_today -> fun db:get_tasks_due_today/1;
        tasks_overdue -> fun db:get_tasks_overdue/1;
        tasks_soon -> fun ?MODULE:get_next_tasks_by_date/1;
        tasks_no_deadline -> fun db:get_tasks_no_deadline/1;
        filter -> fun ?MODULE:get_tasks_by_filter/1;
        tasks_complete -> fun db:get_tasks_completed/1
    end,
    {ok, Tasks} = Function(Archive),
    #list{
       numbered=false,
       data_fields=[{list, md5(undefined)}],
       style=["padding-left: 10px; "],
       body=[render_flat_task(T, Archive) || T <- Tasks]
    }.

get_tasks_by_filter(_) ->
    Filter = wf:session(filter),
    db:search_tasks(Filter).

get_next_tasks_by_date(Archive) -> % {{{1
    Today = sugar:date_format(date()),
    {ok, lists:filter(fun(#db_task{status=complete}) ->
                              false;
                         (_) ->
                              true
                      end, lists:dropwhile(fun(#db_task{due=""}) ->
                                                   true;
                                              (#db_task{due=D}) when D < Today ->
                                                   true;
                                              (_) ->
                                                   false
                                           end, get_tasks_sorted_by_date(Archive)))}.

get_tasks_sorted_by_date(Archive) ->  % {{{1
    {ok, Tasks} = db:get_tasks(Archive),
    lists:sort(fun compare_task_date/2, Tasks).

compare_task_date(#db_task{due=""}, _) ->  % {{{1
    true;
compare_task_date(_, #db_task{due=""}) ->  % {{{1
    false;
compare_task_date(#db_task{due=A}, #db_task{due=B}) ->  % {{{1
    A =< B.

render_flat_task(Task, Archive) ->  % {{{1
    #listitem{body=render_task_link(Task)}.

body() ->  % {{{1
    case wf:session(current_task) of
        #db_task{id=Id, name=Name, due=Due, text=Text, parent=Parent, status=Status}=Task -> 
            wf:state(current_task, Task),
            highlight_selected(Id),
            #panel{id=body,
                   class="span8 scrollable",
                   body=
                   [
                    render_task(Task)
                   ]};
        undefined ->
            #panel{id=body,
                   class="span8",
                   body=[]}
    end.


render_task(#db_task{id=Id,  % {{{1
                     name=Name,
                     due=Due,
                     text=Text,
                     parent=Parent,
                     effort={EffortValue, EffortPeriod},
                     status=Status,
                     changes=Changes}=Task) ->
    error_logger:info_msg("Due: ~p~n", [Due]),
    TextF = re:replace(Text, "\r*\n", "<br>", [{return, list}, noteol, global]), 
    {ok, Updates} = db:get_task_history(Id),
    {ok, Comments} = db:get_task_comments(Id),
    AllComplete = db:are_all_child_tasks_complete(Id),
    Paied = false,
    IncompleteWarning = ?WF_IF(Status==complete andalso not(AllComplete), "<i class='icon-exclamation-sign' title=\"Task marked complete but has incomplete subtasks\"></i>"),
    StatusDropdown = #dropdown{options=db:task_status_list(Paied), value=Status},
    DueDate = ?WF_IF(Due,
                     sugar:date_string(Due),
                     "No due date"),
    DueTime = ?WF_IF(Due,
                     sugar:time_string(Due),
                     ""),
    [
     render_top_buttons(),
     #panel{class="row-fluid",
            body=[
                  #panel{class="span11",
                         body=[
                               #h1{body=#inplace_textbox{id=name,
                                                         tag=name,
                                                         text=Name}},
                               #panel{class="row-fluid",
                                      style="min-height:15px;",
                                      body=[
                                            #panel{class="span2",
                                                   style="min-height:15px;",
                                                   body=["Status: ",
                                                         IncompleteWarning]},
                                            #inplace{id=status, 
                                                     style="min-height:15px;",
                                                     class="span6",
                                                     tag=status,
                                                     text=wf:to_list(Status),
                                                     view=#span{},
                                                     edit=StatusDropdown
                                                    }
                                           ]},
                               #panel{class="row-fluid",
                                      body=[
                                            #panel{ class="span2", body="Due: "},
                                            #inplace{id=due_date,
                                                     style="min-height:15px;",
                                                     class="span2",
                                                     tag=due_date,
                                                     text=DueDate,
                                                     view=#span{},
                                                     start_mode=view,
                                                     edit=#datepicker_textbox{text=DueDate}
                                                    },
                                            #inplace{id=due_time,
                                                     style="min-height:15px;",
                                                     class="span2",
                                                     tag=due_time,
                                                     text=DueTime,
                                                     view=#span{},
                                                     start_mode=view,
                                                     edit=#textbox{text=DueTime}
                                                    }
                                           ]},
                               #panel{class="row-fluid",
                                      body=[
                                            #panel{class="span1", body="Level of effort: "},
                                            #inplace_textbox{id=effort_value,
                                                             style="min-height:15px;",
                                                             class="span2",
                                                             tag=effort_value,
                                                             text=wf:to_list(EffortValue)
                                                            },
                                            #inplace{id=effort_period, 
                                                     style="min-height:15px;",
                                                     class="span6",
                                                     tag=effort_period,
                                                     text=EffortPeriod,
                                                     view=#span{},
                                                     edit=#dropdown{
                                                             options=[
                                                                      {hours, "hours"},
                                                                      {days, "days"},
                                                                      {weeks, "weeks"},
                                                                      {years, "years"}
                                                                     ],
                                                             value=EffortPeriod} 
                                                    }
                                           ]},
                               render_roles(Id)
                              ]},
                  #panel{class="span1",
                         body=render_side_buttons(Id, Task)}
                 ]},
     #br{},
     #panel{class="row-fluid",
            body=[
                  #panel{class="span10",
                         body=[
                               #inplace_textarea{id=text,
                                                 class="span12",
                                                 tag=text,
                                                 html_encode=whites,
                                                 text=Text}
                              ]}
                 ]},
     render_attachments(Task),
     render_comments(Comments),
     render_updates(Updates),
     render_task_changes(Changes)
    ]. 

get_involved_full() -> % {{{1
    #db_task{id=Id} = wf:state(current_task),
    get_involved_full(Id).

get_involved_full(new) -> % {{{1
    {ok, []};
get_involved_full(Id) -> % {{{1
    case wf:state(involved) of
        undefined ->
            {ok, Inv} = db:get_involved_full(Id),
            wf:state(involved, Inv),
            {ok, Inv};
        Inv ->
            {ok, Inv}
    end.


render_roles(Id) -> % {{{1
    {ok, Involved} = get_involved_full(Id),
    #sigma_search{tag=involved, 
                  placeholder="Involved", 
                  class="input-append input-prepend input-block-level search no-border", 
                  textbox_class="",
                  search_button_class="hidden btn btn-inverse search-btn wfid_to_field", 
                  search_button_text="<i class='icon icon-search'></i>",
                  x_button_class="search-x",
                  clear_button_class="hidden pull-right btn btn-inverse",
                  clear_button_text="<i class='icon icon-remove'></i>",
                  results_summary_class="search-results span10",
                  badges=[render_role_row(Inv) || Inv <- Involved],
                  delegate=common}.

render_role_row({#db_contact_roles{role=Role}, Name}) -> % {{{1
    search:simple_badge({Role, Name}, [ R || {_, R} <- ?ROLES]).

render_role_edit_row(OriginalData = {ContactRole, Name}) -> % {{{1
    Rowid = wf:temp_id(),
    RoleFieldid = wf:temp_id(),
    NameFieldid = wf:temp_id(),
    #panel{id=Rowid,
           class="row-fluid",
           body=[
                 #panel{class=span2,
                        body=[
                              element_involved:role_dropdown(RoleFieldid, ContactRole#db_contact_roles.role)
                             ]},
                 #panel{class=span4,
                        body=[
                              #textbox_autocomplete{id=NameFieldid,
                                                    tag=contact,
                                                    class="span11",
                                                    text=Name,
                                                    delegate=common}
                             ]},
                 #panel{class=span2,
                        body=[
                              #button{class="btn btn-link",
                                      body="<i class='icon-ok'></i>",
                                      postback={save_role,
                                                Rowid,
                                                OriginalData,
                                                RoleFieldid,
                                                NameFieldid}},
                              #button{class="btn btn-link",
                                      body="<i class='icon-remove'></i>",
                                      postback={cancel_role,
                                                Rowid,
                                                OriginalData}}
                             ]}
                ]}.


render_top_buttons() -> % {{{1
    #panel{id=top_buttons,
           class="row-fluid",
           style="display:none",
           body=[
                 #panel{class="span4 offset4",
                        body=[
                              #panel{class="row-fluid",
                                     body=[
                                           #button{class="btn btn-link span6",
                                                   body="<i class='icon-remove'></i> Discard",
                                                   postback=discard},

                                           #button{class="btn btn-link span6",
                                                   body="<i class='icon-ok'></i> Save",
                                                   postback=save}
                                          ]}
                               ]}
                 ]}.

render_side_buttons(Id, Task) -> % {{{1
    [
        #panel{class="btn btn-link", body = [
            #link{postback={edit, Id}, new=false, body=[
                "<i class='icon-edit icon-large'></i><br>"      
            ]}
        ]},
        #br{},
        #panel{class="btn-group", body=[
            #link{new=false,
                  data_fields=[{toggle, "dropdown"}],
                  class="btn btn-link droppdown-toggle",
                  body="<i class='icon-reorder icon-large'></i>"
            },
            #list{numbered=false, class="dropdown-menu pull-right", body=[
                #listitem{body=[
                    #link{postback={duplicate, Task},
                          new=false,
                          body=[
                                "<i class='icon-copy icon-large'></i> Duplicate"
                               ]},
                    #link{postback={archive, Task},
                          new=false,
                          body=[
                                "<i class='icon-list-alt icon-large'></i> Archive"
                               ]}
                ]}
            ]}
        ]}
    ].

render_attachments(Task) ->
    case db:get_attachments(Task) of 
        {ok, []} ->
            wf:session(attached_files, sets:new()),
            [];
        {ok, [], undefined} ->
            wf:session(attached_files, sets:new()),
            [];
        {ok, Attachments} ->
            Att = lists:map(fun(#bm_file{hash=AID}) ->
                                    AID
                            end, Attachments),
            wf:session(attached_files, sets:from_list(Att)),
            [
                #br{},
                #panel{class="row-fluid", body=[
                    #panel{class="span6", body="<i class='icon-file-alt'></i> Attachment"},
                    #panel{class="span2 offset4", body="<i class='icon-download-alt'></i> Download all"}
                ]},
                lists:map(fun(#bm_file{name=Path,
                                       size=Size,
                                       time={Date,
                                             _Time},
                                       hash=Id,
                                       status=State}) ->
                    #attachment{fid=Id,
                                filename=Path,
                                size=Size,
                                time=Date,
                                status=State}
                end, Attachments)
            ]
    end.

render_updates([]) -> [];  % {{{1
render_updates(Updates) -> % {{{1
    [
        #br{},
        #panel{class="row-fluid", body=[
            #panel{class="span6", body="<i class='icon-envelope'></i> Related Messages"}
        ]},
        [#update_element{
           collapse=true,
           message=M} || M <- sugar:sort_by_timestamp(Updates)]
    ].


render_task_changes([]) -> [];  % {{{1
render_task_changes(Changes) ->  % {{{1
    [
        #br{},
        #panel{class="row-fluid", body=[
            #panel{class="span12", body="<i class='icon-time'></i> Change History"}
        ]},
        [render_task_change(C) || C <- Changes]
    ].

render_task_change(C) ->  % {{{1
    Contact = case db:get_contact_by_address(C#db_task_change.address) of
                  none -> "Anonymous";
                  {ok, Co} -> Co#db_contact.name
              end,
    Datetime = C#db_task_change.datetime,
    #panel{class="row-fluid", body=[
        #panel{class="span3", text=sugar:date_format(Datetime)},
        #panel{class="span3", text=Contact},
        #panel{class="span6", text=["changed ",C#db_task_change.field," to ",C#db_task_change.new_value]}
    ]}.

render_comments(Comments) -> % {{{1
    #db_contact{name=Me} = wf:user(),
    [
        #br{},
        #panel{class="row-fluid", body=[
            #panel{class="span6", body="<i class='icon-envelope'></i> Comments"}
        ]},
        #panel{class="row-fluid",
               body=[
                     #panel{class="span3",
                            text=sugar:date_format(calendar:local_time())},
                     #panel{class="span2", text=Me},
                     #panel{class="span6",
                            body=#textbox{id=comment,
                                          style="min-height:15px;border:0px;box-shadow:none;padding-left:0",
                                          placeholder="Add comment here"}},

                     #panel{class=span1,
                            style="min-height:20px; height:20px; text-align:right",
                            body=[
                                  #link{id=ok, 
                                        body=["<i class='icon-ok'></i>"],
                                        html_encode=false,
                                        postback=add_comment,
                                        delegate=?MODULE}
                                 ]}
                    ]},
        [render_comment(M) || M <- sugar:sort_by_timestamp(Comments)]
    ].

render_comment(#message{from=From, text=Data, time=Datetime}) ->  % {{{1
    Contact = case db:get_contact_by_address(From) of
                  none -> "User " ++ sugar:date_format(calendar:local_time());
                  {ok, Co} -> Co#db_contact.name
              end,
    Text = try binary_to_term(Data) of
               #task_comment{text=T} ->
                   T;
               _ ->
                   "Wrong comment"
           catch
               error:_ ->
                   "Wrong comment"
           end,
    #panel{class="row-fluid", body=[
        #panel{class="span3",
               text=case Datetime of
                        undefined -> 
                            sugar:date_format(calendar:local_time());
                        _ ->
                            sugar:ttl_to_readable(Datetime)
                    end},
        #panel{class="span3", text=Contact},
        #panel{class="span6", text=Text}
    ]}.

render_calendar_view(Y, M) ->  % {{{1
    {ok, Tasks} = db:get_tasks_by_month(Y, M),
    TasksByDate = lists:keysort(#db_task.due, Tasks),
    FirstDay = calendar:day_of_the_week({Y, M, 1}),
    LastDay = calendar:last_day_of_the_month(Y, M),
    Months = {
      "Jan",
      "Feb",
      "Mar",
      "Apr",
      "May",
      "Jun",
      "Jul",
      "Aug",
      "Sep",
      "Oct",
      "Nov",
      "Dec"
     },
    Days = {"Mon",
            "Tue",
            "Wed",
            "Thu",
            "Fri",
            "Sat",
            #span{style="color:#f00000",
                  text="Sun"}},
    {Prev, Next} = case M of
                       1 ->
                           {{calendar, Y - 1, 12},
                            {calendar, Y, 2}};
                       12 ->
                           {{calendar, Y , 11},
                            {calendar, Y + 1, 1}};
                       M ->
                           {{calendar, Y, M - 1},
                            {calendar, Y, M + 1}}
                   end,
    #panel{
       body=[
             #panel{
                style="cursor:pointer;display:inline-block;width:25%;",
                body="<i class='icon-angle-left'></i> Previous",
                actions=#event{type=click,
                               postback=Prev}
               },
             #panel{
                style="cursor:pointer;display:inline-block;width:50%;text-align:center;",
                body=[element(M, Months),
                      ", ",
                      wf:to_list(Y)
                     ]
               },
             #panel{
                style="cursor:pointer;display:inline-block;width:25%;text-align:right;",
                body="Next <i class='icon-angle-right'></i>",
                actions=#event{type=click,
                               postback=Next}
               },
             #panel{
                id=calendar_grid,
                style="width: 100%;display:table;",
                body=lists:map(
                       fun(Week) ->
                               #panel{
                                  class="calendar-week",
                                  style="width: 100%;display:table-row;",
                                  body=lists:map(
                                         fun(Day) when Week == 1,
                                                       Day < FirstDay;
                                                       Day + 7 * (Week - 1) >= LastDay + FirstDay ->
                                                 #panel{
                                                    style="display:table-cell;border: #000 0px solid; width:15%;",
                                                    body=[
                                                          #panel{
                                                             style="float:right;",
                                                             text=""}
                                                         ]
                                                   };
                                            (Day) when Week == 0 ->
                                                 #panel{
                                                    style="display:table-cell;border: #000 0px solid; width:15%;",
                                                    body=[
                                                          #panel{
                                                             body=element(Day, Days)}
                                                         ]
                                                   };
                                            (Day) ->
                                                 Date = (Day - FirstDay + 1) + 7 * (Week - 1),
                                                 #panel{
                                                    style="border: #000 0px solid;display:table-cell;padding:2%;",
                                                    body=[
                                                          #panel{
                                                             style="width:100%;text-align:right;",
                                                             text=wf:to_list(Date)},
                                                          lists:map(
                                                            fun(#db_task{
                                                                   name=N,
                                                                   id=Id,
                                                                   due={{Y1, M1, D1}, 
                                                                        {H, Mi, _}}
                                                                  }) when Y1 == Y, 
                                                                          M1 == M,
                                                                          D1 == Date ->
                                                                    Text = wf:f("~p:~2..0w ~16s", [H, M, N]),

                                                                    #panel{
                                                                       class="badge",
                                                                       style="cursor:pointer;width:95%;background-color:#000;",
                                                                       text=Text,
                                                                       actions=#event{
                                                                                  type=click,
                                                                                  postback={to_task, Id},
                                                                                  delegate=common}};
                                                               (_) -> []
                                                            end,
                                                            TasksByDate)
                                                         ]
                                                   }
                                         end,
                                         lists:seq(1, 7))}
                       end,
                       lists:seq(0, 5))}]}.

highlight_selected() ->  % {{{1
    case wf:session(current_task) of
        #db_task{id=Id} -> highlight_selected(Id);
        _ -> ok
    end.

highlight_selected(new) ->  % {{{1
    ok;
highlight_selected(Id) ->  % {{{1
    Md5 = md5(Id),
    wf:defer(#remove_class{target=".wfid_tasks a", class=current}),
    wf:defer(#add_class{target=".wfid_tasks a[data-link=\"" ++ Md5 ++ "\"]", class=current}).

check_changing_task_status() -> ok.  % {{{1

save_contact_role(CR = #db_contact_roles{id=new}) -> % {{{1
    #db_task{id=Taskid} = wf:state(current_task),
    {ok, Id} = db:next_id(db_contact_roles),
    NewCR = CR#db_contact_roles{
              id=Id,
              type=db_task,
              tid=Taskid},
    save_contact_role(NewCR);
save_contact_role(CR) -> % {{{1
    db:save(CR).

event({change_mode, Mode}) ->  % {{{1
    wf:session(task_tree_mode, Mode),
    update_task_tree();
event({duplicate, #db_task{name=Name, text=Text} = OTask}) ->  % {{{1
    NName = <<Name/bytes,  " (copy)">>,
    VID = crypto:hash(sha512, <<NName/bytes, Text/bytes>>),
    Task = OTask#db_task{id=VID, name=NName},
    wf:state(current_task, Task),
    db:save(Task),
    db:save_attachments(wf:state(current_task), wf:session_default(attached_files, sets:new())),
    Involved = wf:state(involved),
    [save_contact_role(ContactRole#db_contact_roles{id=new}) || {ContactRole, _} <- Involved],
    %save_payments(TaskName),
    wf:session(task_attached_files, undefined),
    wf:redirect("/tasks");
event({archive, #db_task{id=_Id, parent=_Parent} = Rec}) ->  % {{{1
    {ok, NTask} = db:archive(Rec),
    common:send_messages(NTask),
    update_task_tree(),
    wf:update(body, render_task(Rec));
event({show_archive, true}) ->  % {{{1
    update_task_tree(true),
    wf:replace(archive, #link{id=archive, body="<i class='icon-list-alt'></i> Actual", postback={show_archive, false}}),
    wf:update(subgroups, []);
event({show_archive, false}) ->  % {{{1
    update_task_tree(false),
    wf:replace(archive, #link{id=archive, body="<i class='icon-list-alt'></i> Archive", postback={show_archive, true}}),
    wf:update(subgroups, []);
event({task_chosen, Id}) ->  % {{{1
    common:maybe_unsaved(fun() ->
                                 Right = wf:session(right_parent_id),
                                 {ok, [ #db_task{parent=Par, status=S} = Task ]} = db:get_task(Id),
                                 wf:session(current_task, Task),
                                 wf:state(involved, undefined),
                                 wf:state(current_task, Task),
                                 wf:update(body, render_task(Task)),
                                 expand_task(Id),
                                 highlight_selected(Id)
                         end);
event({add, ParentId}) -> % {{{1
    wf:session(current_task, #db_task{parent=ParentId}),
    wf:redirect("/edit_task");
event({edit, Id}) ->  % {{{1
    Task = wf:session(current_task),
    wf:session(current_task, Task),
    wf:redirect("/edit_task");
event({calendar, Y, M}) -> % {{{1
    wf:update(body, render_calendar_view(Y, M)),
    wf:replace(calendar, calendar_button(task));

event(task) -> % {{{1
    wf:update(body, body()),
    wf:replace(calendar, calendar_button(calendar));

event(save) -> % {{{1
    Task = wf:state(current_task),
    Task2 = calculate_changes(Task),
    wf:state(current_task, Task2),
    db:save(Task2),
    Involved = wf:state(involved),
    db:clear_involved(Task2),
    [save_contact_role(ContactRole) || {ContactRole, _} <- Involved],
    common:send_messages(Task2),
    wf:state(unsaved, false),
    update_task_tree(),
    event({task_chosen, Task2#db_task.id});
event(discard) -> % {{{1
    Task = wf:state(current_task),
    wf:state(unsaved, false),
    event({task_chosen, Task#db_task.id});
    
event(hide) ->  % {{{1
    wf:wire(body, [#remove_class{class="span8"}, #add_class{class="span12"}]),
    wf:replace(hide_show, #button{id=hide_show, class="btn btn-link", body="Show task list <i class='icon-angle-right'></i>", 
                                    actions=#event{type=click, actions=[
                                        #show{trigger=hide_show,target=tasks}, 
                                        #event{postback=show}
                                        ]}});
event(show) ->  % {{{1
    wf:wire(body, [#remove_class{class="span12"}, #add_class{class="span8"}]),
    wf:replace(hide_show, #button{id=hide_show, class="btn btn-link", body="<i class='icon-angle-left'></i> Hide task list", 
                                    actions=#event{type=click, actions=[
                                        #hide{trigger=hide_show,target=tasks}, 
                                        #event{postback=hide}
                                        ]}});

event(add_comment) -> % {{{1
    Text = wf:q(comment), 
    if Text == "" ->
           ok;
       true ->
           #db_task{id=TID} = T = wf:session(current_task),
           common:send_messages(#task_comment{task=TID, 
                                              text=Text,
                                              time=calendar:universal_time()}),
           wf:update(body, render_task(T))
    end;
event(add_role) -> % {{{1
    wf:insert_bottom(role_wrapper, render_role_edit_row({#db_contact_roles{id=new}, ""}));

event({edit_role, Rowid, OriginalData}) -> % {{{1
    wf:replace(Rowid, render_role_edit_row(OriginalData));

event({cancel_role, Rowid, OriginalData}) -> % {{{1
    case OriginalData of
        {#db_contact_roles{id=new}, _} ->
            wf:remove(Rowid);
        _ ->
            wf:replace(Rowid, render_role_row(OriginalData))
    end;

event({save_role, Rowid, {OrigContactRole, _OrigName} = OriginalData, RoleFieldid, NameFieldid}) -> % {{{1
    Name = wf:q(NameFieldid),
    Role = wf:q(RoleFieldid),
    {ok, #db_contact{id=Contactid}} = db:get_contacts_by_name(Name),
    ContactRole = OrigContactRole#db_contact_roles{contact=Contactid, role=Role},
    {ok, CurrentInvolved} = get_involved_full(),
    %% In-place editing of the lists contents since there isn't a lists:replace function
    NewInvolved = case lists:member(OriginalData, CurrentInvolved) of
        true ->
            lists:map(fun(Data) ->
                        case Data of
                                OriginalData -> ContactRole;
                                _ -> Data
                        end
            end, CurrentInvolved);
        false ->
            CurrentInvolved ++ [{ContactRole, Name}]
    end,
    wf:state(involved, NewInvolved),
    maybe_show_top_buttons(),
    wf:replace(Rowid, render_role_row({ContactRole, Name}));

event(Click) ->  % {{{1
    io:format("~p~n",[Click]).



inplace_textarea_event(text, Val) -> % {{{1
    ?UPDATE_CURRENT(text, wf:to_binary(Val)),
    Val.

inplace_textbox_event(effort_value, Val) -> % {{{1
    error_logger:info_msg(Val),
    #db_task{effort={_Value, Period}} = wf:state(current_task),
    ?UPDATE_CURRENT(effort, {list_to_float(Val), Period}),
    Val;

inplace_textbox_event(name, Val) -> % {{{1
    ?UPDATE_CURRENT(name, wf:to_binary(Val)),
    Val.

inplace_event(effort_period, Val) ->  % {{{1
    error_logger:info_msg(Val),
    #db_task{effort={Value, _Period}} = wf:state(current_task),
    ?UPDATE_CURRENT(effort, {Value, wf:to_atom(Val)}),
    Val;

inplace_event(status, Val) ->  % {{{1
    NewStatus = db:sanitize_task_status(Val),
    #db_task{id=Taskid} = wf:state(current_task),
    Acceptable = NewStatus=/=complete orelse db:are_all_child_tasks_complete(Taskid),
    case Acceptable of
        true ->
            ?UPDATE_CURRENT(status, db:sanitize_task_status(Val)),
            Val;
        false -> 
            Task = wf:state(current_task),
            Status = Task#db_task.status,
            wf:wire(#alert{text="Sorry, you can not mark a task as complete until all its subtasks are also complete"}),
            Status
    end;

inplace_event(due_date, Val) when Val == "No Due Date" -> % {{{1
    ?UPDATE_CURRENT(due, undefined),
    Val;
inplace_event(due_date, Val) -> % {{{1
    error_logger:info_msg(Val),
    {DueDate, _DueTime} = DT  = sugar:date_from_string(Val),
    NewVal = case wf:state(current_task) of
                 #db_task{due=undefined} -> DT;
                 #db_task{due=Due} ->
                     {_DueDateO, DueTimeO} = sugar:date_from_string(Due),
                     {DueDate, DueTimeO}
             end,
    ?UPDATE_CURRENT(due, NewVal),
    Val;

inplace_event(due_time, Val) when Val == "" -> % {{{1
    ?UPDATE_CURRENT(due, undefined),
    Val;
inplace_event(due_time, Val) -> % {{{1
    error_logger:info_msg(Val),
    {_DueDate, DueTime} = sugar:date_from_string("2015-01-01 " ++ Val),
    #db_task{due=Due} = wf:state(current_task),
    {DueDateO, _DueTimeO} = sugar:date_from_string(Due),
    NewVal = {DueDateO, DueTime},
    ?UPDATE_CURRENT(due, NewVal),
    Val;
inplace_event(_, V) ->  % {{{1
    V.

update_current_task(Fun) -> % {{{1
    Task = wf:state(current_task),
    NewTask = Fun(Task),
    maybe_show_top_buttons(NewTask),
    wf:state(current_task, NewTask).

maybe_show_top_buttons() -> % {{{1
    CurrentTask = wf:state(current_task),
    maybe_show_top_buttons(CurrentTask).

maybe_show_top_buttons(CurrentTask) -> % {{{1
    #db_task{id=Taskid} = wf:state(current_task),
    case db:get_task(Taskid) of
        {ok, [TaskFromDB]} ->
            {ok, InvolvedFromDB} = db:get_involved_full(Taskid),
            NewInvolved = wf:state(involved),


            TaskChanged = TaskFromDB =/= CurrentTask,
            InvolvedChanged = sets:from_list(InvolvedFromDB) /= sets:from_list(NewInvolved),

            case TaskChanged orelse InvolvedChanged of
                true -> 
                    wf:info("OldTask: ~p~n
                    NewTask: ~p~n", [TaskFromDB, CurrentTask]),
                    wf:state(unsaved, true),
                    wf:wire(top_buttons, #show{});
                false -> 
                    wf:state(unsaved, false),
                    wf:wire(top_buttons, #hide{})
            end;
        _ -> 
            wf:info("NewTask: ~p~n", [CurrentTask]),
            wf:state(unsaved, true),
            wf:wire(top_buttons, #show{})
    end.

calculate_changes(#db_task{id=new, name=Name, text=Text}=Task) -> % {{{1
    Id = crypto:hash(sha512, <<Name/bytes, Text/bytes>>),
    Task#db_task{id=Id};
calculate_changes(Task) -> % {{{1
    Id = Task#db_task.id,
    {ok, [TaskFromDB]} = db:get_task(Id),
    Fields = record_info(fields, db_task),
    #db_contact{address=Me} = wf:user(),
    OriginalChanges = Task#db_task.changes,
    NewChanges = lists:foldl(fun ({_, changes}, Acc) -> Acc;
                                 ({_, id}, Acc) -> Acc;
                                 ({Fieldnum, Field}, Acc) ->
                                     case element(Fieldnum+1, Task) =:= element(Fieldnum+1, TaskFromDB) of
            true -> Acc;
            false ->
                FieldValue = sugar:date_format(element(Fieldnum+1, Task)),
                IsComplex = is_tuple(FieldValue),
                wf:info("FieldValue: ~p", [FieldValue]),
                %IsDate = IsComplex andalso calendar:valid_date(element(1, FieldValue)),
                NewValue = if is_list(FieldValue) ->
                                  string:strip(lists:flatten(io_lib:format("~100s",[FieldValue])));
                              %IsDate ->
                              %    string:strip(lists:flatten(io_lib:format("~100s",[sugar:date_format(FieldValue)])));
                              IsComplex ->
                                  string:strip(lists:flatten(io_lib:format("~100p",[FieldValue])));
                              true ->
                                  string:strip(lists:flatten(io_lib:format("~100p",[FieldValue])))
                           end,
                                  

                IsShortened = wf:to_list(NewValue) =/= wf:to_list(FieldValue),
                NewValue2 = ?WF_IF(IsShortened, NewValue ++ "...", NewValue),
                Change = #db_task_change{
                            address=Me,
                            datetime=calendar:local_time(),
                            field=Field,
                            new_value=NewValue2
                         },
                [Change | Acc]
        end
    end, OriginalChanges, lists:zip(lists:seq(1, length(Fields)),Fields)),

    Involved = wf:state(involved),
    {ok, OriginalInvolved} = db:get_involved_full(Id),

    NewChanges2 = case sets:from_list(Involved) == sets:from_list(OriginalInvolved) of
                      true -> NewChanges;
                      false -> [#db_task_change{
                                   address=Me,
                                   datetime=calendar:local_time(), 
                                   field=involved,
                                   new_value=summarize_involved()
                                } | NewChanges]
                  end,
    Task#db_task{changes=NewChanges2}.

summarize_involved() -> % {{{1
    Involved = wf:state(involved),
    InvolvedStrings = [(wf:to_list(CR#db_contact_roles.role) ++ ": " ++ wf:to_list(Name)) 
                       || {CR, Name} <- Involved],
    string:join(InvolvedStrings, ", ").


drop_event({task, Id}, { subtask, PId }) when PId =:= Id->  % {{{1
    ok;

drop_event({task, Id}, { subtask, PId }) when PId /= Id->  % {{{1
    case db:get_task(PId) of
        {ok, [#db_task{parent=Id}]} ->
            ok;
        _ ->
            case db:get_task(Id) of
                {ok, [#db_task{parent=PId}]} ->
                    ok;
                _ ->
                    db:save_subtask(Id, PId, bm_types:timestamp()),
                    common:send_task_tree(Id, PId, bm_types:timestamp()),
                    update_task_tree(),
                    expand_to_task(Id),
                    event({task_chosen, Id})
            end
    end;

drop_event({task, Id}, task_root) ->  % {{{1
    PId = wf:session(left_parent_id),
    db:save_subtask(Id, PId, bm_types:timestamp()),
    common:send_task_tree(Id, PId, bm_types:timestamp()),
    update_task_tree().

incoming() ->  % {{{1
    wf:update(tasks, render_task_tree()),
    wf:flush().

calendar_button(calendar) -> % {{{1
    {Y, M, _} = date(),
    #button{
       id=calendar,
       class="btn btn-link",
       body="<i class='icon-calendar'></i> Calendar view",
       click=[
              #event{postback={calendar, Y, M}}
             ]
      };
calendar_button(task) -> % {{{1
            #button{
                id=calendar,
                class="btn btn-link",
                body="<i class='icon-calendar'></i> Task view",
                click=[
                    #event{postback=task}
                ]
            }.
