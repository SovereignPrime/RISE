%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_task_leaf).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, task_leaf).

-spec render_element(#task_leaf{}) -> body().
render_element(_Record = #task_leaf{tid=Id, due=Due, name=Task, delegate=Delegate, checked=Checked, current=false}) ->  % {{{1
    EID = wf:to_atom(binary:decode_unsigned(Id)),
    {ok, Sub } = db:get_tasks(Id),
    #draggable{tag={task, Id}, group=tasks, clone=false, body=[
            #droppable{id=task, tag={subtask, Id}, accept_groups=tasks, body=[
                                           #listitem{id=EID, class="leaf clearfix", body=[
                                                                     #panel{ class="row-fluid", body=[
                                                                                                      #panel{ class="span1", body=[

                                                                                                                                   #checkbox{id=john, postback={check, Id}, checked=Checked, delegate=Delegate}
                                                                                                                                  ]},
                                                                                                      #panel{ class="span10", body=[

                                                                                                                                    "<b class='shorten-text' style='-webkit-line-clamp:1;'>", Task, "</b>",
                                                                                                                                    "Due: ",  Due
                                                                                                                                   ]},
                                                                                                      case Sub of
                                                                                                          [] ->
                                                                                                              #panel{ class="span1", style="line-height:22px;", body=[]};
                                                                                                          _ ->
                                                                                                              #panel{ class="span1", style="line-height:22px;", body=["<i class='icon-chevron-right'></i>"]}
                                                                                                      end
                                                                                                     ]}
                                                                            ], actions=#event{type=click, postback={task_chosen, Id}, delegate=Delegate}}]}]};
render_element(_Record = #task_leaf{tid=Id, due=Due, name=Task, delegate=Delegate, checked=Checked, current=true}) ->  % {{{1
    EID = wf:to_atom(binary:decode_unsigned(Id)),
    {ok, Sub } = db:get_tasks(Id),
    #draggable{tag={task, Id}, group=tasks, clone=false, body=[
            #droppable{id=task, tag={ subtask, Id }, accept_groups=tasks, body=[
                    #listitem{id=EID, class="leaf clearfix current", body=[
                                                                     #panel{ class="row-fluid", body=[
                                                                                                      #panel{ class="span1", body=[

                                                                                                                                   #checkbox{id=john, postback={check, Id}, checked=Checked, delegate=Delegate}
                                                                                                                                  ]},
                                                                                                      #panel{ class="span10", body=[
                                                                                                                                    "<b class='shorten-text'  style='-webkit-line-clamp:1;'>", Task, "</b>",
                                                                                                                                    "Due: ",  Due
                                                                                                                                   ]},
                                                                                                      case Sub of
                                                                                                          [] ->
                                                                                                              #panel{ class="span1", style="line-height:22px;", body=[]};
                                                                                                          _ ->
                                                                                                              #panel{ class="span1", style="line-height:22px;", body=["<i class='icon-chevron-right'></i>"]}
                                                                                                      end
                                                                                                     ]}
                            ]}
                    ], actions=#event{type=click, postback={task_chosen, Id}, delegate=Delegate}}]};
render_element(_Record = #task_leaf{tid=Id, due=Due, name=Task, delegate=Delegate, checked=Checked, current=parent}) ->  % {{{1
    EID = wf:to_atom(binary:decode_unsigned(Id)),
    #draggable{tag={task, Id}, group=tasks, clone=false, body=[
            #droppable{id=task, tag={subtask, Id}, accept_groups=tasks, body=[
                    #listitem{id=EID, class="leaf clearfix current", body=[
                                                                     #panel{ class="row-fluid", body=[
                                                                                                      #panel{ class="span1", body=[

                                                                                                                                   #checkbox{id=john, postback={check, Id}, checked=Checked, delegate=Delegate}
                                                                                                                                  ]},
                                                                                                      #panel{ class="span10", body=[
                                                                                                                                    "<b class='shorten-text'  style='-webkit-line-clamp:1;'>", Task, "</b>",
                                                                                                                                    "Due: ",  Due
                                                                                                                                   ]},
                                                                                                      #panel{ class="span1", style="line-height:22px;", body=["<i class='icon-chevron-right'></i>"]}
                                                                                                      
                                                                                                     ]}
                            ]}
                    ], actions=#event{type=click, postback={task_chosen, Id}, delegate=Delegate}}]}.
