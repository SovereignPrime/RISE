%% -*- mode: nitrogen -*-
-module (edit_file).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

main() -> common:main().

title() -> "Welcome to Nitrogen".

icon() -> "<i class='icon-file-text-alt icon-2x'></i>".

buttons(left) ->  % {{{1
    "";
buttons(main) ->  % {{{1
    #panel{class='row-fluid', body=[
            #panel{class='span9 offset3', body=[
                    #panel{class="row-fluid", body=[
                            #button{ class='btn btn-link span2', body="<i class='icon-remove'></i> Discard", 
   					click=#script{script="window.history.back();"}},
                            #button{ class='btn btn-link span2', body="<i class='icon-ok'></i> Save", postback=save, delegate=?MODULE}
                            ]}
                    ]}
            ]}.

left() ->  % {{{1
    #panel{ class="span3", body=[
            #panel{ class="span12", body=[
                    "<i class='icon-plus'></i> Create new with uploaded files:"
                    ]},
            #link{ class="span12", text="Message", postback=add_update},
            #link{ class="span12", text="Task", postback=add_task}%,
            %#link{ class="span12", text="Message", postback=add_expense},
            %#link{ class="span12", text="Message", postback=add_update}
            
            
            ]}.

body() ->  % {{{1
    #panel{ class="span9", body=[
            #panel{ class="row-fluid", body=[
                            common:render_files()
                    ]}]}.
    
event(add_task) ->  % {{{1
    {ok, Id} = db:next_id(db_task),
    wf:session(current_task_id, Id),
    wf:session(current_task, #db_task{id=Id}),
    wf:redirect("/edit_task");
event(add_expense) ->  % {{{1
    {ok, Id} = db:next_id(db_expense),
    wf:session(current_expense_id, Id),
    wf:session(current_expense, #db_expense{id=Id}),
    wf:redirect("/edit_expense");
event(add_update) ->  % {{{1
    {ok, Id} = db:next_id(db_update),
    wf:session(current_subject, undefined),
    wf:session(current_update_id, Id),
    wf:session(current_update, #db_update{id=Id}),
    wf:redirect("/edit_update");
event(click) ->  % {{{1
    wf:replace(button, #panel { 
        body="You clicked the button!", 
        actions=#effect { effect=highlight }
    }).

incoming() -> ok. % {{{1
