%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (files).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("bitmessage/include/bm.hrl").
-include("records.hrl").
-include("db.hrl").

main() -> common:main().

title() -> "Hello from relationships.erl!".

icon() -> "<i class='icon-file-text-alt icon-2x'></i>".

buttons(left) ->  % {{{1
    "";
buttons(main) ->  % {{{1
    #list{numbered=false, class="nav nav-pills", style="display:inline-block;",body=[
        case wf:q(from) of
            "task" -> done_button("/edit_task","Add to Task");
            "message" -> done_button("/edit_update", "Add to Message");
            _ -> more_options_buttons()
        end,
        #listitem{body=[
            %#panel{ class='span2', body="<i class='icon-user'></i> All accounts"},
        ]},
        #listitem{body=[
            common:render_filters()
        ]},
        #listitem{body=[
            %#panel{ class='span2', body="<i class='icon-sort'></i> Sort"},
        ]},
        #listitem{body=[
            #link{id=archive, body="<i class='icon-list-alt'></i> Archive", postback={show_archive, true}}
        ]},
        #listitem{body=[
            common:settings_menu()
        ]},
        #listitem{body=[
            common:render_help()
        ]}
    ]}.

done_button(Url, Label) ->  % {{{1
    #listitem{body=[
        #link{class="btn btn-link", url=Url, new=false, body=[
            "<i class='icon-ok'></i>Done, ", Label
        ]}
    ]}.

more_options_buttons() ->  % {{{1
    #listitem{class="dropdown", body=[
        #link{class="btn btn-link dropdown-toggle",data_fields=[{toggle, "dropdown"}], url="#", new=false, body=[
            "<i class='icon-reorder'></i> More options"
        ]},
        #list{class="dropdown-menu", numbered=false, body=[
            #listitem{body=#link{body=["<i class='icon icon-list-alt'></i> Archive selected"], postback=archive, new=false}},
            #listitem{body=#link{body="Add to message", url = "/edit_update", new=false}},
            #listitem{body=#link{body="Add to task", url = "/edit_task", new=false}}%,
            %#listitem{body=#link{body="Add to expense", postback=edit, new=false}},
            %#listitem{body=#link{body="Add to assert", postback=edit, new=false}}
        ]}
    ]}.


left() ->  % {{{1
    [].

body() ->  % {{{1
    body(false).
body(Archive) ->   % {{{1
    {ok, Files0} = db:get_files(Archive),
    Sortby = wf:state_default(sortby, #bm_file.name),
    Files = lists:sort(fun(A,B) -> element(Sortby, A) =< element(Sortby, B) end, Files0),
    #panel{id=body,
           class="scrollable",
           body=[
                 #table{rows=[
                              #tablerow{cells=[
                                               #tablecell{class="",
                                                          body=[
                                                                %#checkbox{id=check_all,
                                                                %postback=check_all,
                                                                %checked=false,
                                                                %delegate=common}
                                                               ]},
                                               sortheader("File name", #bm_file.name),
                                               sortheader("Type", 2),
                                               sortheader("Size", #bm_file.size),
                                               sortheader("From/To", 4),
                                               #tableheader{text="Linked Message"},
                                               sortheader("Date", #bm_file.time),
                                               sortheader("Status", #bm_file.status)
                                                ]},
                lists:map(fun(F) ->
                            #file_row{file=F}
                          end,
                          Files)
                ]}

            ]}.    

sortheader(Label, Sortby) -> % {{{1
    Sort = wf:state_default(sortby, #bm_file.path),
    #tableheader{body=[
        #link{text=Label, postback={sort, Sortby}},
        if Sortby == Sort ->
               " <i class='icon icon-caret-down'></i>";
           true ->
               " <i class='icon icon-sort'></i>"
        end
    ]}.


event(archive) ->  % {{{1
    Files = sets:to_list(wf:session_default(attached_files, sets:new())),
    db:archive(Files),
    wf:replace(body, body(false));
event({show_archive, true}) ->  % {{{1
    wf:state(archive, true),
    wf:replace(archive, #link{id=archive, body="<i class='icon-list-alt'></i> Actual", postback={show_archive, false}}),
    wf:replace(body, body(true));
event({show_archive, false}) ->  % {{{1
    wf:state(archive, false),
    wf:replace(archive, #link{id=archive, body="<i class='icon-list-alt'></i> Archive", postback={show_archive, true}}),
    wf:replace(body, body(false));
event({check, FID, true}) ->  % {{{1
    AF = wf:session_default(attached_files, sets:new()),
    wf:session(attached_files,  sets:del_element(FID, AF));
event({check, FID, false}) ->  % {{{1
    AF = wf:session_default(attached_files, sets:new()),
    wf:session(attached_files,  sets:add_element(FID, AF));
event({sort, Sortby}) -> % {{{1
    wf:state(sortby, Sortby),
    Archive = wf:state_default(archive, false),
    wf:replace(body, body(Archive));
event(Click) ->  % {{{1
    io:format("~p~n",[Click]).

incoming() ->  % {{{1
    wf:replace(body, body()).
