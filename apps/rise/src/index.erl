%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("bitmessage/include/bm.hrl").
-include("records.hrl").
-include("db.hrl").
-include("protokol.hrl").

main() -> common:main().

title() -> "Welcome to RISE".

icon() -> "<i class='icon-globe icon-2x' style='margin-top:-5px;'></i>".

buttons(main) -> % {{{1
    #list{class="nav nav-pills", style="display:inline-block;", numbered=false,
          body=[
                #listitem{body=[
                                common:render_filters()
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

left() -> % {{{1
    left(false).

left(Archive) -> % {{{1
    Updates = case wf:session(filter) of
                  undefined ->
                      db:get_updates(Archive);
                  F ->
                      db:search_messages(F)
    end,
    case Updates  of 
        {ok, none} ->
            render_left([]);
        {ok, Upd} ->
            render_left(Upd)
    end.

render_left(Updates) -> % {{{1
    SortedUpdates = sugar:sort_by_timestamp(Updates),
    GroupedUpdates = group_updates(SortedUpdates),
    maybe_update_current(SortedUpdates, 
                         fun([]) -> [];
                            (_) ->
                                 Render = [#update_preview{message=M,
                                                           flag=true,
                                                           archive = (Status == archive)} || 
                                           {T, [#message{status=Status}=M|_]} <- GroupedUpdates
                                           
                                          ],
                                 #panel{id=left,
                                        class="span4 scrollable",
                                        body=Render}
                         end).

group_updates(List) ->  % {{{1
    lists:foldr(fun(#message{hash=Hash, text=Data}=M, Threads) ->
                        Packet = receiver:extract_packet(Data),
                        Thread = maps:get(thread, Packet, Hash),
                        case proplists:get_value(Thread, Threads) of
                            undefined ->
                                [{Thread, [M]} | Threads];
                            Ms -> 
                                [{Thread, [M | Ms]} | proplists:delete(Thread, Threads)]
                        end
                end,
                [],
                List).


body() -> % {{{1
    body(false).

body(Archive) -> % {{{1
    Thread = wf:session(current_thread),
    #panel{id=body,
           class="span8 scrollable",
           body=render_body(Thread, Archive)
          }.

render_body(Thread, Archive) -> % {{{1
    wf:session(current_thread, Thread),
    {ok, Updates} = db:get_updates_by_thread(Thread, Archive),
    maybe_update_current(Updates,
                         fun([]) -> [];
                            (_) ->
                                 U = wf:session(current_update),
                                 Type = maps:get(type, U, message),
                                 Icon = element_update_preview:render_icon(Type),
                                 CurrentId = maps:get(id, U, unknown),
                                 Subject = maps:get(subject, U),
                                 [
                                  #h1{body=[Icon," ",wf:html_encode(Subject)]},
                                  [
                                   #update_element{collapse=(Id /= CurrentId),
                                                   message = M} || #message{hash=Id} = M <- sugar:sort_by_timestamp(Updates)] 
                                 ]
                         end).

replace_left() -> % {{{1
    replace_left(left()).

replace_left(Body) -> % {{{1
    wf:wire("scrolltop_temp = objs('left').scrollTop()"),
    wf:replace(left, Body),
    wf:wire("objs('left').scrollTop(scrolltop_temp)").

event({selected, CurrentUpdate, Thread, Archive}) -> % {{{1
    wf:info("Thread: ~p", [Thread]),
    wf:session(current_thread, Thread),
    wf:session(current_update, CurrentUpdate),
    replace_left(left(Archive)),
    wf:update(body, render_body(Thread, Archive)),
    wf:wire("$(\".update-preview\").has(\"input[type=checkbox]:checked\").addClass(\"related-message\");"),
    wf:wire("$(\".update-preview\").has(\"input[type=checkbox]:not(:checked)\").removeClass(\"related-message\");");

event({archive, E, Rec}) -> % {{{1
    {ok, #message{subject=Subject}} = db:archive(#message{hash=Rec}),
    replace_left(),
    wf:update(body, render_body(Subject, false));

event({to_task, #message{from=From, subject=Subject, text=Data}}) -> % {{{1
    #db_contact{id=Me} = wf:user(),
    {ok, #db_contact{id=CID}} = db:get_contact_by_address(From),
    try binary_to_term(Data) of
        #{type := message,
          text := Text} ->
            ID = crypto:hash(sha512, <<Subject/bytes, (wf:to_binary(Text))/bytes>>),
            Task = #db_task{id=ID, name=Subject, text=Text},
            wf:state(current_task, Task),
            wf:session(current_task, Task),
            wf:state(current_task_id, ID),
            wf:session(current_task_id, ID),
            db:save(Task),
            tasks:save_contact_role(#db_contact_roles{id=new,
                                                      contact=CID,
                                                      role="concerning"}),
            tasks:save_contact_role(#db_contact_roles{id=new,
                                                      contact=Me,
                                                      role="responsible"}),
            wf:redirect("/tasks");
        _ ->
            ok
    catch
        error:_ ->
            ok
    end;
    
event({show_archive, true}) -> % {{{1
    wf:replace(archive, #link{id=archive, body="<i class='icon-list-alt'></i> Actual", postback={show_archive, false}}),
    {ok, Updates} = db:get_updates(true),
    #db_contact{id=My} = wf:user(),
    {ok, Tasks} = db:get_tasks(true),
    replace_left(render_left(Updates)),
    wf:replace(body, body(true));

event({show_archive, false}) -> % {{{1
    wf:replace(archive, #link{id=archive, body="<i class='icon-list-alt'></i> Archive", postback={show_archive, true}}),
    replace_left(),
    wf:replace(body, body());

event(Click) -> % {{{1
    io:format("Event ~p in ~p~n", [Click, ?MODULE]).

incoming() -> % {{{1
    replace_left().

maybe_update_current([], Fun) -> Fun([]);  % {{{1
maybe_update_current([#message{hash=FirstId,  % {{{1
                               text=Data}|_] = Messages,
                     Fun) ->
    U = receiver:extract_packet(Data),
    Thread = maps:get(thread, U, FirstId),
    CThread = wf:session(current_thread),
    if CThread /= Thread ->
           wf:session(current_update, U#{id => FirstId}),
           wf:session(current_thread,  Thread),
           Fun(Messages);
       true ->
           Fun(Messages)
    end.



maybe_update_session(K, Default) ->  % {{{1
    Current = wf:session_default(K, Default),
    wf:session(K, Current),
    Current.
