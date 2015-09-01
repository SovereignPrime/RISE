-module(common).
-compile([export_all]).
-include_lib("bitmessage/include/bm.hrl").
-include("records.hrl").
-include("db.hrl").
-include("protokol.hrl").

main() ->  % {{{1
    PWD = os:getenv("ROOTDIR"),
    Timeout = application:get_env(nitrogen, db_timeout, 300),
    case mnesia:wait_for_tables([db_group], Timeout) of
        ok ->
            case wf:user() of
                'undefined' ->
                    case db:get_my_accounts() of 
                        {ok, []} ->
                            wf:redirect("/legal");
                        {ok, [U]} ->
                            db:update(),
                            wf:user(U),
                            main()
                    end;
                R ->
                    {ok, Pid} = wf:comet_global(fun  incoming/0, incoming),
                    receiver:register_receiver(Pid),
                    Online = bitmessage:online(),
                    Pid! {status, Online},
                    T = #template {file=PWD ++ "/site/templates/bare.html" },
                    wf:wire(backup_path,
                            #event{type=change,
                                   postback=backup_download,
                                   delegate=?MODULE}),
                    wf:wire(logs_path,
                            #event{type=change,
                                   postback=logs_download,
                                   delegate=?MODULE}),
                    wf:wire('to_files',
                            #event{type=click,
                                   postback={to_files, undefined},
                                   delegate=?MODULE}),
                    wf:wire('to_relationships',
                            #event{type=click,
                                   postback={to_group, undefined},
                                   delegate=?MODULE}),
                    wf:wire('to_tasks',
                            #event{type=click,
                                   postback={to_task, undefined},
                                   delegate=?MODULE}),
                    wf:wire('to_expenses',
                            #event{type=click,
                                   postback={to_expense, undefined},
                                   delegate=?MODULE}),
                    wf:wire('to_updates',
                            #event{type=click,
                                   postback={to_message, undefined},
                                   delegate=?MODULE}),
                    wf:wire('new_contact',
                            #event{type=click,
                                   postback=add_contact,
                                   delegate=?MODULE}),
                    wf:wire('new_group',
                            #event{type=click,
                                   postback=add_group,
                                   delegate=?MODULE}),
                    wf:wire('new_task',
                            #event{type=click,
                                   postback=add_task,
                                   delegate=?MODULE}),
                    wf:wire('new_expense',
                            #event{type=click,
                                   postback=add_expense,
                                   delegate=?MODULE}),
                    wf:wire('new_update',
                            #event{type=click,
                                   postback=add_update,
                                   delegate=?MODULE}),
                    wf:wire(#event{type=timer,
                                   delay=600000,
                                   actions=#script{
                                              script="location.reload();"
                                             }
                                  }),
                    T
            end;
        {timeout, _} ->
            wf:redirect("/legal")
    end.

unread() -> % {{{1
    {ok, New} = db:get_unread_updates(),
    #span{id=count, class='label label-inverse',text=wf:f("~p new", [length(New)])}.

connection_status(N) when N > 0, % {{{1
                          N /= undefined ->
    "<script type='text/javascript'>" ++
    "$('.tooltip').remove();" ++
    "</script>" ++
    "<i class='icon icon-circle'></i> net" ++
    "<script type='text/javascript'>" ++
    "$('.wfid_connection').tooltip({placement: 'right'});" ++
    "</script>";
connection_status(_N) -> % {{{1
    "<script type='text/javascript'>" ++
    "$('.tooltip').remove();" ++
    "</script>" ++
    "<i class='icon icon-circle-blank'></i> net"++
    "<script type='text/javascript'>" ++
    "$('.wfid_connection').tooltip({placement: 'right'});" ++
    "</script>".

search() -> %{{{1
    Terms = wf:session_default(filter, []),
    Bs = lists:map(fun(In) ->
                           search:get_badge_for_type(In)
                   end,
                   Terms),
    #sigma_search{id=search,
                  tag=search, 
                  placeholder="Search", 
                  class="input-append input-prepend input-block-level search", 
                  textbox_class="",
                  search_button_class="btn btn-inverse search-btn", 
                  search_button_text="<i class='icon icon-search'></i>",
                  x_button_class="search-x",
                  badges=lists:flatten(Bs),
                  clear_button_class="pull-right btn btn-inverse",
                  clear_button_text="<i class='icon icon-remove'></i>",
                  results_summary_class="search-results span10",
                  delegate=?MODULE}.

fade() ->  % {{{1
    #panel{id=fade, 
           style="display:none"}.

render_files() -> % {{{1
    AttachmentsIDs = sets:to_list(wf:session_default(attached_files, sets:new())),
    {ok, Attachments} = db:get_files(AttachmentsIDs),
    #panel{id=files,
           class="span12",
           body=[
                 #panel{ class="row-fluid",
                         body=[
                               "<i class='icon-file-alt'></i> Attachments",
                               #br{},
                               #rise_upload{id=attachments,
                                            tag=filename,
                                            delegate=common,
                                            droppable_text="Drag and drop files here"
                                           },
                               #link{body="<i class='icon-th-large'></i> Select from my files",
                                     postback=add_file,
                                     new=false}
                              ]},
                 #br{},
                 lists:map(fun(#bm_file{path=Path,
                                        name=FName,
                                        size=Size,
                                        time=Timestamp,
                                        hash=Id,
                                        status=Status}) ->
                                   DateTime = sugar:timestamp_to_datetime(Timestamp),
                                   #attachment{fid=Id,
                                               filename=Path ++ "/" ++ FName,
                                               size=Size,
                                               time=DateTime,
                                               status=Status}
                           end,
                           Attachments)
                ]}.

sigma_search_event(to, Terms) -> % {{{1
    {NTerms, Results} = search:contacts(Terms),
    Bs = lists:map(fun({"Term", [$B, $M, $- | _] = Address}) when length(Address) > 35 ->
                           wf:info("Address: ~p", [Address]),
                           #db_contact{address=My} = wf:user(),
                           BAddress = wf:to_binary(Address),
                           CID = receiver:get_or_request_contact(BAddress, My, BAddress),
                           {ok, #db_contact{name=Name}} = db:get_contact(CID),
                           [search:simple_badge({"Contact", Name}, ["Contact"]),
                           search:simple_badge({"Term", ""}, ["Term"])];
                      ({"Term", _}=In) ->
                           wf:info("Term: ~p", [In]),
                           search:simple_badge(In, []);
                      (In) ->
                           wf:info("In: ~p", [In]),
                           search:simple_badge(In, ["Contact"])
                   end,
                   NTerms),
    {lists:flatten(Bs),
                  Results
                };
sigma_search_event(involved, Terms) -> % {{{1
    {NTerms, Results} = search:contacts(Terms),
    Involved = wf:state_default(involved, []),
    {InvolvedN, Bs} = lists:foldl(fun({"Term", _}, A) ->
                                        A;
                                   ({Role, Name}=In, {I, B}) ->
                                        %{ok, #db_contact{id=CID}} = db:get_contacts_by_name(Name),
                                        ContactRole = case lists:keysearch(Name, 2, Involved) of
                                                          false ->
                                                              {#db_contact_roles{id=new,
                                                                                 role=Role},
                                                                                 %contact=CID},
                                                               Name};
                                                          {value, {CR, Name}} when CR#db_contact_roles.role /= Role ->
                                                              {CR#db_contact_roles{role=Role}, Name};
                                                          {value, CR} -> CR
                                                      end,


                                        {[ContactRole | I],
                                         [search:simple_badge(In, [ R || {_, R} <- ?ROLES]) | B]}
                                  end,
                                  {[], []},
                                  NTerms),
    wf:state(involved, InvolvedN),
    tasks:maybe_show_top_buttons(),
    {lists:flatten(Bs),
     case proplists:get_value("Term", NTerms) of
         undefined ->
             [];
         "" ->
             [];
         _ ->
             Results
     end};
sigma_search_event(search, Terms) -> % {{{1
    {NTerms, Results} = search:terms(Terms),
    Bs = lists:map(fun(In) ->
                           search:get_badge_for_type(In)
                   end,
                   NTerms),
    {lists:flatten(Bs),
     #panel{class="",
            body=[
                  Results,
                  #panel{body=#link{body="<i class='icon icon-filter'></i> Create filter with search",
                                    postback={save_filter_name, NTerms},
                                    delegate=?MODULE}}
                ]}}.  
sigma_search_filter_event(to, Terms) ->  % {{{1
    Subject = wf:q(name),
    Text = wf:q(text),
    Update = wf:session_default(current_update, #{}),
    #db_contact{address=My} = wf:user(),
    Involved = lists:foldl(fun({"Term", _}, A) ->
                                   A;
                              ({"Contact", Name}, A) ->
                                   error_logger:info_msg("Adding contact ~p~n", [Name]),
                                   case db:get_contacts_by_name(Name) of
                                       {ok, #db_contact{address=Addr}} ->
                                           [Addr | A];
                                       _ ->
                                           A
                                   end;
                              (_, A) -> A
                           end,
                           [],
                           Terms),

    io:format("~p~n", [Involved]),
    NUpdate = Update#{type => message,
                      subject => Subject,
                      text => Text,
                      from => My, 
                      to => Involved,
                      status => new},
    %db:save_attachments(NUpdate, wf:session_default(attached_files, sets:new())),
    common:send_messages(NUpdate),
    wf:session(current_update, NUpdate),
    wf:redirect("/");
sigma_search_filter_event(search, Terms) ->  % {{{1
    wf:session(filter, Terms),
    search:check_roles(Terms,
                       fun() ->
                               wf:redirect("/tasks")
                       end,
                       fun() -> 
                               wf:redirect("/")
                       end).
                               
sigma_search_filter_clear() ->  % {{{1
    wf:session(filter, undefined),
    wf:session(filter_name, undefined),
    wf:replace(filters, render_filters()),
    wf:replace(left, (wf:page_module()):left()).

render_filters() -> %{{{1
    case wf:session(filter_name) of
        undefined ->
            render_filters(nothing);
        F ->
            wf:wire(#script{script="$('.sigma_search_textbox').keydown()"}),
            wf:wire(#event{type=timer,
                           delay=300,
                           actions=#script{script="$('.sigma_search_results').hide()"}}),
            render_filters(F)
    end.
render_filters(Chosen) -> %{{{1
    {ok, Filters} = db:get_filters(),
    #panel{id=filters,
           class="btn-group",
           body=[
            #link{class=["btn",
                         "dropdown-toggle", 
                         "btn-link"],
                  style=case Chosen of
                             nothing ->
                                 "";
                             _ ->
                                 "color:#fff;background-color:#000;"
                         end, 
                  body=case Chosen of
                           nothing ->
                               "<i class='icon-filter'></i> Smart filter";
                           _ ->
                               "<i class='icon-filter'></i> " ++ Chosen
                       end,
                  data_fields=[{toggle, "dropdown"}],
                  url="#",
                  new=false},
            #list{numbered=false,
                  class="dropdown-menu",
                  body=lists:map(fun(#db_search{name=Name, text=Terms}) ->
                                         #listitem{ class="",
                                                    body=[
                                                          #link{class="pull-left", 
                                                                style="width:70%",
                                                                body=[#span{body=Name,
                                                                            actions=#event{type=mouseup,
                                                                                           postback={filter_load, Name, Terms},
                                                                                           delegate=?MODULE}
                                                                           },
                                                                      #span{class="pull-right",
                                                                            body="<i class='icon icon-remove'></i>",
                                                                            actions=#event{type=mouseup,
                                                                                           postback={filter_delete, Name},
                                                                                           delegate=?MODULE}
                                                                           }]}
                                                         ]}
                                 end, Filters)
                 }
                ]}.

render_help() ->  % {{{1
    #panel{ class='btn-group', body=[
        #link{class="btn dropdown-toggle btn-link", body="<i class='icon-question'></i> Help", data_fields=[{toggle, "dropdown"}], url="#", new=false},
        #list{numbered=false, class="dropdown-menu",body=[
			#listitem{ class="", body=[
				#link{text="Download logs", postback=logs, delegate=?MODULE}
			]},
            #listitem{body=[
				#email_link{text="For support: support@sovereignprime.com", email="support@sovereignprime.com"}
			]}
        ]}
    ]}.
	
settings_menu() -> %{{{1
    #panel{ class="btn-group", body=[
		#link{class="btn dropdown-toggle btn-link",
              body=[
                    #image{class="icon",
                           style="height:20px;vertical-align:middle;margin-top:-5px;",
                           image = "/img/id_card.svg"},
                    " My Profile"
                   ],
              data_fields=[{toggle, "dropdown"}],
              url="#", new=false},
		#list{numbered=false, class="dropdown-menu",body=[
			#listitem{ class="", body=[
				#link{text="View My Profile (and RISE ID)", postback=my_profile, delegate=?MODULE}
			]},
			#listitem{ class="", body=[
				#link{text="Wipe peers", postback=wrap_peers, delegate=?MODULE}
			]},
			#listitem{ class="", body=[
				#link{text="Backup user", postback=backup, delegate=?MODULE}
			]},
			#listitem{ class="", body=[
				#link{text="Restore user", postback=restore, delegate=?MODULE}
			]}
		]}
	]}.

event(my_profile) -> % {{{1
    maybe_unsaved(fun() ->
                      User = #db_contact{id=Id} = wf:user(),
                      wf:session(current_contact, User),
                      wf:session(current_contact_id,Id),
                      wf:redirect("/relationships")
              end);

event(add_group) -> %{{{1
    maybe_unsaved(fun() ->
                      {ok, Id} = db:next_id(db_group),
                      db:save(#db_group{
                                 id=Id,
                                 name="New group",
                                 subgroups=undefined
                                }),
                      wf:redirect("/relationships")
              end);
event(add_contact) -> %{{{1
    maybe_unsaved(fun() ->
                      {ok, Id} = db:next_id(db_contact),
                      wf:session(current_contact_id, Id),
                      Contact = #db_contact{
                                   id=Id,
                                   name="Contact Name"
                                  },
                      db:save(Contact),
                      wf:session(current_contact, Contact),
                      wf:redirect("/relationships")
              end);
event(add_task) -> %{{{1
    maybe_unsaved(fun() ->
                      wf:session(current_task, undefined),
                      wf:session(attached_files, sets:new()),
                      wf:redirect("/edit_task")
              end);
event(add_expense) -> %{{{1
    maybe_unsaved(fun() ->
                      {ok, Id} = db:next_id(db_expense),
                      wf:session(current_expense_id, Id),
                      wf:session(current_expense, #db_expense{id=Id}),
                      wf:session(attached_files, sets:new()),
                      wf:redirect("/edit_expense")
              end);
event(add_update) -> %{{{1
    maybe_unsaved(fun() ->
                      wf:session(current_update,
                                 #{type => message,
                                   id => new}),
                      wf:session(attached_files, sets:new()),
                      wf:redirect("/edit_update")
              end);
event(check_all) -> %{{{1
    case wf:q(check_all) of
        "on" ->
            wf:replace(check, #checkbox{id=check,  postback=check_all, checked=true, delegate=common});
        undefined ->
            wf:replace(check, #checkbox{id=check,  postback=check_all, checked=false, delegate=common})
    end;
event({db_contact, Id}) -> %{{{1
    wf:session(current_contact_id, Id),
    wf:redirect("/relationships");
event({db_update, Id}) -> %{{{1
    wf:session(current_subject, Id),
    wf:redirect("/");
event({db_task, Id}) -> %{{{1
    wf:session(current_task_id, Id),
    {ok, [ Task ]} = db:get_task(Id),
    wf:session(current_task, Task),
    wf:redirect("/tasks");
event({db_file, Id}) -> %{{{1
    wf:redirect("/files");
event({search, Term}) -> %{{{1
    wf:set(".sigma_search_textbox", Term),
    wf:wire(#script{script="$('.sigma_search_textbox').keydown()"});
event({save_filter_name, Terms}) -> %{{{1
    wf:insert_bottom("body",
                     #popup{id=save_filter_name,
                            header="Save filter name...",
                            body=#panel{style="align:center;",
                                        class="input-append",
                                        body=[
                                              #textbox{id=filter_name}, 
                                              #button{id=ok, 
                                                      class="btn btn-link",
                                                      body=[
                                                            "<i class='icon icon-ok'></i>"
                                                           ],
                                                      postback={save_filter, Terms},
                                                      delegate=?MODULE
                                                     }
                                             ]}
                                  }),
    wf:wire(#event{target=save_filter_name,
                   postback={show, save_filter_name},
                   delegate=element_popup});

event({save_filter, Terms}) -> %{{{1
    Name = wf:q(filter_name),
    db:save(#db_search{text=Terms, name=Name}),
    wf:replace(filters, render_filters()),
    wf:wire(#event{postback={close, save_filter_name}, delegate=element_popup}),
    wf:wire(#script{script="$('.sigma_search_x_button').click()"});
event({filter_delete, Name}) ->  % {{{1
    db:delete(db_search, Name),
    sigma_search_filter_clear(),
    wf:replace(filters, render_filters());
event({filter_load, Name, Terms}) ->  % {{{1
    wf:session(filter_name, Name),
    sigma_search_filter_event(search, Terms);
    
event({reply, UID, Packet}) -> % {{{1
    Reply = maps:with([type, thread, to, subject], Packet),
    #db_contact{address=My} = wf:user(),
    From = maps:get(from, Packet),
    To = maps:get(to, Packet),
    Thread = maps:get(thread, Packet, UID),
    wf:session(current_update, Reply#{type => message,
                                      thread => Thread,
                                      to => [From | To] -- [My]}),
    wf:session(attached_files, undefined),
    wf:redirect("/edit_update");

event({to_message, undefined}) ->  % {{{1
    maybe_unsaved(fun() ->
                          wf:redirect("/")
                  end);

event({to_message, #message{text=Data}=Message}) ->  % {{{1
    maybe_unsaved(fun() ->
                          wf:info("UID: ~p~n", [Message]),
                          Update = receiver:extract_packet(Data),
                          wf:session(current_update, Update),
                          wf:redirect("/")
                  end);

event({to_contact, ID}) ->  % {{{1
    maybe_unsaved(fun() ->
                          wf:session(current_contact_id, ID),
                          wf:redirect("/relationships")
                  end);

event({to_group, ID}) ->  % {{{1
    maybe_unsaved(fun() ->
                          wf:session(current_group_id, ID),
                          wf:redirect("/relationships")
                  end);

event({to_files, _ID}) ->  % {{{1
    maybe_unsaved(fun() ->
                          wf:redirect("/files")
    end);

event({to_task, undefined}) -> % {{{1
    maybe_unsaved(fun() ->
                          wf:redirect("/tasks")
    end);

event({to_task, Id}) -> % {{{1
    maybe_unsaved(fun() ->
                          wf:session(current_task_id, Id),
                          {ok, [ Task ]} = db:get_task(Id),
                          wf:session(current_task, Task),
                          wf:redirect("/tasks")
    end);

event(wrap_peers) -> %{{{1
    mnesia:clear_table(addr);
event(backup) -> %{{{1
    wf:wire(#script{script="init_download('" ++ wf:to_list(backup_path) ++ "')"}),
    wf:redirect("/raw?id=backup.rz&file=RISE_BACKUP_" ++ sugar:date_string(date()) ++ ".rz");
event(backup_download) ->  % {{{1
    FD = wf:q(backup_path),
    error_logger:info_msg("Backup to file: ~s", [FD]),
    mnesia:backup(wf:f("~s", [FD]));
event(logs) -> %{{{1
    wf:wire(#script{script="init_download('" ++ wf:to_list(logs_path) ++ "')"}),
    wf:redirect("/raw?id=backup.rz&file=RISE_LOGS_" ++ sugar:date_string(date()) ++ ".tar.gz");
event(logs_download) ->  % {{{1
    FD = wf:q(logs_path),
    error_logger:info_msg("Logs to file: ~s", [FD]),
    WorkDir = application:get_env(nitrogen, workdir, "workdir"),
    Path = WorkDir ++ "/log",
    error_logger:info_msg("Logs from file: ~s", [Path]),
    erl_tar:create(FD, [{"log", Path}], [compressed]);
event(cancel) -> %{{{1
    wf:wire(#script{script="$('.modal').modal('hide')"}),
    wf:remove(".modal");
event(restore) -> %{{{1
    wf:insert_bottom("body",
                     #panel{class="modal fade",
                            body=[
                                  #panel{class="modal-header",
                                         body=[
                                               #button{class="btn-link pull-right",
                                                       text="x",
                                                       postback=cancel,
                                                       delegate=?MODULE},
                                               #h3{text="Restore user"}
                                              ]},
                                  #panel{class="modal-body",
                                         body=[
                                               #rise_upload{id=restore,
                                                            tag=restore,
                                                            delegate=common,
                                                            droppable_text="Drag and drop backup file here"
                                                           }
                                              ]}
                                 ]}),
    wf:wire(#script{script="$('.modal').modal('show')"});
event({unfold, #update_element{id=Id}=Update}) -> % {{{1
    wf:replace(Id,
               Update#update_element{collapse=false});

event({fold, #update_element{id=Id}=Update}) -> % {{{1
    wf:replace(Id, Update#update_element{collapse=true});
event(E) -> %{{{1
    io:format("Event ~p occured in ~p~n", [E, ?MODULE]).

dropevent(A, P) -> %{{{1
    io:format("Drag ~p drop ~p~n", [A, P]).

start_upload_event(_) -> %{{{1
    ok.
finish_upload_event(restore, FPath) -> %{{{1
    FID = filename:basename(FPath),
    wf:info("FID: ~p", [FID]),
    common:restore(FID),
    timer:sleep(100),
    wf:redirect("/relationships");
finish_upload_event(filename, FPath) -> %{{{1
    io:format("File uploaded: ~p for ~p~n", [FPath, new]),
    User = wf:user(),
    File = db:save_file(FPath,User),
    AF = wf:session_default(attached_files, sets:new()),
    wf:session(attached_files, sets:add_element(File , AF)),
    wf:update(files, render_files()).

incoming() -> %{{{1
    receiver:register_receiver(self()),
    receive
        received ->
            wf:wire(#script{script="received();"}),
            wf:flush(),
            ?MODULE:incoming();
        sent ->
            wf:wire(#script{script="sent();"}),
            wf:flush(),
            ?MODULE:incoming();
        update ->
            (wf:page_module()):incoming(),
            wf:replace(count, unread()),
            wf:flush(),
            ?MODULE:incoming();
        {status, N} ->
            wf:update(connection, connection_status(N)),
            wf:flush(),
            ?MODULE:incoming()
    after
        1000 ->
            ?MODULE:incoming()
    end.

save_involved(Type, TId) -> %{{{1
    Involved = wf:qs(person),
    Role = wf:qs(responsible),
    io:format("~p ~p~n", [Involved, Role]),
    List = [ #db_contact_roles{type=Type,
                               tid=TId,
                               role=Role,
                               contact=Contact} || {Contact,
                                                    Role} <- lists:zip(Involved, Role),
                                                   Involved /= [[]],
                                                   Contact /= ""],
    db:clear_roles(Type, TId),
    lists:foreach(fun(#db_contact_roles{contact=C}=P) -> 
                {ok, NPId} = db:next_id(db_contact_roles),
                {ok, #db_contact{id=CID}}  = db:get_contacts_by_name(C),
                db:save(P#db_contact_roles{id=NPId, contact=CID})
        end, List).

send_messages(#task_comment{task=TID}=TP) -> % {{{1
    #db_task{id=TID, name=Subject} = wf:session(current_task),
    #db_contact{address=From} = wf:user(),
    {ok, Involved} = db:get_involved(TID),
    Contacts = [#{type => role,
                  address => C,
                  role => R} || {_, R, #db_contact{bitmessage=C}}  <- Involved],
    #db_contact{address=From} = wf:user(),
    lists:foreach(fun(#{type := role,
                        address := To}) when To /= From ->
                          bitmessage:send_message(From,
                                                  wf:to_binary(To), 
                                                  wf:to_binary(Subject), 
                                                  term_to_binary(TP));
                     (_) ->
                          ok
                  end,
                  Contacts);
send_messages(#{type :=  message, % {{{1
                from := From,
                subject := Subject,
                to := Contacts} = U) ->
    wf:info("Sending ~p~n", [U]),
    Attachments = sets:to_list(wf:session_default(attached_files, sets:new())),
    MSG = term_to_binary(U#{to => [From | Contacts],
                            time => bm_types:timestamp()}),

    lists:foreach(fun(To) ->
                          error_logger:info_msg("Message to ~s sent subject ~s~n",
                                                [To,
                                                 Subject]),

                          bitmessage:send_message(From,
                                                  wf:to_binary(To),
                                                  wf:to_binary(Subject),
                                                  MSG,
                                                  Attachments)
                  end,
                  Contacts);
send_messages(#db_task{id=UID, %{{{1
                       name=Subject,
                       text=Text,
                       due=Date,
                       parent=Parent,
                       status=Status,
                       changes=Changes} = U) ->
    {ok, Involved} = db:get_involved(UID),
    Contacts = [#{type => role,
                  address => C,
                  role => R} || {_, R, #db_contact{bitmessage=C}}  <- Involved],
    #db_contact{address=From} = wf:user(),
    Attachments = sets:to_list(wf:session_default(attached_files, sets:new())),
    lists:foreach(fun(#{type := role,
                        address := To}) when To /= From ->
                bitmessage:send_message(From,
                                        wf:to_binary(To), 
                                        wf:to_binary(Subject), 
                                        term_to_binary(#{type => task,
                                                         id => UID,
                                                         name => Subject,
                                                         due => Date,
                                                         text => Text,
                                                         parent => Parent,
                                                         status => Status,
                                                         involved => Contacts,
                                                         time => bm_types:timestamp(),
                                                         changes => Changes}),

                                        Attachments);
            (_) ->
                ok
        end, Contacts).

send_task_tree(Id, Parent, Time) -> %{{{1
    {ok, PInvolved} = db:get_involved(Parent),
    {ok, Involved } = db:get_involved(Id),
    PContact = sets:from_list(lists:map(fun({_, _, #db_contact{address=A}}) -> A end, PInvolved)),
    #db_contact{bitmessage=From} = wf:user(),
    lists:foreach(fun({_, _, #db_contact{bitmessage=To, my=false}}) ->
                          IsAdresat = sets:is_element(To, PContact),
                          if IsAdresat ->
                                  MSG = term_to_binary(#{type => task_tree,
                                                         task => Id,
                                                         parent => Parent,
                                                         time => Time}),
                                  bitmessage:send_message(From, wf:to_binary(To), <<"$Task tree$">>, MSG);
                              true -> ok
                          end;
                     (_) ->
                          ok
                  end, Involved).

restore(FID) -> %{{{1
    wf:info("FID1: ~p", [FID]),
    mnesia:restore(FID, [{clear_tables, mnesia:system_info(tables) -- [schema, addr]}, {skip_tables, [addr]}]),
    {ok, [Me]} = db:get_my_accounts(),
    wf:user(Me).

remove_duplicates(List) ->  % {{{1
	remove_duplicates(List, []).

remove_duplicates([], Acc) ->  % {{{1
	Acc;
remove_duplicates([H|T], Acc) ->  % {{{1
    case lists:member(H, Acc) of
        true -> remove_duplicates(T, Acc);
        false -> remove_duplicates(T, Acc ++ [H])
    end.

maybe_unsaved(Fun) ->  % {{{1
    case wf:state_default(unsaved, false) of
        true -> wf:wire(#confirm{text="The task is unsaved! Save it?",
                                 postback=save});
        false -> Fun()
    end.
