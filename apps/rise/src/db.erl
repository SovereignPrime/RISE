-module(db).
-compile([export_all]).
-include("db.hrl").
-include("protokol.hrl").
-include_lib("bitmessage/include/bm.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% Simple shortcut to verify either the mnesia table  was properly set up or
%% already exists
-define(V(Response), verify_create_table(Response)).

install(Pid)->  % {{{1
    create_tables(),
    timer:sleep(60000),
    receiver:register_receiver(Pid),
    bitmessage:generate_address(),
    bitmessage:subscribe_broadcast(<<"BM-2DBJhZLvR1rwhD6rgzseiedKASEoNVCA6Q">>),
    bitmessage:subscribe_broadcast(<<"BM-2D7M95NtnPRHskBgj1Ru6XvgmCw6WXuuSY">>).

create_tables() -> % {{{1
    ?V(mnesia:create_table(db_group, [{disc_copies, [node()]}, {attributes, record_info(fields, db_group)}, {type, ordered_set}, {index, [name]}])),
    ?V(mnesia:create_table(db_contact, [{disc_copies, [node()]}, {attributes, record_info(fields, db_contact)}, {type, ordered_set}, {index, [address]}])),
    ?V(mnesia:create_table(db_task, [{disc_copies, [node()]}, {attributes, record_info(fields, db_task)}, {type, ordered_set}])),
    ?V(mnesia:create_table(db_expense, [{disc_copies, [node()]}, {attributes, record_info(fields, db_expense)}, {type, ordered_set}])),
    ?V(mnesia:create_table(db_search, [{disc_copies, [node()]}, {attributes, record_info(fields, db_search)}])),
    ?V(mnesia:create_table(db_contact_roles, [{disc_copies, [node()]}, {attributes, record_info(fields, db_contact_roles)}, {type, ordered_set}])),
    ?V(mnesia:create_table(db_group_members, [{disc_copies, [node()]}, {attributes, record_info(fields, db_group_members)}, {type, bag}])),
    ?V(mnesia:create_table(db_expense_tasks, [{disc_copies, [node()]}, {attributes, record_info(fields, db_expense_tasks)}, {type, bag}])),
    ?V(mnesia:create_table(db_task_tree, [{disc_copies, [node()]}, {attributes, record_info(fields, db_task_tree)}, {type, bag}, {index, [parent, visible]}])),
    ?V(mnesia:create_table(db_contact_note, [{disc_copies, [node()]}, {attributes, record_info(fields, db_contact_note)}, {type, ordered_set}, {index, [contact]}])).
    
account(Pid, Address) ->  % {{{1
            {ok, U} = db:create_account("", true, Address),
            Pid ! accepted.

update() ->  % {{{1
    ok=case mnesia:create_table(db_version, [{disc_copies, [node()]}, {attributes, record_info(fields, db_version)}, {type, ordered_set}]) of
        {atomic, ok} ->
           register_vsn({0, 0, 0}),
           ok;
        _ -> ok
    end,
    create_tables(),
    {ok, VSNStr} = application:get_key(rise, vsn),
    VSNList = string:tokens(VSNStr, "."),
    {Maj, Mid, Min} = VSN = list_to_tuple(lists:map(fun list_to_integer/1, VSNList)),
    case mnesia:dirty_read(db_version, mnesia:dirty_last(db_version)) of
        [#db_version{version=VSN}] ->
            ok;
        [#db_version{version=LVSN,
                     schema=Schema,
                     update_fun=UFun}] ->
            ok=update(VSN),
            register_vsn(VSN)
    end.
        



update({0, 1, 5}) ->  % {{{1
    [ update(X) || X <- lists:seq(1, 6)],
    update_table(db_task,
                 [{due, fun(D) -> sugar:date_from_string(D) end},
                  {recurring, undefined},
                  {effort, {1.0, weeks}},
                  {sort, undefined}
                 ],
                 record_info(fields, db_task));
update(_) ->
    ok.

update_table(Table, NewData, NewAttrs) ->  % {{{1
    case mnesia:transform_table(Table,
                                fun(Old) ->
                                        OldL = tl(tuple_to_list(Old)),
                                        OldAttr = mnesia:table_info(Table, attributes),
                                        OldProp = lists:zip(OldAttr, OldL),
                                        NewL = lists:foldr(
                                                 fun(K, A) ->
                                                         OldV = proplists:get_value(K, OldProp, undefined),
                                                         V = proplists:get_value(K, NewData, OldV),
                                                         case is_function(V) of
                                                             true ->
                                                                 N = V(OldV),
                                                                 [N | A];
                                                             _ ->
                                                                 [V | A]
                                                         end
                                                 end,
                                                 [],
                                                 NewAttrs),
                                        list_to_tuple([Table | NewL])
                                end,
                                NewAttrs) of
        {atomic, ok} ->
            ok;
        Any -> Any
    end.
                           







qlc_result(QH) ->  % {{{1
    transaction(fun() ->
                        qlc:e(QH)
                end).

%%%
%% Get new id fot Type
%%%

next_id(Type) ->  % {{{1
    transaction(fun() ->
                case mnesia:last(Type) of
                    '$end_of_table' ->
                        1;
                    A -> 
                        A+1
                end
        end).
%%%
%% General routines
%%
save(Contact) ->  % {{{1
    transaction(fun() ->
                mnesia:write(Contact)
        end).

save_uniq(Contact) ->  % {{{1
    transaction(fun() ->
                case mnesia:match_object(Contact) of
                    [_] ->
                        ok;
                    [] ->
                        mnesia:write(Contact)
                end
        end).

delete(Type, Id) ->  % {{{1
    transaction(fun() ->
                mnesia:delete({Type, Id})
        end).

archive(Rec) ->  % {{{1
    set_archive(Rec, true).

set_archive(Rec, false) when is_record(Rec, db_task) ->  % {{{1
    transaction(fun() ->
                R = Rec#db_task{status=new},
                mnesia:write(R),
                R
        end);
set_archive(Rec, true) when is_record(Rec, db_task) ->  % {{{1
    transaction(fun() ->
                R = Rec#db_task{status=archive},
                mnesia:write(R),
                R
        end);
set_archive(Rec, IsArchive) when is_record(Rec, message) ->  % {{{1
    transaction(fun() ->
                        #message{hash=Id} = Rec,
                         [R]  = mnesia:wread({message, Id}),
                         RN = R#message{status=archive},
                         mnesia:write(message, RN, write),
                         RN
                end);
set_archive(#db_contact{address=Address}, IsArchive) ->  % {{{1
    transaction(fun() ->
                [R] = mnesia:index_read(db_contact, Address, #db_contact.address),
                mnesia:write(R#db_contact{status=archive}),
                Updates = mnesia:select(message,
                                        [{#message{status='$1',
                                                   folder=incoming,
                                                   from=Address,
                                                   _='_'},
                                          [{'/=', '$1', archive}],
                                          ['$_']}]),
                lists:foreach(fun(M) ->
                                      db:save(M#message{status=archive})
                              end,
                              Updates),
                {ok, Tasks} = get_tasks_by_user(R#db_contact.id)
                %lists:foreach(fun(#db_task{id=TID}=T) ->
                %                      case length(get_involved(TID)) of
                %                          1 ->
                %                              save(T#db_task{status=archive});
                %                          _ ->
                %                              ok
                %                      end
                %              end,
                %              Tasks)
        end);
set_archive(Rec, IsArchive) when is_list(Rec) ->  % {{{1
    transaction(fun() ->
                iterate(bm_file, Rec, fun(_T, R) ->
                            [R1] = mnesia:wread({bm_file, R}),
                            mnesia:write(R1#bm_file{status=archive}),
                            [R1]
                    end)
        end);
set_archive(Rec, IsArchive) when is_record(Rec, db_expense) ->  % {{{1
    transaction(fun() ->
                mnesia:write(Rec#db_expense{status=archive})
        end).


search_groups([]) ->  % {{{1
    {ok, []};
search_groups(Term) ->  % {{{1
    transaction(fun() ->
                        Tab = mnesia:table(db_group),
                        QH = qlc:q([G || G <- Tab, 
                                         re:run(G#db_group.name, Term, [caseless]) /= nomatch]),
                        qlc:e(QH)
                end).

search_contacts("") ->  % {{{1
    {ok, []};
search_contacts(Term) ->  % {{{1
    transaction(fun() ->
                        Tab = mnesia:table(db_contact),
                        QH = qlc:q([G || G <- Tab, 
                                         G#db_contact.status /= archive,
                                         (re:run(G#db_contact.name ++ G#db_contact.email, Term, [caseless]) /= nomatch orelse wf:to_binary(Term) == G#db_contact.address)]),
                        qlc:e(QH)
                end).

search_files("") ->  % {{{1
    {ok, []};
search_files(Terms) ->  % {{{1
    search:check_roles(Terms,
                       fun() -> {ok, []} end,
            fun() ->
                Term = search:get_term(Terms),
                transaction(
                  fun() ->
                      UReq = create_contacts_request(Terms),
                      DReq = create_dates_request(Terms),
                      Request = UReq ++ DReq,

                      Tab = mnesia:table(bm_file,
                                         [{traverse,
                                           {select,
                                            [{#bm_file{time='$3',
                                                       _='_'},
                                              DReq,
                                              ['$_']}]}}]),
                      QH = qlc:q([G || G <- Tab,
                                       G#bm_file.status /= archive,
                                       re:run(G#bm_file.name,
                                              Term,
                                              [caseless]) /= nomatch]),

                      qlc:e(QH)
                  end)
            end).

search_messages("") ->  % {{{1
    {ok, []};
search_messages(Terms) ->  % {{{1
    Term = search:get_term(Terms),
    SearchText = fun(G, Text) -> 
                         re:run(wf:to_list(G#message.subject) ++ wf:to_list(Text),
                                Term,
                                [caseless]) /= nomatch
                 end,
    transaction(
      fun() ->
              UReq = create_contacts_request(Terms),
              DReq = create_dates_request(Terms),
              Msg = mnesia:table(message,
                                 [{traverse,
                                   {select,
                                    [{#message{from='$1',
                                               to='$2',
                                               time='$3',
                                               _='_'},
                                      UReq ++ DReq,
                                      ['$_']}]}}]),
              QH = qlc:q([G || G <- Msg,
                               G#message.status /= archive,
                               try binary_to_term(G#message.text) of
                                   #{type := message,
                                     text := T} ->
                                       check_roles(Terms,
                                                   fun() ->
                                                           SearchText(G, T) 
                                                   end);
                                   #{type := task,
                                     text := T,
                                     involved := I,
                                     due := D,
                                     status := S} ->
                                       search:check_roles(Terms,
                                                          fun() ->
                                                                  check_due(D, Terms) 
                                                                  and
                                                                  check_status(S, Terms)
                                                                  and
                                                                  search_roles(I, Terms) 
                                                                  and
                                                                  SearchText(G, T)
                                                                  
                                                          end,
                                                          fun() ->
                                                                  SearchText(G, T)
                                                          end);
                                   _ ->
                                       false
                               catch 
                                   error:badarg ->
                                       false
                               end]),
                              qlc:e(QH)
            end).


search_tasks("") ->  % {{{1
    {ok, []};
search_tasks(Terms) ->  % {{{1
    {ok, Messages} = search_messages(Terms),
    {ok, lists:foldl(fun(#message{text=Data}, A) ->
                        try binary_to_term(Data) of
                            #{type := task,
                              id := Id} ->
                                {ok, T} = db:get_task(Id),
                                A ++ T;
                            _ ->
                                A
                        catch 
                            error:badarg ->
                                A
                        end
                end, 
                [],
               Messages)}.
get_filters() ->  % {{{1
    transaction(fun() ->
                        mnesia:select(db_search, [{#db_search{_='_'}, [], ['$_']}])
                end).
%%%%
%%% Task routines
%%%%
%
%
save_subtask(Id, PId, Time) ->  % {{{1
    transaction(fun() ->
                        Tree = mnesia:select(db_task_tree, [{#db_task_tree{task=Id, time='$1', _='_'}, [{'>', '$1', Time}], ['$_']}]),
                        case Tree of
                            [] ->
                                [ C ] = mnesia:wread({ db_task, Id }),
                                mnesia:write(C#db_task{parent=PId});
                            _ ->
                                ok
                        end,
                        mnesia:write(#db_task_tree{task=Id, parent=PId, time=Time})
                end).

get_task() ->  % {{{1
    transaction(fun() ->
                        mnesia:read(db_task, mnesia:last(db_task))
                end).

get_task(Id) ->   % {{{1
    transaction(fun() ->
                        mnesia:read(db_task, Id)
                end).

get_task_history(Hash) ->  % {{{1
    transaction(fun() ->
                        mnesia:foldr(fun(#message{hash=Id, text=D, status=S}=Msg, A) when S /= archive ->
                                             try binary_to_term(D) of
                                                 #{type := task,
                                                   id := Hash}=Task ->
                                                     A ++ [Msg];
                                                 _ ->
                                                     A
                                             catch
                                                 error:_ ->
                                                     A
                                             end;
                                        (_, A) ->
                                             A
                                     end, [], message)
                end).

get_task_comments(Hash) ->  % {{{1
    transaction(fun() ->
                        mnesia:foldr(fun(#message{hash=Id, text=D, status=S}=Msg, A) when S /= archive ->
                                             try binary_to_term(D) of
                                                 #{type := task_comment,
                                                   task := Hash}=Task ->
                                                     A ++ [Msg];
                                                 _ ->
                                                     A
                                             catch
                                                 error:_ ->
                                                     A
                                             end;
                                        (_, A) ->
                                             A
                                     end, [], message)
                end).

get_tasks_by_user(UID) ->  % {{{1
    get_tasks_by_user(UID, '_').

get_tasks_by_user(UID, Role) ->  % {{{1
    transaction(fun() ->
                        Tasks = mnesia:match_object(#db_contact_roles{contact=UID,
                                                                      type=db_task,
                                                                      role=Role,
                                                                      _='_'}),

                        iterate(db_task,
                                Tasks,
                                fun(_, #db_contact_roles{tid=I, role=Role}) ->
                                        case mnesia:read(db_task, I) of
                                            [Task] ->
                                                [ Task#db_task{parent=Role} ];
                                            [] ->
                                                []
                                        end
                                end)
                end).

archive_op(true) -> '==';  % {{{1
archive_op(false) -> '/='.  % {{{1

get_tasks(Parent) when not is_boolean(Parent) ->  % {{{1
    get_tasks(Parent, false);
get_tasks(Archive) ->  % {{{1
    ArchOp = archive_op(Archive),
    transaction(fun() ->
                        mnesia:select(db_task, [{#db_task{status='$1', _='_'}, [{ArchOp, '$1', 'archive'}], ['$_']}])
                end).

get_tasks(Parent, Archive) ->  % {{{1
    ArchOp = archive_op(Archive),
    transaction(fun() ->
                        mnesia:select(db_task, [{#db_task{parent=Parent, status='$1', _='_'}, [{ArchOp, '$1', 'archive'}], ['$_']}])
                end).

get_tasks_due_today(Archive) ->  % {{{1
    Today = date(),
    error_logger:info_msg("searching: ~p",[Today]),
    ArchOp = archive_op(Archive),
    transaction(fun() ->
                        mnesia:select(db_task, [{#db_task{status='$1', due=Today, _='_'}, [{ArchOp, '$1', 'archive'}], ['$_']}])
                end).

get_tasks_by_month(Y, M) ->  % {{{1
    transaction(fun() ->
                        mnesia:select(db_task, [{#db_task{status='$1', due={{Y, M, '_'}, '_'}, _='_'}, [{'/=', '$1', 'archive'}], ['$_']}])
                end).

get_tasks_overdue(Archive) ->  % {{{1
    Today = sugar:date_format(date()),
    error_logger:info_msg("searching: ~p ~p",[Archive, Today]),
    ArchOp = archive_op(Archive),
    transaction(fun() ->
                        mnesia:select(db_task,
                                      [{#db_task{status='$1',
                                                 due='$2',
                                                 _='_'},
                                        [{ArchOp, '$1', 'archive'},
                                         {'<', '$2', Today},
                                        {'=/=', '$2', ""}],
                                        ['$_']}])
                end).

get_tasks_no_deadline(Archive) ->
    ArchOp = archive_op(Archive),
    transaction(fun() ->
                        mnesia:select(db_task, [{#db_task{status='$1', due="", _='_'}, [{ArchOp, '$1', 'archive'}], ['$_']}])
                end).

get_tasks_completed(_Archive) ->
    transaction(fun() ->
                        mnesia:select(db_task, [{#db_task{status=complete,  _='_'}, [], ['$_']}])
                end).

are_all_child_tasks_complete(Taskid) ->
    {ok, Tasks} = get_tasks(Taskid, false),
    lists:all(fun(Task) ->
                      Task#db_task.status=:=complete andalso
                      are_all_child_tasks_complete(Task#db_task.id)
              end, Tasks).

get_orphan_tasks(Archive) ->
    {ok, Tasks} = get_tasks(Archive),
    Taskids = [T#db_task.id || T <- Tasks],
    Orphans = [T || T <- Tasks,
                    T#db_task.parent=/=undefined,
                    not(lists:member(T#db_task.parent, Taskids))],
    {ok, Orphans}.

get_tasks_by_subject(Subject, false) ->  % {{{1
    transaction(fun() ->
                        mnesia:select(db_task, [{#db_task{name=Subject, status='$1', _='_'}, [{'/=', '$1', 'archive'}], ['$_']}])
                end);
get_tasks_by_subject(Subject, true) ->  % {{{1
    transaction(fun() ->
                        mnesia:select(db_task, [{#db_task{name=Subject, status='$1', _='_'}, [{'==', '$1', 'archive'}], ['$_']}])
                end).
save_task_tree(Id, Parent) ->  % {{{1
    transaction(fun() ->
                        TT = #db_task_tree{task=Id, parent=Parent},
                        case mnesia:read(db_task, Id) of
                            [] ->
                                mnesia:write(TT#db_task_tree{visible=false});
                            [_] ->
                                mnesia:write(TT#db_task_tree{visible=true})
                        end
                end).

search_parent(Id, PId) ->  % {{{1
    transaction(fun() ->
                        search_parent_rec(Id, PId)
                end).

get_children(UID, Time) ->  % {{{1
    transaction(fun() ->
                        CS = mnesia:select(db_task_tree, [{#db_task_tree{parent=UID,time='$1', _='_'}, [{'>', '$1', Time}], ['$_']}]),
                        CSS = lists:sort(fun(#db_task_tree{time=A}, #db_task_tree{time=B}) -> A > B end, CS),
                        lists:foldl(fun(#db_task_tree{task=T}, A) ->
                                            case lists:keymember(T, 2, A) of
                                                true ->
                                                    A;
                                                false ->
                                                    [T|A]
                                            end
                                    end, [], CSS)
                end). 

task_status_list() ->  % {{{1
    task_status_list(false).

task_status_list(WhthValue) ->  % {{{1
    [{new, "New"},
     {accepted, "Accepted"},
     {in_progress, "In Progress"},
     {complete, "Complete"}] ++
     if WhthValue ->
            [
             {paied, "Paied"}
            ];
        true ->
            []
     end ++
     [{archive, "Archived"}].

nice_task_status_name(changed) ->  % {{{1
    "Changed";
nice_task_status_name(Status) ->  % {{{1
    proplists:get_value(Status, task_status_list()).

sanitize_task_status(Status) when is_list(Status) ->  % {{{1
    try 
        S = list_to_existing_atom(Status),
        {S, _} = lists:keyfind(S, 1, task_status_list()),
        S
    catch
        Class:Error ->
            error_logger:warning_msg("Invalid task status: ~p~nStacktrace: ~p",[Status, erlang:get_stacktrace()]),
            new %% if task status is a fail, generate error message and use new
    end.



%%%
%% Expense routines
%%%

get_expense(Id) ->  % {{{1
    transaction(fun() ->
                        mnesia:read(db_expense, Id)
                end).

get_expense_tasks(EId) ->  % {{{1
    transaction(fun() ->
                        T = mnesia:read(db_expense_tasks, EId),
                        iterate(db_task, T, fun(Type, #db_expense_tasks{task=Id}) ->
                                                    mnesia:read(Type, Id)
                                            end)
                end).
%%%
%% Updates routines
%%%
get_update(Id) ->   % {{{1
    transaction(fun() ->
                        [U] = mnesia:read(message, Id),
                        U
                end).
get_updates(Archive) ->  % {{{1
    ArchOp = archive_op(Archive),
    transaction(fun() ->
                        mnesia:select(message,
                                      [{#message{status='$1',
                                                 subject='$3',
                                                 _='_'},
                                        [{'and',
                                          {ArchOp, '$1', archive},
                                          {'/=', '$3', <<"$Task tree$">>},
                                          {'/=', '$3', <<"$Get vCard$">>},
                                          {'/=', '$3', <<"$vCard$">>},
                                          {'/=', '$3', <<"$Update223322$">>}
                                         }],
                                        ['$_']}])
                end).

get_updates_by_thread(Thread) ->  % {{{1
    get_updates_by_thread(Thread, false).

get_updates_by_thread(new, Archive) ->  % {{{1
    {ok, []};
get_updates_by_thread(undefined, Archive) ->  % {{{1
    get_updates(Archive);
get_updates_by_thread(Thread, Archive) ->  % {{{1
    ArchOp = archive_op(Archive),
    transaction(fun() ->
                        Messages = mnesia:select(message,
                                                [{#message{status='$1',
                                                           _='_'},
                                                  [{ArchOp, '$1', archive}],
                                                  ['$_']}]),
                        lists:filter(fun(#message{hash=T}) when T == Thread-> true;
                                        (#message{text=Data}) ->
                                             case receiver:extract_packet(Data) of
                                                 #{thread := T} when T == Thread->
                                                     true;
                                                 _ -> false
                                             end;
                                        (_) -> false
                                     end,
                                     Messages)
                end).

get_updates_by_user(UID) when is_list(UID) ->   % {{{1
    get_updates_by_user(list_to_binary(UID)); 
get_updates_by_user(UID) ->   % {{{1
    transaction(fun() ->
                        mnesia:select(message,
                                      [{#message{status='$1',
                                                 enc='$2',
                                                 folder=incoming,
                                                 from=UID,
                                                 _='_'},
                                        [{'/=', '$1', archive},
                                         {'or', 
                                          {'==', '$2', 3},
                                          {'==', '$2', 2}
                                         }],
                                        ['$_']}])
                end).

get_unread_updates() ->  % {{{1
    transaction(fun() ->
                        mnesia:select(message,
                                      [{#message{status=unread,
                                                 folder=incoming,
                                                 subject='$3',
                                                 _='_'},
                                        [{'and',
                                          {'/=', '$3', <<"$Task tree$">>},
                                          {'/=', '$3', <<"$Get vCard$">>},
                                          {'/=', '$3', <<"$vCard$">>},
                                          {'/=', '$3', <<"$Update223322$">>}
                                         }],
                                        ['$_']}])
                end).

get_unread_ids() -> % {{{1
    Ms = get_unread_updates(),
    [M#message.hash || M <- Ms].

get_status(message, Id) ->  % {{{1
    transaction(fun() ->
                        case  mnesia:read({message, Id}) of
                            [#message{status=S}] ->
                                S;
                            _ -> unknown
                        end
                end).

set_read(Id) ->  % {{{1
    transaction(fun() ->
                        case  mnesia:wread({message, Id}) of
                            [#message{status=S,
                                      folder=incoming} = U] ->
                                mnesia:write(message,
                                             U#message{status=read},
                                             write),
                                S;
                            _ ->
                                read
                        end
                end).

%%%
%% Contact routines
%%%

get_contact(undefined) ->  % {{{1
    {ok, Id} = next_id(db_contact),
    {ok, #db_contact{id=Id}};
get_contact(Id) ->  % {{{1
    transaction(fun() ->
                        [U] = mnesia:read(db_contact, Id),
                        U
                end).

get_contact_by_address(Address) when is_list(Address) ->  % {{{1
    get_contact_by_address(list_to_binary(string:strip(Address)));
get_contact_by_address(Address) ->  % {{{1
    transaction(fun() ->
                        case mnesia:index_read(db_contact, Address, #db_contact.address) of
                            L when length(L)>=1 ->
                                case coalesce_best_contact(L, false) of
                                    none -> coalesce_best_contact(L, true);
                                    U -> U
                                end;
                            _ ->
                                none
                        end
                end).

-spec coalesce_best_contact([#db_contact{}], boolean()) -> none | #db_contact{}.
%% @doc Returns the first contact it comes across that has an actual name, and
%% whether or not it's marked as Archived
coalesce_best_contact([User], Archive) when (User#db_contact.status==archive)==Archive ->  % {{{1
    User;
coalesce_best_contact([User | Rest], Archive) when (User#db_contact.status==archive)==Archive ->  % {{{1
    case User#db_contact.name of
        "unknown" -> coalesce_best_contact(Rest, Archive);
        "" -> coalesce_best_contact(Rest, Archive);
        _ -> User
    end;
coalesce_best_contact([User | Rest], Archive) ->  % {{{1 
    %% It is needed when contact is archived, but messages not
    case User#db_contact.name of
        "unknown" -> coalesce_best_contact(Rest, Archive);
        "" -> coalesce_best_contact(Rest, Archive);
        _ -> User
    end.


get_involved(Id) ->  % {{{1
    transaction(fun() ->
                        R = mnesia:match_object(#db_contact_roles{type=db_task,
                                                                  tid=Id,
                                                                  _='_'}),

                        lists:map(fun(#db_contact_roles{role=Role,
                                                        contact=Contact}) ->
                                          [ #db_contact{name=Name,
                                                        email=_Email}=C ] = mnesia:read(db_contact, Contact),
                                          {Name, Role, C}
                                  end, R)
                end).

get_involved_full(Id) -> % {{{1
    transaction(fun() ->
                        R = mnesia:match_object(#db_contact_roles{type=db_task,
                                                                  tid=Id,
                                                                  _='_'}),

                        lists:map(fun(ContactRole = #db_contact_roles{contact=Contact}) ->
                                          [ #db_contact{name=Name}] = mnesia:read(db_contact, Contact),
                                          {ContactRole, Name}
                                  end, R)
                end).

clear_involved(#db_task{id=Id}) ->  % {{{1
    transaction(fun() ->
                        R = mnesia:match_object(#db_contact_roles{type=db_task,
                                                                  tid=Id,
                                                                  _='_'}),

                        lists:foreach(fun(O) ->
                                              mnesia:delete_object(O)
                                      end,
                                      R)
                end).

get_users(N) ->  % {{{1
    transaction(fun() ->
                        mnesia:select(db_contact, [{#db_contact{name='$1'}, [], ['$1']}], N, read)
                end).

add_user_to_group(Group, User) ->  % {{{1
    transaction(fun() ->
                        mnesia:write(#db_group_members{group=Group, contact=User})
                end).

get_contacts_by_group(Group) ->  % {{{1
    get_contacts_by_group(Group, false).
get_contacts_by_group(my, false) -> % {{{1
    transaction(fun() -> 
                        mnesia:match_object(#db_contact{my=true, _='_'})
                end);
get_contacts_by_group(all, false) ->  % {{{1
    transaction(fun() ->
                        mnesia:select(db_contact, [{#db_contact{my=false,status='$1', _='_'}, [{'/=', '$1', archive}],['$_']}])
                end);
get_contacts_by_group(all, true) ->  % {{{1
    transaction(fun() ->
                        mnesia:select(db_contact, [{#db_contact{my=false,status='$1', _='_'}, [{'==', '$1', archive}],['$_']}])
                end);
get_contacts_by_group(Group, false) ->  % {{{1
    transaction(fun() ->
                        G = mnesia:select(db_group, [{#db_group{id='$1', subgroups=Group, _='_'}, [], ['$1']}]),
                        U = iterate(db_group_members, [Group | G]),
                        io:format("~p ~p ~n", [G, U]),
                        iterate(db_contact, U, fun(Type, #db_group_members{contact=Id}) ->
                                                       case mnesia:read(Type, Id) of
                                                           [#db_contact{status=archive} = C] -> 
                                                               [];
                                                           C ->
                                                               C
                                                       end
                                               end)
                end);
get_contacts_by_group(Group, true) ->  % {{{1
    transaction(fun() ->
                        G = mnesia:select(db_group, [{#db_group{id='$1', subgroups=Group, _='_'}, [], ['$1']}]),
                        U = iterate(db_group_members, [Group | G]),
                        io:format("~p ~p ~n", [G, U]),
                        iterate(db_contact, U, fun(Type, #db_group_members{contact=Id}) ->
                                                       case mnesia:read(Type, Id) of
                                                           [#db_contact{status=archive} = C] -> 
                                                               [C];
                                                           _->
                                                               []
                                                       end
                                               end)
                end).

get_groups_for_user(UID) ->  % {{{1
    transaction(fun() ->
                        GIDS = mnesia:select(db_group_members, [{#db_group_members{group='$1', contact=UID}, [], ['$1']}]),
                        iterate(db_group, GIDS)
                end).

get_notes_by_user(UID) ->  % {{{1
    transaction(fun() ->
                        mnesia:index_read(db_contact_note, UID, #db_contact_note.contact)
                end).

clear_roles(Type, Id) ->  % {{{1
    transaction(fun() ->
                        case mnesia:match_object(#db_contact_roles{type=Type, tid=Id, _='_'}) of
                            [] ->
                                ok;
                            R  ->
                                iterate(db_contact_roles, R, fun(T, R) ->
                                                                     mnesia:delete_object(R),
                                                                     []
                                                             end)
                        end
                end).

get_contacts_by_name(Name) ->  % {{{1
    transaction(fun() ->
                        [C] = mnesia:select(db_contact, [{#db_contact{name=Name, status='$1', _='_'}, [{'/=', '$1', archive}], ['$_']}]),
                        C
                end).
backup(#db_contact{address=Address} = Cotact) ->  % {{{1
    transaction(fun() ->
                        mnesia:select(db_contact, [{#db_contact{status='$1', _='_'}, [{'/=', '$1', 'archive'}], ['$_']}]) ++
                        mnesia:match_object(#privkey{address=Address, _='_'}) ++
                        mnesia:select(message,
                                      [{#message{from=Address,
                                                 status='$1',
                                                 _='_'},
                                        [{'/=', '$1', 'archive'}],
                                        ['$_']}])
                        %mnesia:match_object(#db_task{to=Address, _='_'}) ++
                end).
restore(Privkey, Contacts, Messages) ->  % {{{1
    transaction(fun() ->
                        ok=mnesia:write(privkey, Privkey, write),
                        lists:foreach(fun(Contact) ->
                                              mnesia:write(db_contact, Contact, write)
                                      end, Contacts),
                        [ #db_contact{bitmessage=MyAddress}] = mnesia:match_object(#db_contact{my=true, _='_'}),
                        lists:foreach(fun(#message{from=F} = Msg) when F == MyAddress ->
                                              ok=mnesia:write(message, Msg, write);
                                         (#message{to=F} = Msg) when MyAddress == F ->    
                                              ok=mnesia:write(message, Msg, write)
                                      end, Messages),
                        MyAddress
                end).

%%%
%% Group routines
%%%

get_groups() ->  % {{{1
    transaction(fun() ->
                        get_subgroup(undefined)
                end).

update_group_name(Id, Name) ->  % {{{1
    transaction(fun() ->
                        [G] = mnesia:read(db_group, Id),
                        mnesia:write(G#db_group{id=Id, name=Name})
                end).
save_subgroup(Id, Parent) ->  % {{{1
    transaction(fun() ->
                        [G] = mnesia:wread({db_group, Id}),
                        mnesia:write(G#db_group{subgroups=Parent})
                end).
delete_group(Id) ->  % {{{1
    transaction(fun() ->
                        Sub = mnesia:match_object(#db_group{subgroups=Id, _='_'}),
                        iterate(db_group, Sub, fun(_Type, G) ->
                                                       mnesia:write(G#db_group{subgroups=undefined}),
                                                       []
                                               end),
                        mnesia:delete({db_group_members, Id}),
                        mnesia:delete({db_group, Id})
                end).
%%%
%% File routines
%%%

save_file(Path, #db_contact{id=UID}) ->  % {{{1
    Size = filelib:file_size(Path),
    Type = filename:extension(Path),
    <<FID:64/bytes, _/bytes>> = bm_message_encryptor:process_attachment(Path),
    FID.

%%%
%% Attachment routines
%%%

get_attachments(Record) ->  % {{{1
    Type = element(1, Record),
    Id = element(2, Record),
    transaction(fun() ->
                        A = mnesia:select(db_attachment,
                                          [{#db_attachment{ file='$1',
                                                            type=Type,
                                                            tid=Id,
                                                            _='_'},
                                            [],
                                            ['$1']}]),
                        iterate(bm_file, A)
                end).

save_attachments(Record, Files) when is_tuple(Record) ->  % {{{1
    Type = element(1, Record),
    Id = element(2, Record),
    save_attachments(#{type => Type, id => Id}, Files);
save_attachments(#{type := Type, id := Id}, Files) ->  % {{{1
    transaction(fun() ->
                        {ok, NId} = db:next_id(db_attachment),
                        save_attachment(Type, Id, sets:to_list(Files), NId)
                end).

get_files(FIDs) when is_list(FIDs) ->  % {{{1
    transaction(fun() ->
                        iterate(bm_file, FIDs)
                end);
get_files(true)  ->  % {{{1
    transaction(fun() ->
                        mnesia:select(bm_file, [{#bm_file{status=archive, _='_'}, [], ['$_']}])
                end);
get_files(false)  ->  % {{{1
    transaction(fun() ->
                        mnesia:select(bm_file, [{#bm_file{status='$1', _='_'}, [{'/=', '$1', archive}], ['$_']}])
                end).

get_addresat(FID) ->  % {{{1
    transaction(fun() ->
                        Attachments = mnesia:read(db_attachment, FID),
                        iterate(db_contact, Attachments, fun(_, #db_attachment{type=Type, tid=TID}) ->
                                                                 [#db_contact{email=Email}] = mnesia:read(Type, TID),
                                                                 Email
                                                         end)
                end).


get_linked_messages(FID) when is_list(FID) ->  % {{{1
    get_linked_messages(wf:to_binary(FID));
get_linked_messages(FID) ->  % {{{1
    transaction(fun() ->
                        Messages = mnesia:table(message, {traverse,
                                                          {select,
                                                          [{
                                                            #message{status='$1',
                                                                     _='_'},
                                                            [{'/=', '$1', archive}],
                                                            ['$_']
                                                           }]
                                                         
                                                          }}),
                        qlc:e(qlc:q([ M || #message{attachments=As}=M <- Messages,
                                           As /= [],
                                           sets:is_element(FID, sets:from_list(As))
                                    ]))
                end).


%%%
%%  Admin functions
%%%

all_tasks() ->  % {{{1
    transaction(fun() ->
                        mnesia:match_object(#db_task{_='_'})
                end).
all_expenses() ->  % {{{1
    transaction(fun() ->
                        mnesia:match_object(#db_expense{_='_'})
                end).

all_updates() ->  % {{{1
    transaction(fun() ->
                        mnesia:match_object(#db_update{_='_'})
                end).

all_expense_taskss() ->  % {{{1
    transaction(fun() ->
                        mnesia:match_object(#db_expense_tasks{_='_'})
                end).
all_memberss() ->  % {{{1
    transaction(fun() ->
                        mnesia:match_object(#db_group_members{_='_'})
                end).

all_attachments() ->  % {{{1
    transaction(fun() ->
                        mnesia:match_object(#db_attachment{_='_'})
                end).

all_files() ->  % {{{1
    transaction(fun() ->
                        mnesia:match_object(#db_file{_='_'})
                end).

all_groups() ->  % {{{1
    transaction(fun() ->
                        mnesia:match_object(#db_group{_='_'})
                end).

all_contacts() ->  % {{{1
    transaction(fun() ->
                        mnesia:match_object(#db_contact{_='_'})
                end).

all_involved() ->  % {{{1
    transaction(fun() ->
                        mnesia:match_object(#db_contact_roles{_='_'})
                end).
%%%
%% Account routines
%%%

get_my_accounts() ->  % {{{1
    transaction(fun() ->
                        mnesia:match_object(db_contact, #db_contact{my=true, _='_'}, read)
                end).

create_account(User, Passwd, Address) ->  % {{{1
    transaction(fun() ->
                        case mnesia:match_object(db_contact, #db_contact{email=User, my=Passwd, _='_'}, write) of
                            [] -> 
                                N = case mnesia:table_info(db_task, size) of
                                        '$end_of_table' ->
                                            0;
                                        A -> 
                                            A
                                    end,
                                mnesia:write(#db_contact{id=N+1,
                                                         name="Me",
                                                         email=User,
                                                         address=Address,
                                                         bitmessage=Address,
                                                         my=Passwd
                                                        }),
                                [U] = mnesia:read(db_contact, N+1),
                                U;
                            [U] ->
                                U
                        end
                end).

%%%
%% Transaction helper
%%%

transaction(Fun) ->  % {{{1
    case mnesia:transaction(Fun) of
        {aborted, R} ->
            {error, R};
        {atomic, '$end_of_table'} ->
            {ok, [], undefined};
        {atomic, R} ->
            {ok, R}
    end.
%%%
%% Helpers
%%%

save_attachment(_Type, _Id, [], _N) ->  % {{{1
    ok;
save_attachment(Type, Id, [File|Rest], N) ->  % {{{1
    case mnesia:match_object(#db_attachment{file=File, type=Type, tid=Id, _='_'}) of
        [] ->
            mnesia:write(#db_attachment{id=N, file=File, type=Type, tid=Id}),
            save_attachment(Type, Id, Rest, N+1);
        _ ->
            save_attachment(Type, Id, Rest, N)
    end.

iterate(_, []) ->  % {{{1
    [];
iterate(Type, [Id|R]) ->  % {{{1
    mnesia:read(Type, Id) ++ iterate(Type, R).

iterate(_, [], _) ->  % {{{1
    [];
iterate(Type, [Id|R], Fun) ->  % {{{1
    Fun(Type, Id) ++ iterate(Type, R, Fun).

get_subgroup(G) ->  % {{{1
    Groups = mnesia:match_object(#db_group{subgroups=G, _='_'}),
    Sub = lists:map(fun(#db_group{id=N}=Gr) ->
                            Gr#db_group{subgroups=get_subgroup(N)}
                    end, Groups).

search_parent_rec(Id, PId) ->  % {{{1
    case mnesia:read(db_task, PId) of
        [] ->
            case mnesia:read(db_task_tree, PId) of
                [] ->
                    undefined;
                P ->
                    #db_task_tree{parent=PPId} = hd(lists:sort(fun(#db_task_tree{time=A}, #db_task_tree{time=B}) -> A > B end, P)),
                    search_parent_rec(Id, PPId)
            end;
        [#db_task{id=PId}] ->
            PId
    end.

verify_create_table({atomic, ok}) -> ok;  % {{{1
verify_create_table({aborted, {already_exists, _Table}}) -> ok. % {{{1

filter_by_date({0, M, D}, TTL, A) -> % {{{1
    case sugar:timestamp_to_datetime(TTL) of
        {{_, M1, D1}= DT, _} when M1 == M; D1 == D ->
            A ++ [DT];
        _ ->
            A
    end;
filter_by_date({Y, 0, 0}, TTL, A) -> % {{{1
    case sugar:timestamp_to_datetime(TTL) of
        {{Y1, _, _}= DT, _} when Y1 == Y ->
            A ++ [DT];
        _ ->
            A
    end;
filter_by_date(Date, TTL, A) ->  % {{{1
    case sugar:timestamp_to_datetime(TTL) of
        {Date, _} ->
            error_logger:info_msg("Dates: ~p~n", [Date]),
            A ++ [Date];
        _ ->
            A
    end;
filter_by_date(_,_, A) ->  % {{{1
    A.

search_dates(Date) ->  % {{{1
    transaction(fun() ->
                        Msg = mnesia:foldl(fun(#message{time=TTL, 
                                                       status=Status},
                                               A) when Status /= archive -> 
                                                   TS = sugar:ttl_to_timestamp(TTL),
                                                   filter_by_date(Date,
                                                                  TS,
                                                                  A);
                                              (_, A) -> A
                                           end,
                                           [],
                                           message),
                        TasksH =mnesia:foldl(fun(#db_task{due=Due, 
                                                          status=Status},
                                                 A) when Status /= archive, 
                                                         Due /= "" -> 
                                                     TS = sugar:datetime_to_timestamp(Due),
                                                     filter_by_date(Date,
                                                                    TS,
                                                                    A);
                                              (_, A) -> A
                                             end,
                                             [],
                                             db_task),
                        FilesH = mnesia:foldl(fun(#bm_file{time=TS, 
                                                          status=Status},
                                                 A) when Status /= archive -> 
                                                     filter_by_date(Date,
                                                                    TS,
                                                                    A);
                                              (_, A) -> A
                                             end,
                                             [],
                                             bm_file),
                        lists:usort(Msg ++ TasksH ++ FilesH)

                end).

check_roles(Terms, Fun) ->  % {{{1
    search:check_roles(Terms, fun() -> false  end, Fun).


create_contacts_request(Terms) ->  % {{{1
    case {proplists:get_value("Group", Terms, error),
          proplists:get_value("Contact", Terms, error)} of
        {error, error} ->
            [];
        {G, error} ->
            [#db_group{id=GID}] = mnesia:index_read(db_group,
                                                    G,
                                                    #db_group.name),
            case lists:map(
                   fun(
                     #db_group_members{contact=UID}) ->
                           [#db_contact{address=A}] = mnesia:read(db_contact, UID),
                           {'orelse',
                            {'==', '$1', A},
                            {'==', '$2', A}}
                   end,
                   mnesia:read(db_group_members, GID)) of
                [] ->
                    [];
                L ->
                    [list_to_tuple(['orelse' | L])]
            end;
        {error, U} ->
            {ok, #db_contact{address=A}} = get_contacts_by_name(U),
            [{'orelse',
              {'==', '$2', A},
              {'==', '$1', A}}];
        _ ->
            []
    end.

create_dates_request(Terms) -> % {{{1
    case {proplists:get_value("Daterange", Terms, error),
          proplists:get_value("Date", Terms, error),
          proplists:get_value("Due", Terms, error),
          proplists:get_value("Duerange", Terms, error)} of
        {error, error, error, error} -> [];
        {{SD1, ED1}, _, error, error} ->
            [{'andalso', 
              {'>=', '$3', sugar:ttl_from_string(SD1)},
              {'<', '$3', sugar:ttl_from_string(ED1) + 24 * 3600}}];
        {D, _, error, error} when is_list(D) ->
            [SD1, ED1] = string:tokens(D, " "),
            [{'andalso', 
              {'>=', '$3', sugar:ttl_from_string(SD1)},
              {'<', '$3', sugar:ttl_from_string(ED1) + 24 * 3600}}];
        {error, D1, error, error} ->
            [{'andalso', 
              {'>=', '$3', sugar:ttl_from_string(D1)},
              {'<', '$3', sugar:ttl_from_string(D1) + 24 * 3600 }}];
        _ -> []
    end.

check_due(D, Terms) ->  % {{{1
    case {proplists:get_value("Due", Terms, error),
          proplists:get_value("Duerange", Terms, error)} of
        {error, error} ->
            true;
        {error, {SDate, EDate}} ->
            Date = sugar:timestamp_from_string(D),
            Date >= sugar:timestamp_from_string(SDate) andalso Date =< sugar:timestamp_from_string(EDate);
        {error, Duerange} ->
            [SDate, EDate] = string:tokens(Duerange, " "),
            Date = sugar:timestamp_from_string(D),
            Date >= sugar:timestamp_from_string(SDate) andalso Date =< sugar:timestamp_from_string(EDate);
        {Due, _} ->
            Due == sugar:date_string(D)
    end.
            
check_status(S, Terms) ->  % {{{1
    case proplists:get_value("Status", Terms, error) of
        error ->
            true;
        Status ->
            case lists:keyfind(Status, 2, task_status_list(true)) of
             {StatusA, Status} ->
                    StatusA == S;
                _ ->
                    false
            end
    end.

search_roles(Involved, Terms) ->  % {{{1
    error_logger:info_msg("Involved: ~p~n", [Involved]),
    IsRoleFiltered = lists:any(fun({Role, _}) ->
                                       proplists:is_defined(Role, Terms)
                               end,
                               ?ROLES),
    IsInRole = lists:any(fun(#{type := role,
                               role := R,
                               address := A}) ->
                                 {value, {Role, _R}, _} = lists:keytake(R, 2, ?ROLES),
                                 case proplists:get_value(Role, Terms, error) of
                                     error ->
                                         wf:info("Role: ~p", [Role]),
                                         false;
                                     Name ->
                                         case get_contact_by_address(A) of
                                             {ok, #db_contact{name=N}} when N == Name ->
                                                 wf:info("Role1: ~p", [Role]),
                                                 true;
                                             T ->
                                                 wf:info("Role2: ~p", [T]),
                                                 false
                                         end
                                 end
                         end,
                         Involved),
    wf:info("Filtered: ~p In role: ~p", [IsRoleFiltered, IsInRole]),
    (not IsRoleFiltered) or IsInRole.

register_vsn(VSN) ->  % {{{1
    Tables = mnesia:system_info(local_tables),
    Schema = lists:map(fun(T) ->
                               {T, mnesia:table_info(T, attributes)}
                       end,
                       Tables),
    transaction(fun() ->
                        mnesia:write(#db_version{version=VSN,
                                                 schema=Schema})
                end).


