-module(search).
-compile([export_all]).

-include("db.hrl").
-include("protokol.hrl").
-include_lib("bitmessage/include/bm.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

dates_if(Terms) ->  % {{{1
    case proplists:is_defined("Daterange", Terms) or proplists:is_defined("Duerange", Terms) of
        true ->
            {Terms, []};
        false ->
            Term = get_term(Terms),
            case {proplists:get_value("Date", Terms, error),
                  proplists:get_value("Due", Terms, error)} of
                {error, error} ->
                    case  dates(Term) of
                        {[], Ts} ->
                            {Terms, Ts};
                        {Date, Ts} ->
                            {proplists:delete("Term", [{"Date", Term} | Terms]), Ts}
                    end;
                {Date, error} ->
                    case  dates(Term) of
                        {[], Ts} ->
                            {Terms, Ts};
                        {DateN, Ts} ->
                            ADict1 = proplists:delete("Date", Terms),
                            DT = sugar:date_from_string(Date),
                            case lists:usort([DT, DateN]) of
                                L when length(L) == 1->
                                    {proplists:delete("Term", [{"Date", Date} | ADict1]), []};
                                L ->
                                    {proplists:delete("Term", [{"Daterange", list_to_tuple(L)} | ADict1]), []}
                            end
                    end;
                {_, Date} ->
                    case  dates(Term) of
                        {[], Ts} ->
                            {Terms, Ts};
                        {DateN, Ts} ->
                            ADict1 = proplists:delete("Due", Terms),
                            DT = sugar:date_from_string(Date),
                            case lists:usort([DT, DateN]) of
                                L when length(L) == 1->
                                    {proplists:delete("Term", [{"Due", Date} | ADict1]), []};
                                L ->
                                    {proplists:delete("Term", [{"Duerange", list_to_tuple(L)} | ADict1]), []}
                            end
                    end
            end
    end.

dates(Term) when length(Term) == 1 ->  % {{{1
    NTerm = "0" ++ Term,
    dates(NTerm);
dates(Term) when length(Term) == 2 ->  % {{{1
    try
        T = wf:to_integer(Term),
        {ok, Dates} = db:search_dates({0, T, T}),
        {[], Dates}
    catch 
        error:badarg ->
            {[], []}
    end;
dates(Term) when length(Term) == 4 ->  % {{{1
    try
        T = wf:to_integer(Term),
        {ok, Dates}= db:search_dates({T, 0, 0}),
        {[], Dates}
    catch 
        error:badarg ->
            {[], []}
    end;
dates(Term) when length(Term) == 10 ->  % {{{1
    try
        Date = sugar:date_from_string(Term),
        {ok,  Dates} = db:search_dates(Date),
        {Date, []}
    catch 
        error:badarg ->
            {[], []}
    end;
dates(Term) ->  % {{{1
    {[], ""}.

contacts(Terms) ->  % {{{1
    Term = get_term(Terms),
    case proplists:is_defined("Group", Terms) of
        false ->
            {ok, Contacts} = db:search_contacts(Term),
            case Contacts of
                [] ->
                    {Terms, []};
                Contacts ->
                    case lists:keyfind(Term, #db_contact.name, Contacts) of
                        false ->
                            {Terms, #panel{body=["<dl class='dl-horizontal'>",
                                                 "<dt>Contacts:</dt><dd>",
                                                 lists:map(fun(#db_contact{id=Id,
                                                                           name=Name,
                                                                           email=Email}) ->
                                                                   #panel{body=[
                                                                                #link{text=wf:f("~s (~s)", [Name, Email]),
                                                                                      postback={search, Name},
                                                                                      delegate=common}
                                                                               ]}
                                                           end, Contacts),
                                                 "</dd>"]}};
                        #db_contact{name=Term} ->
                            {proplists:delete("Term", [{"Contact", Term} | Terms]), []}
                    end
            end;
        _ ->
            {Terms, []}
    end.

groups(Terms) ->  % {{{1
    Term = get_term(Terms),
    case {proplists:is_defined("Group", Terms), proplists:is_defined("Contact", Terms)} of
        {false, false} ->
            {ok, Groups} = db:search_groups(Term),
            case Groups of
                [] ->
                    {Terms, []};
                Groups ->
                    case lists:keyfind(Term, #db_group.name, Groups) of
                        false ->
                            {Terms, #panel{body=["<dl class='dl-horizontal'>",
                                 "<dt>Groups:</dt><dd>",
                                 lists:map(fun(#db_group{id=Id, name=Name}) ->
                                                   #panel{body=[
                                                                #link{text=Name,
                                                                      postback={search, Name},
                                                                      delegate=common}
                                                               ]}
                                           end, Groups),
                                 "</dd>"]}};
                        #db_group{name=Term} ->
                            {proplists:delete("Term", [{"Group", Term} | Terms]), []}
                    end
            end;
        _ ->
            {Terms, []}
    end.

statuses(Terms) ->  % {{{1
    Term = get_term(Terms),
    Statuses = db:task_status_list(true),
    case proplists:get_value("Status", Terms, error) of
        error ->
            case lists:keyfind(Term, 2, Statuses) of
                false ->
                    {Terms, format_dropdown_group("Statuses", 
                                                  Term,
                                                  lists:foldl(fun({_, Status}, A) ->
                                                                      case re:run(Status, Term, [global, caseless]) of
                                                                          nomatch ->
                                                                              A;
                                                                          _ ->
                                                                              [Status | A]
                                                                      end
                                                              end, 
                                                              [],
                                                              Statuses))};
                    _ ->
                            {proplists:delete("Term", [{"Status", Term} | Terms]), []}
            end;
        _ ->
            {Terms, []}
    end.


files(Files) ->  % {{{1
    case Files of
        [] ->
            [];
        Files ->
            ["<dl class='dl-horizontal'>",
                        "<dt>Files:</dt><dd>",
                        lists:map(fun(#bm_file{hash=Id,
                                               name=Name,
                                               time=Date,
                                               size=Size}) ->
                                          Users = element_file_row:get_file_senders(Id),
                                          #panel{body=[
                                                       #link{text=wf:f("~s (~s) - ~s - ~s", [
                                                                                   Name,
                                                                                   sugar:format_file_size(Size),
                                                                                   Users,
                                                                                   sugar:date_format(Date)
                                                                                  ]),
                                                             url="/files"}
                                                      ]}
                                  end, Files),
                        "</dd>"]
    end.

messages(Messages) ->  % {{{1
    M = lists:foldl(fun(#message{hash=Id, subject=Subject, from=FID, text=Data}, A) ->
                          {ok,
                           #db_contact{name=From}} = db:get_contact_by_address(FID),
                          try binary_to_term(Data) of
                              #message_packet{text=Text} ->
                                  A ++ [#panel{body=[
                                                     #link{text=wf:f("~s (~s) ~100s",
                                                                     [
                                                                      Subject,
                                                                      From,
                                                                      Text
                                                                     ]),
                                                           postback={to_message,
                                                                     Id},
                                                           delegate=common}
                                                    ]}];
                              _ ->
                                  A
                          catch
                              error:_ ->
                                  A
                          end
                    end,
                    [],
                    Messages),
    case M of
        [] ->
            [];
        M ->
            ["<dl class='dl-horizontal'>",
                        "<dt>Messages:</dt><dd>",
                        M,
                         "</dd>"]
    end.

tasks(Messages) ->  % {{{1
    error_logger:info_msg("Messages for tasks: ~p~n", [Messages]),
    Tasks = lists:foldl(fun(#message{hash=Id,
                                     subject=Subject,
                                     from=FID,
                                     text=Data}, A) ->
                                {ok,
                                 #db_contact{name=From}} = db:get_contact_by_address(FID),
                                try binary_to_term(Data) of
                                    #task_packet{id=ID, name=Subject, text=Text} ->
                                        A ++ [#panel{
                                                 body=[
                                                       #link{text=wf:f("~s - ~100s",
                                                                       [
                                                                        Subject,
                                                                        Text
                                                                       ]),
                                                             postback={to_task,
                                                                       ID},
                                                             delegate=common}
                                                      ]}];
                                    _ ->
                                        []
                                catch
                                    error:_ -> []
                                end
                        end,
                        [],
                        Messages),
    error_logger:info_msg("Tasks: ~p~n", [Tasks]),
    case Tasks of
        [] ->
            [];
        Tasks ->
            ["<dl class='dl-horizontal'>",
             "<dt>Tasks:</dt><dd>",
             Tasks,
             "</dd>"]
    end.

format_dates([]) ->  % {{{1
    "";
format_dates(Dates) ->  % {{{1
    ["<dl class='dl-horizontal'>",
                        "<dt>Dates:</dt><dd>",
                        lists:foldl(fun(Date, A) ->
                                    A ++ [#panel{body=[
                                            #link{text=sugar:date_string(Date),
                                                 postback={search, sugar:date_string(Date)},
                                                 delegate=common}
                                            ]}]
                            end, [], Dates),
                         "</dd>"].



terms(Terms) ->  % {{{1
    {GTerms, G} = search:groups(Terms),
    {CTerms, C} = search:contacts(GTerms),
    {STerms, S} = search:statuses(CTerms),
    {DTerms, D} = search:dates_if(STerms),
    

    {ok, M} = db:search_messages(DTerms),
    %{ok, F} = db:search_files(DTerms),

    error_logger:info_msg("Messages: ~p~n", [M]),
    {DTerms,
     [
      format_dates(lists:usort(D)),
      G, C, S,
      messages(lists:usort(M)),
      tasks(lists:usort(M))
      %files(lists:usort(F))
     ]}.

get_term(Terms) ->  % {{{1
   proplists:get_value("Term", Terms, "").

check_roles(Terms, True, False) ->  % {{{1
    Roles = [R || {R, _} <- ?ROLES],
    Additional = ["Due", "Duerange", "Status"],
    case lists:any(fun(R) -> proplists:is_defined(R, Terms) end, Roles ++ Additional) of
        true ->
            True();
        false ->
            False()
    end.

bage_types() ->  % {{{1
    [
     {["Date", "Due"], fun ?MODULE:date_badge/2},
     {["Daterange", "Duerange"], fun ?MODULE:daterange_badge/2},
     {["Contact" | [ R || {R, _} <- ?ROLES]], fun ?MODULE:simple_badge/2},
     {["Group"], fun ?MODULE:simple_badge/2},
     {["Status"], fun ?MODULE:simple_badge/2}
    ].

date_badge({Type, Date}, Variants) ->  % {{{1
    Text = sugar:date_format(Date),
    simple_badge({Type, Text}, Variants).

daterange_badge({Type, {SDate, EDate}}, Variants) ->  % {{{1
    Text=sugar:date_string(SDate) 
    ++ " " ++
    sugar:date_string(EDate),
    simple_badge({Type, Text}, Variants);
daterange_badge({Type, Daterange}, Variants) ->  % {{{1
    [SDate, EDate] = string:tokens(Daterange, " "),
    daterange_badge({Type, {SDate, EDate}}, Variants).

simple_badge({Type, Text}, Variants) ->  % {{{1
    #sigma_search_badge{type=Type,
                        text=Text,
                        dropdown=Variants -- [Type]}.

get_badge_for_type({"Term", _Data}) ->  % {{{1
    [];
get_badge_for_type({Type, _Data}=In) ->  % {{{1
    lists:foldl(fun({Variants, Fun}, A) ->
                        case lists:member(Type, Variants) of
                            true ->
                                Fun(In, Variants);
                            _ ->
                                A
                        end
                end,
                simple_badge(In, [Type]),
                bage_types()).

format_dropdown_group(_Name, _Term, []) ->  % {{{1
    [];
format_dropdown_group(Name, Term, List) ->  % {{{1
                    #panel{body=["<dl class='dl-horizontal'>",
                                 "<dt>", 
                                 Name, 
                                 ":</dt><dd>",
                                 lists:map(fun(Item) ->
                                                   #panel{body=[
                                                                #link{text=Item,
                                                                      postback={search, Item},
                                                                      delegate=common}
                                                               ]}
                                           end,
                                           List),
                                 "</dd>"]}.
