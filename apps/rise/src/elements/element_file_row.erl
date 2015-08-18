%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_file_row).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("bitmessage/include/bm.hrl").
-include("records.hrl").
-include("db.hrl").
-include("torrent.hrl").
-export([
    reflect/0,
    render_element/1,
    event/1,
    get_file_senders/1
]).

%% Move the following record definition to records.hrl:

-spec reflect() -> [atom()].
reflect() -> record_info(fields, file_row).

-spec render_element(#file_row{}) -> body().
%% Rendrer filerow  {{{1
render_element(Record = #file_row{
                           id=Id,
                           file=#bm_file{
                                   hash=FID,
                                   name=Name,
                                   path=Path,
                                   size=Size,
                                   time=Date,
                                   status=Status
                                  }
                          } = File) ->
    Type = case filename:extension(Name) of
               [] ->
                   "BINARY";
               T ->
                   string:to_upper(tl(T))
           end,
    {ok, Messages} = db:get_linked_messages(FID),
    Linked = sugar:once_join(lists:map(fun(#message{subject=S}) ->
                                         wf:to_list(S)
                       end,
                       Messages)),
    For = get_file_senders(Messages),

    Check =  sets:is_element(FID, wf:session_default(attached_files, sets:new())),

    #tablerow{id=Id,
              cells=[
            #tablecell{body=[
                            #checkbox{id=check,
                                      postback={check, FID, Check},
                                      checked=Check}
                    ], class=""},
            #tablecell{text=Name, class=""},
            #tablecell{text=Type, class=""},
            #tablecell{text=sugar:format_file_size(Size), class=""},
            #tablecell{text=For, class=""},
            #tablecell{text=Linked, class=""},
            #tablecell{text=sugar:date_format(Date), class=""},
            case Status of
                downloading ->
                    Percent = wf:to_integer(bitmessage:progress(FID) * 100),
                    #tablecell{body=#progressbar{progress=wf:to_list(Percent),
                                                 width=80},
                               class=""};
                _ when Status == uploaded; Status == downloaded; Status == imported ->
                    [
                     #tablecell{text=Status,
                                class=""},
                     #tablecell{body=
                                #panel{class="span1",
                                       body="<i class='icon icon-save'></i>",
                                       style="text-align:center;",
                                       actions=#event{type=click,
                                                      postback={save, Name, FID},
                                                      delegate=element_attachment} }}
                    ];
                received ->
                    [
                     #tablecell{text=Status,
                                class=""},
                     #tablecell{body=
                                #panel{class="span2",
                                       body="<i class='icon-download-alt'></i>",
                                       style="text-align:center;",
                                       actions=#event{type=click,
                                                      postback={download, Record},
                                                      delegate=?MODULE}
                                      }}
                    ]
            end
            ]}.

event({download, #file_row{id=I, file=#bm_file{hash=Id}} = Attachment}) -> % {{{1
    {ok, [ File ]} = db:get_files([Id]),
    common:get_torrent(Id), 
    NFile = File#bm_file{status=downloading},
    db:save(NFile),
    wf:replace(I, Attachment#file_row{file=NFile});
event(E) ->  % {{{1
    error_logger:warning_msg("Wrong event ~p in ~p", [E, ?MODULE]).

get_file_senders(FID) when is_binary(FID) ->
    {ok, Messages} = db:get_linked_messages(FID),
    get_file_senders(Messages);
get_file_senders(Messages) ->
    #db_contact{address=My} = wf:user(),
    sugar:once_join(lists:map(fun(#message{from=M, to=A}) when M == My ->
                                      {ok, #db_contact{name=C}} = db:get_contact_by_address(A),
                                      C;
                                 (#message{from=A, to=M}) when M == My ->
                                      {ok, #db_contact{name=C}} = db:get_contact_by_address(A),
                                      C
                              end,
                              Messages)).
