%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_update).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("bitmessage/include/bm.hrl").
-include("protokol.hrl").
-include("records.hrl").
-include("db.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, update_element).

name_from_address(Address) ->  % {{{1
    case db:get_contact_by_address(Address) of
        {ok, #db_contact{id=Id,
                         name=FN}} -> 
            {Id, FN};
        {ok, none} -> 
            {wf:to_list(Address),
             "User " ++ sugar:date_format(calendar:local_time())}
    end.

-spec render_element(#update_element{}) -> body().
%% Render collapsed update {{{1
render_element(#update_element{id=Id,
                               message=#message{
                                          from=From,
                                          to=To,
                                          text=Data,
                                          enc=Enc,
                                          time=TTL,
                                          status=Status}=Message,
                               age=Age,
                               collapse=true
                               }=Record) ->
    {FromId, FromName} = name_from_address(From),
    {ToId, ToName} = name_from_address(To),

    Text = case receiver:extract_packet(Data) of
               #{type := Type,
                 text := T} when Type == message;
                                 Type == task; 
                                 Type == task_comment ->
                   wf:html_encode(T);
               Packet ->
                   wf:f("Packet: ~p", [Packet])
           end,
    TD = bm_types:timestamp() - sugar:ttl_to_timestamp(TTL), %Timstamp,
    #panel{id=Id,
           class="row-fluid clickable",
           body=[

                 #panel{class="span4",
                        body=[
                              #span{body="<i class='icon-chevron-down'></i> ",
                                    actions=#event{type=click,
                                                   postback={unfold, Record},
                                                   delegate=common}},
                              #span{text=FromName,
                                    actions=#event{type=click,
                                                   delegate=common,
                                                   postback={to_contact, FromId}}
                                   },
                              " <i class='icon-arrow-right'></i> ",
                              #span{text=ToName,
                                    actions=#event{type=click,
                                                   postback={to_contact, ToId},
                                                   delegate=common
                                                  }}
                             ]},

                #panel{class="span6",
                       body=io_lib:format("~100s",
                                          [Text])},
                #panel{class="span2",
                       body=[
                             sugar:format_timedelta(TD),
                             format_status(Status)
                            ]}
            ]};
%% Render uncollapse update  {{{1
render_element(#update_element{id=Id,
                               message=#message{
                                          hash=UID,
                                          from=From,
                                          to=To,
                                          text=Data,
                                          subject=Subject,
                                          enc=Enc,
                                          time=TTL,
                                          attachments=Attachments,
                                          status=Status
                                         } = Message,
                               age=Age,
                               collapse=false
                              }=Record) ->
    {FromId, FromName} = name_from_address(From),
    {ToId, ToName} = name_from_address(To),
    NStatus = case db:set_read(UID) of
                  {ok, unread} ->
                      index:replace_left(),
                      wf:replace(count, common:unread()),
                      read;
                  {ok, _} ->
                      Status
              end,
    Packet = receiver:extract_packet(Data),
    wf:info("Packet ~p", [Packet]),
    TD = bm_types:timestamp() - sugar:ttl_to_timestamp(TTL), %Timstamp,
    #panel{id=Id,
           body=[
                 #panel{class="row-fluid",
                        body=[
                              #panel{class="span4",
                                     body=[
                                           #span{body="<i class='icon-chevron-down'></i> ",
                                                 actions=#event{type=click,
                                                                postback={fold, Record},
                                                                delegate=common}},
                                           #span{text=FromName,
                                                 style="cursor:pointer;",
                                                 actions=#event{type=click,
                                                                delegate=common,
                                                                postback={to_contact, FromId}}
                                                },
                                           " <i class='icon-arrow-right'></i> ",
                                           #span{text=ToName,
                                                 style="cursor:pointer;",
                                                 actions=#event{type=click,
                                                                postback={to_contact, ToId},
                                                                delegate=common
                                                               }}
                                          ]},
                              #panel{class="span2 offset6",
                                     body=[
                                           sugar:format_timedelta(TD),
                                           format_status(NStatus)
                                          ]}
                             ]},
                 #panel{
                    class="row-fluid",
                    body=[
                          #panel{
                             class="span12",
                             body=case Packet of
                                      #{type := task,
                                        due := Due,
                                        involved := Involved,
                                        status := TStatus,
                                        text := Text} ->
                                          #panel{
                                             class="",
                                             body=[
                                                   #panel{
                                                      class="", 
                                                      body=["Due: ", sugar:date_format(Due)]
                                                     },
                                                   #panel{
                                                      class="", 
                                                      body=["Status: ", TStatus]
                                                     },
                                                   lists:map(fun(#role_packet{
                                                                    address=Address,
                                                                    role=R}) ->
                                                                     {ok,
                                                                      #db_contact{
                                                                         name=Name}
                                                                     } = db:get_contact_by_address(Address),
                                                                     #panel{
                                                                        class="",
                                                                        body=[Name ++ " - " ++ R]}
                                                             end,
                                                             Involved),
                                                   #br{},
                                                   wf:html_encode(Text, whites)
                                            ]};
                                      #{text := Text} ->
                                          wf:html_encode(Text, whites)

                                  end
                            }
                         ]},
                 #panel{
                    class="row-fluid",
                    body=[
                          #panel{
                             class="span3 offset4",
                             body=[
                                   #link{
                                      class="btn btn-link",
                                      body=[
                                            #span{class="icon-reply icon-large",
                                                  text=" "}
                                           ],
                                      postback=case Enc of

                                                   E when E == 3; E == 2 -> 
                                                       {reply, Subject, From};
                                                   4 ->
                                                       {to_task, maps:get(tid, Packet, empty)}
                                               end,
                                      new=false,
                                      delegate=common},

                                   #panel{
                                      class="btn-group",
                                      body=[
                                            #link{
                                               class="btn btn-link droppdown-toggle",
                                               body=[
                                                     "<i class='icon-reorder icon-large'></i>"
                                                    ],
                                               new=false,
                                               data_fields=[{toggle, "dropdown"}]},
                                            #list{
                                               numbered=false,
                                               class="dropdown-menu pull-right",
                                               body=[
                                                     case Enc of
                                                        E when E == 2;
                                                               E == 3 ->
                                                            #listitem{
                                                               body=[
                                                                     #link{
                                                                        body=[
                                                                              #span{
                                                                                 style="float: left;",
                                                                                 body=tasks:icon()},
                                                                              " Convert to task"
                                                                             ],
                                                                        postback={to_task, Message},
                                                                        new=false}]};
                                                        _ -> []
                                                    end,
                                               #listitem{
                                                  body=[
                                                        #link{
                                                           body=[
                                                                 "<i class='icon-list-alt icon-large'></i> Archive"
                                                                ],
                                                           postback={archive, Enc, UID},
                                                           new=false}]}
                                                    ]}

                                           ]}
                                  ]}]},
                 case Attachments of
                     undefined ->
                         [];
                     [] ->
                         [];
                     A ->
                         {ok, Files} = db:get_files(A),
                         [
                          #panel{class="row-fluid", body=[
                                                          #panel{class="span6",
                                                                 body="<i class='icon-file-alt'></i> Attachment"},
                                                          #panel{class="span2 offset4",
                                                                 body="<i class='icon-download-alt'></i> Download all"}
                                                         ]},
                          lists:map(fun(#bm_file{
                                           hash=FID,
                                           name=Path,
                                           size=Size,
                                           time=Timestamp,
                                           status=FStatus}) ->
                                            Datetime = sugar:date_format(sugar:timestamp_to_datetime(Timestamp)),
                                            #attachment{fid=FID,
                                                        filename=Path,
                                                        size=Size,
                                                        time=Datetime,
                                                        status=FStatus}
                                    end,
                                    Files)
                         ]
                 end

                ]};
%% Render update as single paragraph for relationships {{{1
render_element(#update_element{
                  id=Id,
                  message=#message{
                             hash=UID,
                             from=From,
                             to=To,
                             text=Data,
                             subject=Subject,
                             enc=Enc,
                             attachments=Attachments,
                             time=TTL,
                             status=Status
                            } = Message,
                  age=Age,
                  collapse=paragraph}) ->
    Text = case receiver:extract_packet(Data) of
               #{type := Type,
                 text := T} when Type == message;
                                 Type == task; 
                                 Type == task_comment ->
                   wf:html_encode(T);
               Packet ->
                   wf:f("Packet: ~p", [Packet])
           end,

    TD = bm_types:timestamp() - sugar:ttl_to_timestamp(TTL), %Timstamp,
    [
     #panel{class="row-fluid",
            body=[
                  #panel{style="cursor:pointer",
                        class="span9",
                         body="<b>Subject: </b>" ++ Subject},
                  #panel{class="span2 cell-right",
                         body=sugar:format_timedelta(TD)}
                 ],
            actions=#event{type=click,
                           postback={to_message, UID},
                           delegate=common}},
     #panel{class="row-fluid",
            body=[
                  #panel{class="span12",
                         body=Text}
                 ]}
    ].

format_status(ok) ->  % {{{1
    " (received)";
format_status(read) ->  % {{{1
    " (read)";
format_status(unread) ->  % {{{1
    " (unread)";
format_status(ackwait) ->  % {{{1
    " (sent)";
format_status(new) ->  % {{{1
    [" (creating message authentification)",
     #progressbar{progress="0"}];
format_status(wait_pubkey) ->  % {{{1
    [" (waiting for key)",
     #progressbar{progress="25"}];
format_status(encrypt_message) ->  % {{{1
    [" (sending)",
     #progressbar{progress="80"}];
format_status(Status) ->  % {{{1
    " " ++ wf:to_list(Status).

