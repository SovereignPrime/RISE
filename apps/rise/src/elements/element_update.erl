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
    render_element/1,
    event/1,
    time_delta/1
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
                                          hash=UID,
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
    TD = time_delta(TTL),
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
                             format_status(Status, -1, UID)
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
    TD = time_delta(TTL),
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
                                           format_status(NStatus, -1, UID)
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
                                                   lists:map(fun(#{type := role,
                                                                   address := Address,
                                                                   role := R}) ->
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
                                      postback={reply, UID, Packet},
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

%% Render new update  {{{1
render_element(#update_element{id=Id,
                               message=Message,
                               collapse=new
                              }=Record) ->
    #db_contact{id=FromId,
                name=FromName,
               address=FromAddress} = wf:user(),
    To = case maps:get(type, Message, message) of
             message ->
                 maps:get(to, Message, []);
             T when T == task;
                    T == task_comment ->
                 TID = case T of
                           task -> 
                               maps:get(id, Message);
                           task_comment ->
                               maps:get(task, Message)
                       end,
             
                 
                 {ok, Involved} = db:get_involved(TID),
                 [C || {_, _, #db_contact{bitmessage=C}}  <- Involved, 
                       C /= FromAddress]
         end,
    #panel{
       id=Id,
       body=[
             #panel{
                class="row-fluid",
                body=[
                      #panel{
                         class="span2",
                         body=[
                               #span{
                                  body="<i class='icon-chevron-down'></i> "},
                               #span{
                                  text=FromName,
                                  style="cursor:pointer;",
                                  actions=#event{type=click,
                                                 delegate=common,
                                                 postback={to_contact, FromId}}
                                 },
                               " <i class='icon-arrow-right'></i> "]},
                      #panel{
                         class="span7",
                         body=[
                               #sigma_search{
                                  id=to,
                                  tag=to, 
                                  style="border: #000 1px solid; box-shadow: none;",
                                  placeholder="Contacts", 
                                  class="input-append input-prepend input-block-level search", 
                                  textbox_class="",
                                  search_button_class="btn btn-inverse search-btn wfid_to_field", 
                                  badges=[render_contact(A) || A <- To],
                                  search_button_text="<i class='icon icon-arrow-right'></i>",
                                  x_button_class="search-x",
                                  clear_button_class="pull-right btn btn-inverse",
                                  clear_button_text="<i class='icon icon-remove'></i>",
                                  results_summary_class="search-results span10",
                                  delegate=common}
                              ]},
                      #panel{
                         style="text-align: right;",
                         class="span1 offset1",
                         body="Now (composing)"
                        }
                     ]},
             #panel{
                    class="row-fluid",
                    body=[
                          #panel{
                             class="span11",
                             body=#textarea{class="input-block-level",
                                            style="border: #000 0px solid; box-shadow: none;",
                                            rows=15,
                                            %text=Text,
                                            placeholder="Some text here",
                                            id=text}
                            }
                         ]}
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

    TD = time_delta(TTL),
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
                           postback={to_message, Message},
                           delegate=common}},
     #panel{class="row-fluid",
            body=[
                  #panel{class="span12",
                         body=Text}
                 ]}
    ].

format_status(ok, _P, _UID) ->  % {{{1
    " (received)";
format_status(read, _P, _UID) ->  % {{{1
    " (read)";
format_status(unread, _P, _UID) ->  % {{{1
    " (unread)";
format_status(ackwait, _P, _UID) ->  % {{{1
    " (sent)";
format_status(new, P, UID) ->  % {{{1
    Id=wf:temp_id(),
    Progress = if P > 20 -> 
                      20;
                  P == -1 ->
                      1;
                  true ->
                      P + 1
               end,
    wf:wire(#event{type=timer, 
                   delay=5000,
                   target=Id,
                   postback={status, Id, Progress, UID},
                   delegate=?MODULE}),
    #span{id=Id,
          body=[" (creating message authentification)",
                #progressbar{progress=wf:to_list(Progress)}]};
format_status(wait_pubkey, P, UID) ->  % {{{1
    Id=wf:temp_id(),
    Progress = if P > 70 -> 
                      70;
                  P == -1 ->
                      21;
                  true ->
                      P + 1
               end,
    wf:wire(#event{type=timer, 
                   delay=5000,
                   target=Id,
                   postback={status, Id, Progress, UID},
                   delegate=?MODULE}),
    #span{id=Id,
          body=[" (waiting for key)",
                #progressbar{progress=wf:to_list(Progress)}]};
format_status(encrypt_message, P, UID) ->  % {{{1
    Id=wf:temp_id(),
    Progress = if P > 97 -> 
                      97;
                  P == -1 ->
                      70;
                  true ->
                      P + 1
               end,
    wf:wire(#event{type=timer, 
                   delay=5000,
                   target=Id,
                   postback={status, Id, Progress, UID},
                   delegate=?MODULE}),
    #span{id=Id,
    body=[" (sending)",
          #progressbar{progress=wf:to_list(Progress)}]};
format_status(Status, _P, _UID) ->  % {{{1
    " " ++ wf:to_list(Status).

render_contact(Address) ->  % {{{1
    {ok, #db_contact{name=Name}} = db:get_contact_by_address(Address),
    search:simple_badge({"Contact", Name}, ["Contact"]).

event({status, Id, Progress, UID}) ->  % {{{1
    case db:get_status(message, UID) of
        {ok, unknown} -> 
            wf:redirect(wf:uri());
        {ok, Status} ->
            wf:replace(Id, format_status(Status, Progress, UID))
    end.

time_delta(TTL) ->  % {{{1
    case bm_types:timestamp() - sugar:ttl_to_timestamp(TTL) of
        TD1 when TD1 >= 0 -> TD1;
        _ -> 0
    end.
