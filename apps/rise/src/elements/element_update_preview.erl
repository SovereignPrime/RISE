%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_update_preview).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("bitmessage/include/bm.hrl").
-include("protokol.hrl").
-include("records.hrl").
-include("db.hrl").
-export([
         reflect/0,
         render_element/1,
         render_icon/1
        ]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, update_preview).


-spec render_element(#update_preview{}) -> body().
%% Render Messages  or  update  {{{1
render_element(#update_preview{id=Id,
                               message=#message{
                                          hash=UID,
                                          from=From,
                                          to=To,
                                          time=TTL,
                                          subject=Subject,
                                          text=Data,
                                          status=Status},
                               flag=Flag,
                               archive=Archive}) -> 
    #{type := Icon,
      text := Text} = Packet = receiver:extract_packet(Data),
    TD = bm_types:timestamp() - sugar:ttl_to_timestamp(TTL), %Timstamp,
    CurrentUpdate = wf:session_default(current_update, Packet),

    Thread = case maps:get(thread, Packet, UID) of
                 undefined -> UID;
                 T -> T
             end,
    CurrentUpdateThread = maps:get(thread, CurrentUpdate, Thread),
    CurrentThread = wf:session_default(current_thread, Thread),

    Class = if Thread == CurrentThread ->
           "current";
       true ->
           ""
    end,

    #panel{class=['update-preview',clickable,Class],
           style="line-height:18px;margin-top:18px;",
           body=[
                 #panel{class="row-fluid no-padding",
                        body=[

                              #panel{class="span1 no-padding",
                                     body=render_icon(Icon, Status)},
                              #panel{class='span9 no-padding update-participant-list',
                                     text=participant_list([From] ++ [To])},
                              #panel{class='span2 cell-right no-padding update-age',
                                     body=[sugar:format_timedelta(TD)]}
                             ]},
                 case Subject of 
                     undefined -> "";
                     Subject ->
                         #panel{class='row-fluid',
                                body=[
                                      #panel{class='span11 offset1 no-padding',
                                             style="overflow: hidden; font-weight:bold",
                                             text=Subject}
                                     ]}
                 end,
                 #panel{class='row-fluid',
                        body=[
                              if Flag == true ->
                                     [
                                      #panel{class='span11 shorten-text offset1',
                                             style="-webkit-line-clamp:2;",
                                             text=[Text]}
                                     ];
                                 true ->
                                     #panel{class='span12 shorten-text',
                                            style="-webkit-line-clamp:2;",
                                            text=[Text]}
                              end
                             ]}
                ],
           actions=#event{type=click,
                          postback={selected,
                                    if Thread == CurrentUpdateThread ->
                                           CurrentUpdate;
                                       thue -> Packet
                                    end,
                                    Thread, 
                                    Archive}}}.

render_icon(Icon) ->  % {{{1
    render_icon(Icon, true).

render_icon(Icon, unread) when Icon == message;  % {{{1
                               Icon == task_comment;
                               Icon == error ->
    #span{
       class="icon-stack",
       body="<i class='icon-sign-blank icon-stack-base'></i><i class='icon-envelope' style='color:#fff'></i>"
      };
render_icon(Icon, State) when Icon == message;  % {{{1
                              Icon == task_comment;
                              Icon == error ->
    #span{
       class="icon-stack",
       body="<i class='icon-sign icon-stack-base'></i><i class='icon-envelope'></i>"
      };
render_icon(task, unread) ->  % {{{1
    #span{
          body=[
                "<i class='icon-sign-blank icon-stack-blank icon-2x'>",
                "</i>",
                #image{image="/img/tasks.svg", class='icon-task-unread'}
               ]
      };
render_icon(task, State) ->  % {{{1
    #span{
          body=[
                #image{image="/img/tasks.svg",
                       style="-webkit-filter:invert(0);top:2px;",
                       class='icon-task-unread'}
               ]
      };
render_icon(update, unread) ->  % {{{1
    #span{
       body="<i class='icon-sign-blank icon-stack-base'></i><i class='icon-refresh'></i>"
      };
render_icon(update, State) ->  % {{{1
    #span{
       class="icon-stack",
       body="<i class='icon-sign icon-stack-base'></i><i class='icon-refresh' style='color:#fff'></i>"
      }.

participant_list(List) ->  % {{{1
    Deduped = common:remove_duplicates(lists:flatten(List)),
    wf:join([get_name(Address) || Address <- Deduped], ", ").

get_name(UID) ->  % {{{1
    case db:get_contact_by_address(UID) of
        {ok, #db_contact{name=FN}} ->
            FN;
        {ok, none} ->
            "User " ++ sugar:date_string(date())
            %wf:to_list(UID)
    end.

