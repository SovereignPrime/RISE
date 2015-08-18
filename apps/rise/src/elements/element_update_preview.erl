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
         render_icon/1,
         decode_type/1
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
    {Text, Timestamp, Icon} = decode_type(Data),
    TD = bm_types:timestamp() - sugar:ttl_to_timestamp(TTL), %Timstamp,
    %TD = bm_types:timestamp() - Timestamp,
    CurrentId = wf:session(current_update_id),
    HasCurrent = lists:any(fun(I) -> (I == CurrentId) end, sugar:maybe_wrap_list(UID)),
    Class = if HasCurrent ->
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
                                    sugar:maybe_wrap_list(UID),
                                    Subject,
                                    Archive}}}.

render_icon(Icon) ->  % {{{1
    render_icon(Icon, true).

render_icon(Icon, unread) when Icon==1; Icon==2; Icon==3 ->  % {{{1
    #span{
       class="icon-stack",
       body="<i class='icon-sign-blank icon-stack-base'></i><i class='icon-envelope' style='color:#fff'></i>"
      };
render_icon(Icon, State) when Icon==1; Icon==2; Icon==3 ->  % {{{1
    #span{
       class="icon-stack",
       body="<i class='icon-sign icon-stack-base'></i><i class='icon-envelope'></i>"
      };
render_icon(4, unread) ->  % {{{1
    #span{
          body=[
                "<i class='icon-sign-blank icon-stack-blank icon-2x'>",
                "</i>",
                #image{image="/img/tasks.svg", class='icon-task-unread'}
               ]
      };
render_icon(4, State) ->  % {{{1
    #span{
          body=[
                #image{image="/img/tasks.svg",
                       style="-webkit-filter:invert(0);top:2px;",
                       class='icon-task-unread'}
               ]
      };
render_icon(5, unread) ->  % {{{1
    #span{
       body="<i class='icon-sign-blank icon-stack-base'></i><i class='icon-refresh'></i>"
      };
render_icon(5, State) ->  % {{{1
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

decode_type(Data) ->  % {{{1
    try binary_to_term(Data, []) of
        #message_packet{text=Txt,
                        time=TS} ->
            {Txt, TS, 3};
        #update_packet{text=Txt, time=TS} -> 
            {Txt, TS, 5};
        #task_comment{task=TID,
                      time=TS,
                      text=Txt} ->
            {Txt, sugar:datetime_to_timestamp(TS), 4};
        Task when element(1, Task) == task_packet ->
            #task_packet{text=Txt,
                         time=TS} = receiver:extract_task(Task),
            {Txt, TS, 4};
        _ ->
            {<<"Decoding error! Data: ", Data/bytes>>, bm_types:timestamp(), 1}
    catch
        error:badarg ->
            {<<"Decoding error! Data: ", Data/bytes>>, bm_types:timestamp(), 1}
    end.

