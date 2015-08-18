%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_attachment).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").
-export([
    reflect/0,
    render_element/1,
    event/1
]).

-define(ALPHA, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890").

-spec reflect() -> [atom()].
reflect() -> record_info(fields, attachment).

-spec render_element(#attachment{}) -> body().
render_element(#attachment{id=I,
                           fid=Id,
                           filename=File,
                           size=Size,
                           time=Time,
                           status=received} = Attachment) -> % {{{1
    PathId = wf:temp_id(),
    #panel{id=I,
           class="row-fluid",
           body=[
                 #panel{class="span5", body=File},
                 #panel{class="span1", body=sugar:format_file_size(Size)},
                 #panel{class="span5", body=sugar:date_format(Time)},
                 #panel{class="span1",
                        body=[
                              #hidden{id=PathId,
                                      actions=#event{type=change, 
                                                     postback={path, PathId, Attachment},
                                                     delegate=?MODULE}},
                              "<i class='icon-download-alt'></i>"
                             ],
                        style="text-align:center;",
                        actions=#event{type=click,
                                       postback={download, File, PathId},
                                       delegate=?MODULE}}
                ]};

render_element(Record=#attachment{id=I, % {{{1
                                  fid=Id,
                                  filename=File,
                                  size=Size,
                                  time=Time,
                                  status=downloading}=Attachment) ->
    Downloaded = bitmessage:progress(Id) * 100,
    {ok, Pid} = wf:comet(fun() ->
                                 receive 
                                     update ->
                                         wf:update(I, render_element(Attachment)),
                                         wf:flush()
                                 end
                         end),
    timer:send_after(10000, Pid, update),
    #panel{id=I, class="row-fluid", body=[
            #panel{class="span5", body=File},
            #panel{class="span1", body=sugar:format_file_size(Size)},
            #panel{class="span5", body=sugar:date_format(Time)},
            #panel{class="span1", body=[
                            wf:to_list(wf:to_integer(Downloaded))
                    ], style="text-align:center;"}

            ]};

render_element(#attachment{id=I,
                           fid=Id,
                           filename=File,
                           size=Size,
                           time=Time,
                           status=Status}) -> % {{{1
    #panel{id=I, class="row-fluid", body=[
            #panel{class="span5", body=File},
            #panel{class="span1", body=sugar:format_file_size(Size)},
            #panel{class="span5", body=sugar:date_format(Time)},
            #panel{class="span1",
                   body="<i class='icon icon-save'></i>",
                   style="text-align:center;",
                   actions=#event{type=click,
                                  postback={save, File, Id},
                                  delegate=?MODULE}}
            ]}.

event({path, PathId, #attachment{id=Id, fid=FID}=Attachment}) -> % {{{1
    Path = wf:q(PathId),
    bitmessage:get_attachment(FID, filename:dirname(Path)),
    wf:update(Id, Attachment#attachment{status=downloading});
event({download, File, PathId}) -> % {{{1
    wf:wire(#script{script="init_download('" ++ wf:to_list(PathId) ++ "')"}),
    wf:redirect("/raw?file=" ++ File).
