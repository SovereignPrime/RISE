%% -*- mode: nitrogen -*-
-module (edit_update).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

main() -> common:main().

title() -> "Welcome to Nitrogen".

icon() -> "<i class='icon-globe icon-2x'></i>".

buttons(left) -> % {{{1
    "";
buttons(main) -> % {{{1
    #panel{class='row-fluid', body=[
            #panel{class='span9 offset3',
                   body=[
                         #panel{class="row-fluid",
                                body=[
                                      #button{class='btn btn-link span2',
                                              body="<i class='icon-remove'></i> Discard", 
                                              click=#redirect{url="/index"}},
                                      #button{class='btn btn-link span2',
                                              body="<i class='icon-ok'></i> Send",
                                              postback=save,
                                              delegate=?MODULE}
                                     ]}
                    ]}
            ]}.

left() -> % {{{1
    Current = wf:session(current_update),
    Thread = maps:get(tread, Current, undefined),
    wf:info("Left"),
    {ok, Updates} = db:get_updates_by_thread(Thread),
    [
    #panel{class="span3",
           body=[
                 #panel{id=files,
                        class="row-fluid",
                        body=[
                              common:render_files()
                             ]},
                 #panel{class="row-fluid",
                        body=[
                              case Updates of
                                  [] ->
                                      [];
                                  U -> 
                                      #panel{ class="span12",
                                              body=[
                                                    "<i class='icon-file-alt'></i> Previous updates",
                                                    #br{},
                                                    [#update_preview{flag=false,
                                                                     message=M} || M <- U]
                                                   ]}
                              end
                             ]}
                ]}].
body() -> % {{{1
    Current  = wf:session(current_update),
    Subject = maps:get(subject, Current, <<>>),
    Text = maps:get(text, Current, <<>>),
    To = maps:get(to, Current, <<>>),
    #panel{class="span9",
           body=[
                 #panel{class="row-fluid",
                        body=[
                              #panel{class="input-prepend span12",
                                     body=[
                                           #span{class="add-on",
                                                 body=[
                                                       #span{html_encode=false,
                                                             text="<i class='icon-message'></i>"}
                                                      ]},
                                           #textbox{id=name,
                                                    placeholder="Re:something",
                                                    text=Subject,
                                                    next=due,
                                                    class="span12"}
                                          ]}
                             ]},
                 #sigma_search{tag=to, 
                               placeholder="Contacts", 
                               class="input-append input-prepend input-block-level search", 
                               textbox_class="",
                               search_button_class="hidden btn btn-inverse search-btn wfid_to_field", 
                               badges=[render_contact(A) || A <- To],
                               search_button_text="<i class='icon icon-search'></i>",
                               x_button_class="search-x",
                               clear_button_class="pull-right btn btn-inverse",
                               clear_button_text="<i class='icon icon-remove'></i>",
                               results_summary_class="search-results span10",
                               delegate=common},

                 #panel{class="row-fluid",
                        body=[
                              #panel{class="span12",
                                     body=[
                                           #textarea{class="input-block-level",
                                                     rows=15,
                                                     text=Text,
                                                     placeholder="Some text here",
                                                     id=text}
                                          ]}

                             ]}
                ]}.


event(add_file) -> % {{{1
    Subject = wf:q(name),
    InvolvedS = wf:qs(person),
    Text = wf:q(text),
    Update = wf:session(current_update),
    #db_contact{id=UID} = wf:user(),
    Involved = lists:map(fun([]) ->
                    <<"">>;
                (N) ->
                    io:format("~p~n", [N]),
                    I = wf:session(wf:to_binary(N)),
                    io:format("~p~n", [I]),
                    {ok,  #db_contact{bitmessage=BM} } = db:get_contact(I),
                    BM
            end, InvolvedS) -- [<<"">>],
    io:format("~p~n", [Involved]),
    NUpdate = Update#{type => message,
                      subject => Subject,
                      text => Text,
                      from => UID, 
                      to => Involved,
                      status => new},
    wf:session(current_update, NUpdate),
    wf:redirect("/files?from=message");
event(save) -> % {{{1
    wf:wire(#script{script="$('.wfid_to_field').click();"});
event(Ev) -> % {{{1
    io:format("Event ~p in module ~p~n", [Ev, ?MODULE]).

incoming() ->  % {{{1
    ok.

render_contact(Address) ->  % {{{1
    {ok, #db_contact{name=Name}} = db:get_contact_by_address(Address),
    search:simple_badge({"Contact", Name}, ["Contact"]).

