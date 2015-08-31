%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (relationships).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("bitmessage/include/bm.hrl").
-include("records.hrl").
-include("db.hrl").

main() -> common:main().

title() -> "Hello from relationships.erl!".

icon() -> "<i class='icon-user icon-2x'></i>".

buttons(main) ->  % {{{1
    #list{numbered=false, class="nav nav-pills", style="display:inline-block;", body=[
       % #listitem{body=[
       %     #link{style="display:inline-block;text-align:right;",  body="<i class='icon-envelope-alt'></i> Email connect", postback=invite}
       % ]},
        #listitem{body=[
            common:render_filters()
        ]},
        %%#listitem{body=[
        %%    %#panel{ class='span2', body="<i class='icon-sort'></i> Sort"},
        %%]},
        #listitem{body=[
            #link{id=archive, body="<i class='icon-list-alt'></i> Archive", postback={show_archive, true}}
        ]},
        #listitem{body=[
            common:settings_menu()
        ]},
        #listitem{body=[
            common:render_help()
        ]}
    ]}.

left() ->  % {{{1
    GID = wf:session_default(current_group_id, all),
    {ok, Users} = db:get_contacts_by_group(GID),
    [
     #panel{id=group_list,
            class="span2",
            body=[render_group_list(false)]},
     #panel{id=user_list,
            class="span2",
            body=render_contact_list(Users)}
    ].

render_group_list(Archive) ->  % {{{1
    {ok, Groups} = db:get_groups(),
    G = wf:session(current_group_id),
    wf:wire(wf:f("group~p", [G]), #add_class{class="active"}),
    [
     #list{numbered=false,
           body=
           #group_item{gid=my,
                       name="My accounts",
                       sub=[],
                       archive=Archive }
     },
     #list{numbered=false,
           body=
           #group_item{gid=all,
                       name="All contacts",
                       sub=Groups,
                       archive=Archive }
     }
    ].

render_contact_list(Users) ->  % {{{1
    [
     #list{
        numbered=false,
        body=lists:map(fun(#db_contact{id=Id, name=Name}) ->
                #contact_li{uid=Id, name=Name, checked=false}
             end, Users)
     }

    ].
        

body() ->   % {{{1
    Body = case wf:session(current_contact_id) of
        undefined -> #h2{text="No Contact Currently Selected"};
        Contactid ->
            {ok, Contact} = db:get_contact(Contactid),
            wf:session(current_contact, Contact),
            contact_render(Contact)
    end,
    #panel{id=contact_panel, class="span8 scrollable", body=Body}.

contact_render(#db_contact{id=Id,  % {{{1
                           name=Name,
                           email=Email,
                           phone=Phone,
                           address=Address,
                           photo=Photo}) ->
    {ok, Notes} = db:get_notes_by_user(Id),
    {ok, Tasks} = db:get_tasks_by_user(Id),
    {ok, Updates} = db:get_updates_by_user(Address),
    {ok, Groups} = db:get_groups_for_user(Id),
    [
        #vcard{id=vcard,
               name=Name,
               address=Address,
               email=Email,
               phone=Phone,
               photo=Photo,
               groups=[ N|| #db_group{name=N} <- Groups]},

        #panel{class='row-fluid',
               body=[
                     #panel{class=span10,
                            body=[
                                  #h2{body=[
                                            "<i class='icon-file-alt'></i>",
                                            " Notes"
                                           ]}
                                 ]},
            #panel{class=span2,
                   body="Show All",
                   style="text-align:right"}
        ]},
        #panel{id=add_note,
               class='row-fluid',
               style="min-height:20px; height:20px;",
               body=[
                     #panel{class=span3,
                            style="min-height:20px; height:20px;",
                            text=sugar:date_format(calendar:local_time())},

                     #panel{class=span8,
                            style="min-height:20px; height:20px;",
                            body=[
                                  #textbox{id=note,
                                           class="span10",
                                           style="min-height:15px;border:0px;box-shadow:none;padding-left:0",
                                           placeholder="Add note here",
                                           next=ok}
                                ]},
                #panel{class=span1,
                       style="min-height:20px; height:20px; text-align:right",
                       body=[
                             #link{id=ok, 
                                   body=["<i class='icon-ok'></i>"],
                                   html_encode=false,
                                   postback={add_note, Id},
                                   delegate=?MODULE}
                            ]}
                    ]},
        [ render_contact_note(Note) || Note <- Notes],
        #panel{class='row-fluid', body=[
            #panel{class=span10, body=[
                #h2{body=[
                    #image{image="/img/tasks.svg",
                           class="icon",
                           style="height: 20px;"},

                    " Tasks"
                ]}
            ]},
            #panel{class=span2, body="Show All", style="text-align:right"}
        ]},
        lists:map(fun(Task = #db_task{id=TID,
                                      parent=Responsible,
                                      name=Name,
                                      due=Due}) ->
            #panel{class='row-fluid',
                   style="min-height:20px; height:20px;",
                   body=[
                #panel{class=span2,
                       style="min-height:20px; height:20px;",
                       text=Responsible},
                #panel{class=span7,
                       style="min-height:20px; height:20px;",
                       body=[
                    #link{text=Name, postback={to_task, TID}, delegate=common}
                ]},
                #panel{class=span3,
                       style="min-height:20px; height:20px; text-align:right",
                       text=sugar:date_format(Due)}
            ]}
        end, Tasks),
       
        #panel{class='row-fluid',
               body=[
                     #panel{class=span10,
                            body=[
                                  #h2{body=[
                                            "<i class='icon-message'></i> Messages"
                                           ]}  
                                 ]},
                     #panel{class=span2,
                            body="Show All",
                            style="text-align:right"}
                    ]},
        #panel{class="row-fluid",
               body=[
                     #panel{class="span12",
                            body=[
                                  lists:map(fun(M) ->
                                                    #update_element{collapse=paragraph,
                                                                    message=M,
                                                                    age="Age"}
                                            end, Updates)
                                 ]}
                    ]}
    ].

render_contact_note(Note = #db_contact_note{id=Id,  % {{{1
                                            datetime=Datetime,
                                            text=Text}) ->
    EID = wf:to_atom("note" ++ wf:to_list(Id)),
    #panel{id=EID,
           class='row-fluid',
           style="min-height:20px; height:20px;",
           body=[
                 #panel{class=span3,
                        style="min-height:20px; height:20px;",
                        text=sugar:date_format(Datetime)},

                 #panel{class=span8,
                        style="min-height:20px; height:20px;",
                        body=[
                              #inplace_textbox{tag={note, Note},
                                               html_encode=true,
                                               text=Text}
                             ]},
                 #panel{class=span1,
                        style="min-height:20px; height:20px; text-align:right",
                        body=[
                             #link{id=remove, 
                                   body=["<i class='icon-remove'></i>"],
                                   html_encode=false,
                                   postback={del_note, Id},
                                   delegate=?MODULE}
                             ]}
                ]}.

%%%
%% Event handlers
%%%

event(invite) ->  % {{{1
    #db_contact{email=MEmail} = wf:user(),
    #db_contact{email=REmail} = wf:session(current_contact),
    [_,  BaseServer ] = string:tokens(wf:to_list(MEmail), "@"),
    MailPrefixes = application:get_env(nitrogen, mail_prefixes,["smtp", "mail", "m"]),
    [Server | _] = lists:filter(fun(P) ->
                         case inet:getaddr(P ++ "." ++ BaseServer, inet) of
                             {ok, _} ->
                                 true;
                             _ ->
                                 false
                         end
                 end, MailPrefixes),
    wf:insert_bottom("body", #panel{ class="modal fade", body=[
                                             #panel{ class="modal-header", body=[
                                                                                 #button{class="btn-link pull-right", text="x", postback=cancel},
                                                                                 #h3{text="Email settings"}
                                                                                ]},
                                             #panel{ class="modal-body", body=[
                                                                               #label{text="Your email address"}, #textbox{id=mail, text=MEmail},#br{},
                                                                               #label{text="Email password"}, #password{id=passwd}
                                                                                ]},
                                             #panel{ class="modal-footer", body=[
                                                                                 #button{class="btn btn-link", text="Send", postback={invite, Server ++ "." ++ BaseServer, REmail}},
                                                                                 #button{class="btn btn-link", text="Cancel", postback=cancel}
                                                                              ]}
                                            ]}),
    wf:wire(#script{script="$('.modal').modal('show')"});


event(cancel) ->  % {{{1
    wf:wire(#script{script="$('.modal').modal('hide')"}),
    wf:remove(".modal");

event({archive, Rec}) ->  % {{{1
    db:archive(#db_contact{address=Rec}),
    Id = wf:session(current_group_id),
    {ok, Contacts} = db:get_contacts_by_group(Id),
    wf:update(group_list, render_group_list(false)),
    wf:update(user_list, render_contact_list(Contacts));

event({show_archive, true}) ->  % {{{1
    wf:replace(archive, #link{id=archive, body="<i class='icon-list-alt'></i> Actual", postback={show_archive, false}}),
    Id = wf:session(current_group_id),
    {ok, Contacts} = db:get_contacts_by_group(Id, true),
    wf:update(group_list, render_group_list(true)),
    wf:update(user_list, render_contact_list(Contacts));

event({show_archive, false}) ->  % {{{1
    wf:replace(archive, #link{id=archive, body="<i class='icon-list-alt'></i> Archive", postback={show_archive, true}}),
    Id = wf:session(current_group_id),
    {ok, Contacts} = db:get_contacts_by_group(Id, false),
    wf:update(group_list, render_group_list(false)),
    wf:update(user_list, render_contact_list(Contacts));

event({contact, Id}) ->  % {{{1
    {ok, Contact} = db:get_contact(Id),
    wf:session(current_contact, Contact),
    wf:update(contact_panel, contact_render(Contact));

event({group, Id, Archive}) ->  % {{{1
    {ok, Contacts} = db:get_contacts_by_group(Id, Archive),
    wf:session(current_group_id, Id),
    wf:update(group_list, render_group_list(Archive)),
    wf:wire(wf:f("group~p", [Id]), #add_class{class="active"}),
    wf:update(user_list, render_contact_list(Contacts));

event({group_delete, Id, Archive}) ->  % {{{1
    db:delete_group(Id),
    wf:update(group_list, render_group_list(Archive));

event({group_rename, Id, Archive}) ->  % {{{1
    wf:update(wf:f("group_~p", [Id]), render_group_list(Archive));

event({write_to, Addr}) ->  % {{{1
    wf:session(current_update, #{type => message,
                                 to => [Addr]}),
    wf:redirect("/edit_update");

event({task_for, Addr}) ->  % {{{1
    {ok, Contact} = db:get_contact_by_address(Addr),
    Me = wf:user(),
    wf:session(involved, [{0, responcible, Me},{0, concerning, Contact}]),
    common:event(add_task);

event({add_note, CID}) ->  % {{{1
    {ok, Id} = db:next_id(db_contact_note),
    Datetime = calendar:local_time(),
    case wf:q(note) of
        "" ->
            ok;
        Text ->
            Note = #db_contact_note{id=Id,
                                     contact=CID,
                                     text=Text,
                                     datetime=Datetime},
            db:save(Note),
            wf:set(note, ""),
            wf:insert_after(add_note, 
                            render_contact_note(Note))
    end;

event({del_note, NID}) ->  % {{{1
    db:delete(db_contact_note, NID),
    EID = wf:to_atom("note" ++ wf:to_list(NID)),
    wf:remove(EID);

event(Click) ->  % {{{1
    io:format("~p~n",[Click]).

inplace_textbox_event({name, Id}, Name) ->  % {{{1
    Contact = wf:session(current_contact),
    ContactN = Contact#db_contact{name=Name},
    db:save(ContactN),
    wf:session(current_contact, ContactN),
    {ok, Contacts} = db:get_contacts_by_group(wf:session(current_group_id)),
    wf:update(user_list, render_contact_list(Contacts)),
    Name;

inplace_textbox_event({email, Id}, Name) ->  % {{{1
    #db_contact{my=My} = Contact = wf:session(current_contact),
    ContactN = Contact#db_contact{email=Name},
    if My ->
           wf:user(ContactN);
       true ->
           ok
    end,
    wf:session(current_contact, ContactN),
    db:save(ContactN),
    Name;

inplace_textbox_event({phone, Id}, Name) ->  % {{{1
    Contact = wf:session(current_contact),
    ContactN = Contact#db_contact{phone=Name},
    db:save(ContactN),
    wf:session(current_contact, ContactN),
    Name;

inplace_textbox_event({address, Id}, Name) ->  % {{{1
    Contact = wf:session(current_contact),
    ContactN = Contact#db_contact{bitmessage=wf:to_binary(Name), address=wf:to_binary(Name)},
    db:save(ContactN),
    wf:session(current_contact, ContactN),
    Name;

inplace_textbox_event({note, Note}, Text) ->  % {{{1
    wf:info("Updating ~p text ~p", [Note, Text]),
    db:save(Note#db_contact_note{text=Text}),
    Text;

inplace_textbox_event(T, Name) ->  % {{{1
    io:format("Saved ~p tag ~p", [Name, T]),
    Name.

drop_event({contact, CId}, {subgroup, SId}) ->  % {{{1
    io:format("User: ~p Group: ~p~n", [CId, SId]),
    {ok, Contacts} = db:get_contacts_by_group(wf:session(current_group_id)),
    db:add_user_to_group(SId, CId),
    wf:update(user_list, render_contact_list(Contacts)),
    wf:update(contact_panel, contact_render(wf:session(current_contact)));

drop_event({group, CId, Archive}, {subgroup, all}) ->  % {{{1
    db:save_subgroup(CId, undefined),
    wf:update(group_list, render_group_list(Archive));

drop_event({group, CId, Archive}, {subgroup, SId}) when CId /= SId ->  % {{{1
    io:format("Group ~p to subgroup ~p~n", [SId, CId]),
    db:save_subgroup(CId, SId),
    wf:update(group_list, render_group_list(Archive));

drop_event(G, P) ->  % {{{1
    io:format("D&D ~p to ~p~n", [G, P]).

incoming() ->  % {{{1
    Id = wf:session(current_group_id),
    {ok, Contacts} = db:get_contacts_by_group(Id),
    wf:update(user_list, render_contact_list(Contacts)).
