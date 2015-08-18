%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_vcard).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").
-export([
    reflect/0,
    render_element/1,
    start_upload_event/1,
    finish_upload_event/4
    ]).

-spec reflect() -> [atom()].  % {{{1
reflect() -> record_info(fields, vcard).

-spec render_element(#vcard{}) -> body(). % {{{1
render_element(#vcard{id=Id,
                      photo=Photo,
                      name=Name,
                      email=Email,
                      phone=Phone,
                      address=Address,
                      groups=Groups}) ->
    Photo2 = ?WF_IF(Photo=="undefined.png","/img/nophoto.png","/photo/" ++ Photo),
    {ok, AGroups} = db:get_groups(),
    #panel{id=Id,
           class="vcard row-fluid",
           body=[
                 #panel{class="span2",
                        body=[
                              #image{id=img_vcard,
                                     image=Photo2,
                                     class="image-polaroid",
                                     actions=[
                                              #event{type=click,
                                                     actions=[
                                                              #event{target=upload_vcard,
                                                                     actions=#show{}},
                                                              #event{actions=#hide{}}
                                                             ]}
                                             ]},
                              #span{id=upload_vcard,
                                    body= #upload{tag={photo, Id},
                                                  show_button=false,
                                                  droppable=true,
                                                  droppable_text="Drag and drop photo",
                                                  style="width:100%;height:100%",
                                                  delegate=?MODULE}, 
                                    actions=#hide{}} 
                             ]},
                 #panel{class="span9",
                        body=[
                              #panel{class="row-fluid",
                                     body=[
                                           #h1{class="",
                                               body= #inplace_textbox{text=Name,
                                                                      tag={name, Id}}
                                              },
                                           #panel{body= [ "Email: ",
                                                          #inplace_textbox{class="inline",
                                                                           tag={ email, Id},
                                                                           text=  Email,
                                                                           validators=#is_email{text="You provided wrong e-mail address"}}
                                                        ]},
                                           #panel{body= [
                                                         "Tel: ",
                                                         #inplace_textbox{class="inline",
                                                                          text=Phone,
                                                                          tag={phone, Id}}
                                                        ]},
                                           #panel{body= [
                                                         "RISE ID: ",
                                                         #inplace_textbox{class="inline",
                                                                          text=Address,
                                                                          tag={address, Id}}
                                                        ]},
                                           #panel{body= [ [#span{class="label", text=G}, " "] || G <- Groups]}
                                          ]}

                             ]},
                 #panel{class="span1",
                        body=[
                              #link{class="btn btn-link",
                                    body = "<i class='icon-envelope icon-large'></i>",
                                    postback={write_to, Address }},
                              #link{class="btn btn-link",
                                    body = #image{image="/img/tasks.svg",
                                                  class="icon",
                                                  style="height: 40px;"},
                                    postback={task_for, Address }},
                              #panel{class="btn-group",
                                     body=[
                                           #link{ class="btn btn-link dropdown-toggle",
                                                  body=[
                                                        "<i class='icon-reorder icon-large'></i>"
                                                       ],
                                                  new=false,
                                                  data_fields=[{toggle, "dropdown"}]},
                                           #list{numbered=false,
                                                 class="dropdown-menu pull-right",
                                                 body=[
                                                       #listitem{body=[
                                                                       #link{body=[
                                                                                   "<i class='icon-list-alt icon-large'></i> Archive"
                                                                                  ],
                                                                             postback={archive, Address},
                                                                             new=false}]}
                                                      ]}
                                          ]}
                             ]}
                ]}.

event({checked, Address, GID}) ->
    {ok, Contact} = db:get_contact_by_address(Address),
    State = wf:q(wf:to_list(GID)),
    case State of
        undefined ->
            mnesia:dirty_delete_object(#db_group_members{group=GID, contact=Contact#db_contact.id});
        "on" ->
            db:save(#db_group_members{group=GID, contact=Contact#db_contact.id})
    end,
    wf:redirect("/relationships").


start_upload_event(_) ->
    ok.
finish_upload_event({photo, _}, FName, FPath, _Node) ->
    io:format("File uploaded: ~p to ~p ~n", [FName, FPath]),
    NewFilename = filename:basename(FPath),% ++ filename:extension(FName),
    NewPath = "./photo/" ++ NewFilename,
    ok =  filelib:ensure_dir(NewPath),
    ok = file:rename(FPath, NewPath),
    CU = wf:session(current_contact),
    NCU = CU#db_contact{photo=NewFilename},
    db:save(NCU),
    wf:session(current_contact, NCU),
    wf:update(contact_panel, relationships:contact_render(NCU)).
