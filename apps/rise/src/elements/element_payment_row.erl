%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (element_payment_row).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-export([
    reflect/0,
    render_element/1
]).

-spec reflect() -> [atom()].
reflect() -> record_info(fields, payment_row).

-spec render_element(#payment_row{}) -> body().
render_element(_Record = #payment_row{pid=Id,
                                      from=From,
                                      to=To,
                                      tasks=Tasks,
                                      due=Due,
                                      status=Status,
                                      amount=Amount,
                                      currency=Currency,
                                      type=Type}) ->
    #tablerow{ cells=[
            #tablecell{body=[
                    #checkbox{id=wf:f("check~p", [ Id ]),  postback={check, Id}, checked=false}
                    ], class=""},
            #tablecell{text=From, class=""},
            #tablecell{text=To, class=""},
            #tablecell{text=Tasks, class=""},
            #tablecell{text=Due, class=""},
            #tablecell{text=Status, class=""},
            #tablecell{text=Amount, class=""},
            #tablecell{text=Currency, class=""},
            #tablecell{text=Type, class=""} 
            ]}.
