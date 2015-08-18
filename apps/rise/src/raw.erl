%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (raw).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> 
    File = wf:q(file),
    Id = wf:q(id),
    wf:header("Content-type", "octet/binary"),
    wf:header("Content-Disposition",wf:f("attachment; filename=~s", [File])),
    wf:to_binary(Id).

