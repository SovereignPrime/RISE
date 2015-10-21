%% -*- mode: nitrogen -*-
%% vim: ts=4 sw=4 et
-module (raw).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() -> 
    case wf:q(image) of
        "true" ->
            File = wf:q(file),
            case file:read_file(File) of 
                {ok, Data} ->
                    wf:header("Content-type", "image/jpeg"),
                    Data;
                _ -> <<>>
            end;
        _ ->
            File = wf:q(file),
            Id = wf:q(id),
            wf:header("Content-type", "octet/binary"),
            wf:header("Content-Disposition",wf:f("attachment; filename=~s", [File])),
            wf:to_binary(Id)
    end.

