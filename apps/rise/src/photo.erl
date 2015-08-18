-module(photo).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	error_logger:info_msg("photo"),
	File = sanitize(wf:path_info()),
	case filelib:is_regular("./photo/" ++ File) of
		true ->
			wf:content_type("application/octet-stream"),
			{ok, Bin} = file:read_file("./photo/" ++ File),
			Bin;
		false ->
			wf:redirect("/img/nophoto.png"),
			[]
	end.

%% Prevent directory snooping by just replacing slashes
sanitize([]) -> [];
sanitize([$/ | R]) -> sanitize(R);
sanitize([C | R]) -> [C | sanitize(R)].
