%% The type of torrent records.
-type torrent_mode() :: 'progress' | 'endgame'.
-type torrent_state() ::
        'leeching' | 'seeding' | 'paused' | 'unknown' | 'waiting' | 'checking' | 'partial'.
-type peer_id() :: etorrent_types:peer_id().
-type torrent_id() :: etorrent_types:torrent_id().

-record(torrent,
	{ %% Unique identifier of torrent, monotonically increasing
          id :: non_neg_integer(),
          %% How many bytes are there left before we stop.
          %% Only valid (i.e. stored AND checked) pieces are counted.
          left = unknown :: unknown | non_neg_integer(),
          display_name :: string(),
          %% The number of bytes this client still has to download. 
          %% Clarification: The number of bytes needed to download to be 
          %% 100% complete and get all the included files in the torrent.
          left_or_skipped = unknown :: unknown | non_neg_integer(),
          %% How many bytes are there in total
          total  :: non_neg_integer(),
          %% How many bytes we want from this torrent (both downloaded and not yet)
          wanted :: non_neg_integer(),
          %% How many bytes have we uploaded
          uploaded :: non_neg_integer(),
          %% How many bytes have we downloaded
          downloaded :: non_neg_integer(),
          %% Uploaded and downloaded bytes, all time
          all_time_uploaded = 0 :: non_neg_integer(),
          all_time_downloaded = 0 :: non_neg_integer(),
          %% Number of pieces in torrent
          pieces = unknown :: non_neg_integer() | 'unknown',
          %% How many people have a completed file?
          %% `complete' field from the tracker responce.
          seeders = 0 :: non_neg_integer(),
          %% How many people are downloaded
          %% `incomplete' field from the tracker responce.
          leechers = 0 :: non_neg_integer(),
          connected_seeders = 0 :: non_neg_integer(),
          connected_leechers = 0 :: non_neg_integer(),
          %% This is a list of recent speeds present so we can plot them
          rate_sparkline = [0.0] :: [float()],
          %% BEP 27: is this torrent private
          is_private :: boolean(),
          %% A rewritten local peer id for this torrent.
          peer_id :: peer_id() | undefined,
          %% A rewritten target directory (download_dir).
          directory :: file:filename() | undefined,
          mode :: torrent_mode(),
          state :: torrent_state(),
          is_paused = false :: boolean(),
          is_partial = false:: boolean()}).

