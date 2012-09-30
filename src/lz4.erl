-module(lz4).

-export([compress/1, compress/2, uncompress/2]).

-on_load(init/0).

-type option() :: high | {block, integer()}.

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, lz4_nif), 0).

-spec compress(binary()) -> {ok, binary()} | {error, term()}.
compress(Binary) ->
    compress(Binary, []).

-spec compress(binary(), [option()]) -> {ok, binary()} | {error, term()}.
compress(_Binary, _Options) ->
    ?nif_stub.

-spec uncompress(binary(), integer()) -> {ok, binary()} | {error, term()}.
uncompress(_Binary, _OnigSize) ->
    ?nif_stub.

