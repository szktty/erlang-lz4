-module(lz4).

-export([compress/1, compress/2, uncompress/2,
    pack/1, pack/2, unpack/1,
    test/0, test_mp/0]).

-on_load(init/0).

-type option() :: high | {block, integer(), integer()}.
-type pack() :: binary().

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
uncompress(_Binary, _OrigSize) ->
    ?nif_stub.

-spec pack(binary()) -> {ok, pack()} | {error, term()}.
pack(Binary) ->
    pack(Binary, []).

-spec pack(binary(), [option()]) -> {ok, pack()} | {error, term()}.
pack(Binary, Options) ->
    case compress(Binary, Options) of
        {ok, Compressed} ->
            OrigSize = byte_size(Binary),
            {ok, <<OrigSize:4/little-unsigned-integer-unit:8,
                Compressed/binary>>};
        Error ->
            Error
    end.

-spec unpack(pack()) -> {ok, binary()} | {error, term()}.
unpack(<<OrigSize:4/little-unsigned-integer-unit:8, Binary/binary>>) ->
    uncompress(Binary, OrigSize).

test() ->
    Raw = <<"Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.">>,
    Raw1 = binary:copy(Raw, 40000),
    lz4:pack(Raw1).

test_mp() ->
    Raw = <<"Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.">>,
    Raw1 = binary:copy(Raw, 40000),
    lz4:pack(Raw1, [{block, 1024, 16}]).

