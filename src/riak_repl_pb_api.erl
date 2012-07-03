
-module(riak_repl_pb_api).

-include("riak_repl_pb.hrl").

-export([get/4, get/5, get/6]).

-define(DEFAULT_TIMEOUT, 60000).

get(Pid, Bucket, Key, ClusterName) ->
    get(Pid, Bucket, Key, ClusterName, [], ?DEFAULT_TIMEOUT).

get(Pid, Bucket, Key, ClusterName, Timeout) when is_integer(Timeout);
        Timeout == infinity ->
    get(Pid, Bucket, Key, ClusterName, [], Timeout);
get(Pid, Bucket, Key, ClusterName, Options) ->
    get(Pid, Bucket, Key, ClusterName, Options, ?DEFAULT_TIMEOUT).

get(Pid, Bucket, Key, ClusterName, Options, Timeout) ->
    Req = get_options(Options, #rpbreplgetreq{bucket = Bucket, key = Key,
            cluster_name = ClusterName}),
    EReq = riak_repl_pb:encode(Req),
    gen_server:call(Pid, {req, <<128, EReq/binary>>, Timeout}, infinity).

%%% internal functions

%% taken from riak_erlang_client
get_options([], Req) ->
    Req;
get_options([{basic_quorum, BQ} | Rest], Req) ->
    get_options(Rest, Req#rpbreplgetreq{basic_quorum = BQ});
get_options([{notfound_ok, NFOk} | Rest], Req) ->
    get_options(Rest, Req#rpbreplgetreq{notfound_ok = NFOk});
get_options([{r, R} | Rest], Req) ->
    get_options(Rest, Req#rpbreplgetreq{r = riak_pb_kv_codec:encode_quorum(R)});
get_options([{pr, PR} | Rest], Req) ->
    get_options(Rest, Req#rpbreplgetreq{pr = riak_pb_kv_codec:encode_quorum(PR)});
get_options([{if_modified, VClock} | Rest], Req) ->
    get_options(Rest, Req#rpbreplgetreq{if_modified = VClock});
get_options([head | Rest], Req) ->
    get_options(Rest, Req#rpbreplgetreq{head = true});
get_options([deletedvclock | Rest], Req) ->
    get_options(Rest, Req#rpbreplgetreq{deletedvclock = true}).
