%% Riak Enterprise Multi Data Center
%% Copyright (c) 2012 Basho Technologies, Inc.  All Rights Reserved.
%%
-module(riak_repl_pb_api).

-include("riak_repl_pb.hrl").

%% Tunneled proxy-get (over the erlang client api)
-define(PB_MSG_PROXY_GET, 128).
-define(PB_MSG_GET_CLUSTER_ID, 129).
-define(PB_MSG_RESP_CLUSTER_ID, 130).

-export([get/4, get/5, get/6,
         get_clusterid/1, get_clusterid/2]).

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
                                              cluster_id = ClusterName}),
    Pkt = riak_repl_pb:encode(Req),
    {ok, {MsgCode, Msg}} = riakc_pb_socket:tunnel(Pid, ?PB_MSG_PROXY_GET, Pkt,
        Timeout),
    {ok, riak_pb_codec:decode(MsgCode, Msg)}.

%% @doc Send a request to get the cluster id (unique cluster name with timestamp)
get_clusterid(Pid) ->
    get_clusterid(Pid, ?DEFAULT_TIMEOUT).

get_clusterid(Pid, Timeout) ->
    Pkt = riak_repl_pb:encode(#rpbreplgetclusteridreq{}),
    {ok, {?PB_MSG_RESP_CLUSTER_ID, Msg}} = riakc_pb_socket:tunnel(Pid, ?PB_MSG_GET_CLUSTER_ID,
        Pkt, Timeout),
    {ok, riak_repl_pb:decode_rpbreplgetclusteridresp(Msg)}.

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
