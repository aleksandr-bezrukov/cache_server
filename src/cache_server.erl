-module(cache_server).

-export([start/1, stop/0, get_data/1, put_data/2, get_by_date/2, cleaning/0]).

-behaviour(gen_server).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,terminate/2, code_change/3]).

-record(state, {
        apns_host :: string(),
        apns_port :: integer(),
        connections = orddict:new() :: orddict:orddict(file:name_all(), port())
         }).

    start([{drop_interval, Seconds}]) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [{drop_interval, Seconds}], []).

    stop() -> gen_server:call(?MODULE, stop).

    insert(Key, Value, Seconds) -> gen_server:call(?MODULE,{insert,Key,Value,Seconds}).

    lookup(Key) -> gen_server:call(?MODULE,{find,Key}).

    lookup_by_date(DateFrom, DateTo) -> gen_server:call(?MODULE,{find_period,DateFrom, DateTo}).

get_emo(Who) -> gen_server:call(?MODULE, {get, Who}).
put_emo({Who, Status}, Time) -> gen_server:call(?MODULE, {put, Who, Status, Time}).
get_by_date(FromDate, ToDate) -> gen_server:call(?MODULE, {get_by_date, FromDate, ToDate}).

get_by_date('$end_of_table',_,_,Acc) ->
    {ok, Acc};
get_by_date(Name, FromSec, ToSec, Acc) ->
    [{_, Status, Exp}] = ets:lookup(?MODULE, Name),
    if
	Exp >= FromSec,	Exp =< ToSec ->
	    get_by_date(ets:next(?MODULE,Name),FromSec,ToSec,[{binary_to_list(Name), binary_to_list(Status)}|Acc]);
	true -> 
	    get_by_date(ets:next(?MODULE,Name),FromSec,ToSec,Acc)
end.


    handle_cast(_Msg, State) -> {noreply, State}.
    handle_info(_Info, State) -> {noreply, State}.
    terminate(_Reason, _State) -> ok.
    code_change(_OldVsn, State, _Extra) -> {ok, State}.


-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() -> supervisor:start_link({local,?MODULE},?MODULE,[]).

init([]) -> 


    SupervisorSpecification = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60},

    ChildSpecifications =
        [#{id => cache_server_genserver,
           start => { cache_server_genserver, start_link, []},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [some_worker]}
%,         #{id => other_worker,
%           start => {other_worker, start_link, []},
%           restart => permanent,
%           shutdown => 2000,
%           type => worker,
%           modules => [other_worker]}
        ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.



%GenServer = [{cache_server_gensrv,{}}]
%    ,{ok,{{one_for_one,1,5},GenServer}}.

%init()
%s t a r t _ l i n k ( ) ->
%s u p e r v i s o r : s t a r t _ l i n k ( { l o c a l , 7M0DULE}, 7M0DULE, [ ] ) .
%init(FileName) ->
%UsrChild = { u s r , { u s r , s t a r t l i n k , [ ] } ,
%permanent, 2000, worker, [usr, u s r d b ] } ,
%{ o k , { { o n e _ f o r _ a l l , l , l } , [UsrChild]}}.