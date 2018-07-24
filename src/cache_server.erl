-module(cache_server).

-export([start/1, stop/0, lookup/1, insert/3, lookup_by_date/2, cleaning/0]).

-behaviour(gen_server).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,terminate/2, code_change/3]).

%-record(state, {
%        apns_host :: string(),
%        apns_port :: integer(),
%        connections = orddict:new() :: orddict:orddict(file:name_all(), port())
%         }).

-define(CACHENAME, ?MODULE).

    start([{drop_interval, Seconds}]) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [{drop_interval, Seconds}], []).

    stop() -> gen_server:call(?MODULE, {stop}).

    insert(Key, Value, Seconds) -> gen_server:call(?MODULE,{insert,Key,Value,Seconds}).

    lookup(Key) -> gen_server:call(?MODULE,{find,Key}).

    lookup_by_date(DateFrom, DateTo) -> gen_server:call(?MODULE,{find_period,DateFrom, DateTo}).

    cleaning() ->
	%Now = 
	os:system_time(second).
	%,delete_expired(ets:select(CACHENAME,[{{'$1','$2','$3'},[{'<','$3',Now}],[['$1'])).

%    delete_expired(ExpiredList) ->
%	case ExpiredList of
%	    [] -> ok;
%	    [[H]|T] -> ets:delete(?CACHENAME,H)
%	    ,delete_expired(T)
%	end.

    init([{drop_interval, DrTime}]) ->
        ok = timer:start()
	,{ok, _TRef} = timer:apply_interval(DrTime*1000, cache_server, cleaning, [])
	,{ok, ets:new(?CACHENAME,[public,named_table])}.

    handle_call({insert,Key,Value,Seconds},_From,State) ->
	Now = calendar:datetime_to_gregorian_seconds({date(), time()})
	,Reply = ets:insert(?CACHENAME,{Key,Value,Seconds+Now})
	,{reply, Reply, State};

    handle_call({find,Key}, _From, State) ->
	Now = calendar:datetime_to_gregorian_seconds({date(), time()})
	,Reply = case ets:lookup(?CACHENAME,Key) of
		    [] -> []
		    ;[{_,_,TimeOfRecord}] when Now > TimeOfRecord -> []
		    ;[{_,Value,_}] -> Value
		 end
	,{reply, Reply, State};

    handle_call({find_period,DateFrom, DateTo}, _From, State) ->
	FromSec = calendar:datetime_to_gregorian_seconds(DateFrom)
	,ToSec = calendar:datetime_to_gregorian_seconds(DateTo)
	,Reply = get_in_period(ets:first(?CACHENAME), FromSec, ToSec, [])
	,{reply, Reply, State};

    handle_call({stop}, _From, State) ->
	{stop, normal, stopped, State}.


    get_in_period('$end_of_table',_,_,Acc) ->
        {ok, Acc};
    get_in_period(Next, FromSec, ToSec, Acc) ->
	[{_, Status, Exp}] = ets:lookup(?CACHENAME, Next),
	if
	    Exp >= FromSec,	Exp =< ToSec ->
		get_in_period(ets:next(?CACHENAME,Next),FromSec,ToSec,[{binary_to_list(Next), binary_to_list(Status)}|Acc]);
	    true -> 
		get_in_period(ets:next(?CACHENAME,Next),FromSec,ToSec,Acc)
	end.

	
    handle_cast(_Msg, State) -> {noreply, State}.
    handle_info(_Info, State) -> {noreply, State}.
    terminate(_Reason, _State) -> ok.
    code_change(_OldVsn, State, _Extra) -> {ok, State}.
