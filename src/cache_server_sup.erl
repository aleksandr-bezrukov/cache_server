-module(cache_server_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() -> supervisor:start_link({local,?MODULE},?MODULE,[]).

init([]) -> 


    SupervisorSpecification = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60},

    ChildSpecifications =
        [#{id => cache_server,
           start => { cache_server, start_link, []},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [cache_server]}
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