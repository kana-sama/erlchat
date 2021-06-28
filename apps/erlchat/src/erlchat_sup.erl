-module(erlchat_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {#{strategy => one_for_all}, [
        #{id => erlchat_server, start => {erlchat_server, start_link, []}},
        #{id => erlchat_room, start => {erlchat_room, start_link, []}}
    ]}}.
