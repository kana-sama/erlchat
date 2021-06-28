-module(erlchat_server).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, nope, []).

init(nope) ->
    Dispatch = cowboy_router:compile([{'_', [
        {"/ws", erlchat_member, []},
        {"/", cowboy_static, {priv_file, erlchat, "index.html"}},
        {"/app.js", cowboy_static, {priv_file, erlchat, "app.js"}}
    ]}]),
    {ok, _} = cowboy:start_clear(listener, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
    {ok, nope}.

handle_call(_, _, nope) -> {noreply, nope}.
handle_cast(_, nope) -> {noreply, nope}.