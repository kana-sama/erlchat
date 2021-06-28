-module(erlchat_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([{'_', [
        {"/ws", erlchat_member, []},
        {"/", cowboy_static, {priv_file, erlchat, "index.html"}},
        {"/app.js", cowboy_static, {priv_file, erlchat, "app.js"}}
    ]}]),
    {ok, _} = cowboy:start_clear(listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    erlchat_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(listener).
