-module(erlchat_member).
-behaviour(cowboy_websocket).
-export([init/2, new_member_logined/2, member_unlogined/2, message_posted/3]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

%% API

init(Req, _) ->
    #{name := Name} = cowboy_req:match_qs([name], Req),
    Opts = #{idle_timeout => 60000 * 30},
    {cowboy_websocket, Req, Name, Opts}.

new_member_logined(Conn, Name) ->
    Conn ! {event_new_member_logined, Name}.

member_unlogined(Conn, Name) ->
    Conn ! {event_member_unlogined, Name}.

message_posted(Conn, From, Message) ->
    Conn ! {event_message_posted, From, Message}.

%% implementation

websocket_init(Name) ->
    Members = erlchat_room:login(Name, self()),
    Data = list_to_binary(lists:join(<<", ">>, lists:map(fun(M) -> io_lib:format("\"~s\"", [M]) end, Members))),
    {[{text, <<"{ \"members\": [", Data/binary , "] }">>}], Name}.

websocket_handle({text, Message}, Name) ->
    erlchat_room:message(Name, Message),
    {[], Name};
websocket_handle(Req, Name) ->
    io:format("Unknown request: ~w", [Req]),
    {[], Name}.

websocket_info({event_new_member_logined, Member}, Name) ->
    {[{text, <<"{ \"enter\": \"", Member/binary, "\" }">>}], Name};
websocket_info({event_member_unlogined, Member}, Name) ->
    {[{text, <<"{ \"leave\": \"", Member/binary, "\" }">>}], Name};
websocket_info({event_message_posted, From, Message}, Name) ->
    {[{text, <<"{ \"from\": \"", From/binary, "\", \"message\": \"", Message/binary, "\" }">>}], Name};
websocket_info(_, Name) ->
    {[], Name}.

terminate(_, _, Name) ->
    erlchat_room:unlogin(Name),
    ok.
