-module(erlchat_member).
-behaviour(cowboy_websocket).
-export([new_member_logined/2, member_unlogined/2, message_posted/3]).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

%% API

new_member_logined(Member, Name) -> send_json(Member, #{enter => Name}).
member_unlogined(Member, Name) -> send_json(Member, #{leave => Name}).
message_posted(Member, From, Message) -> send_json(Member, #{from => From, message => Message}).
send_json(Member, Value) -> global:send({erlchat_member, Member}, {send_json, Value}).

%% implementation

init(Req, _) ->
    #{name := Name} = cowboy_req:match_qs([name], Req),
    Opts = #{idle_timeout => 60000 * 30},
    {cowboy_websocket, Req, Name, Opts}.

websocket_init(Name) ->
    global:register_name({erlchat_member, Name}, self()),
    Members = erlchat_room:login(Name),
    {[{text, jsone:encode(#{members => Members})}], Name}.

websocket_handle({text, Message}, Name) -> erlchat_room:message(Name, Message), {[], Name}.
websocket_info({send_json, Value}, Name) -> {[{text, jsone:encode(Value)}], Name}.
terminate(_, _, Name) -> erlchat_room:unlogin(Name).
