-module(erlchat_room).
-behaviour(gen_server).

-export([start_link/0, login/2, unlogin/1, message/2]).
-export([init/1, handle_cast/2, handle_call/3]).

%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, nope, []).

login(Name, Conn) ->
    gen_server:call(?MODULE, {login, Name, Conn}).

unlogin(Name) ->
    gen_server:cast(?MODULE, {unlogin, Name}).

message(Name, Message) ->
    gen_server:cast(?MODULE, {message, Name, Message}).


%% implementation

init(nope) ->
    {ok, maps:new()}.

handle_call({login, Name, Conn}, _, Members) ->
    maps:foreach(fun(_, MemberConn) ->
        erlchat_member:new_member_logined(MemberConn, Name)
    end, Members),
    NewMembers = maps:put(Name, Conn, Members),
    {reply, maps:keys(NewMembers), NewMembers}.

handle_cast({unlogin, Name}, Members) ->
    NewMembers = maps:remove(Name, Members),
    maps:foreach(fun(_, MemberConn) ->
        erlchat_member:member_unlogined(MemberConn, Name)
    end, NewMembers),
    {noreply, NewMembers};

handle_cast({message, From, Message}, Members) ->
    maps:foreach(fun(_, MemberConn) ->
        erlchat_member:message_posted(MemberConn, From, Message)
    end, Members),
    {noreply, Members}.
