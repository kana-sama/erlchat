-module(erlchat_room).
-behaviour(gen_server).

-export([start_link/0, login/1, unlogin/1, message/2]).
-export([init/1, handle_cast/2, handle_call/3]).

%% API

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, nope, []).
login(Name) -> gen_server:call(?MODULE, {login, Name}).
unlogin(Name) -> gen_server:cast(?MODULE, {unlogin, Name}).
message(From, Message) -> gen_server:cast(?MODULE, {message, From, Message}).


%% implementation

init(nope) ->
    {ok, []}.

handle_call({login, Name}, _, Members) ->
    lists:foreach(fun(Member) -> erlchat_member:new_member_logined(Member, Name) end, Members),
    {reply, [Name|Members], [Name|Members]}.

handle_cast({unlogin, Name}, Members) ->
    NewMembers = lists:delete(Name, Members),
    lists:foreach(fun(Member) -> erlchat_member:member_unlogined(Member, Name) end, NewMembers),
    {noreply, NewMembers};

handle_cast({message, From, Message}, Members) ->
    lists:foreach(fun(Member) -> erlchat_member:message_posted(Member, From, Message) end, Members),
    {noreply, Members}.
