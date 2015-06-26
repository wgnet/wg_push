-module(wg_push_sender).
-behavior(gen_server).

-include("wg_push.hrl").

-export([start_link/0, set_apns_host_port/2, send_message/2, send_messages/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_connection/2, send/2, parse_reply/1]). %% inner functions exported for testing


-record(state, {
        apns_host :: string(),
        apns_port :: integer(),
        connections = orddict:new() :: orddict:orddict(file:name_all(), port())
         }).


%%% module API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec set_apns_host_port(string(), integer()) -> ok.
set_apns_host_port(Host, Port) ->
    gen_server:call(?MODULE, {set_apns_host_port, Host, Port}).


-spec send_message(#wg_push_item{}, #wg_push_ssl_options{}) -> ok | {error, term()}.
send_message(Message, SSL_Options) ->
    send_messages([Message], SSL_Options).


-spec send_messages([#wg_push_item{}], #wg_push_ssl_options{}) -> ok | {error, term()}.
send_messages(Messages, SSL_Options) ->
    gen_server:call(?MODULE, {send_messages, Messages, SSL_Options}).


%%% gen_server API

init([]) ->
    {ok, #state{
            apns_host = application:get_env(wg_push, apns_host, "gateway.sandbox.push.apple.com"),
            apns_port = application:get_env(wg_push, apns_port, 2196)
           }}.


handle_call({set_apns_host_port, Host, Port}, _From, State) ->
    State2 = State#state{apns_host = Host, apns_port = Port},
    {reply, ok, State2};


handle_call({send_messages, Messages, SSL_Options}, _From, State) ->
    {Reply, State3} = send_messages(Messages, SSL_Options, State),
    {reply, Reply, State3};

handle_call(_Any, _From, State) ->
    {noreply, State}.


handle_cast(_Any, State) ->
    {noreply, State}.


handle_info(_Request, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.



%%% inner functions
-spec send_messages([#wg_push_item{}], #wg_push_ssl_options{}, #state{}) -> ok | {error, term()}.
send_messages([], _SSL_Options, State) ->
    {ok, State};
send_messages(Messages, #wg_push_ssl_options{certfile = CertFile} = SSL_Options,
              #state{connections = Connections} = State) ->
    case get_connection(SSL_Options, State) of
        {ok, Socket, State2} ->
            case send(Socket, Messages) of
                ok -> {ok, State2};
                {error, closed} ->
                    {{error, closed}, State2#state{connections = orddict:erase(CertFile, Connections)}};
                {error, ItemID, shutdown} ->
                    ssl:close(Socket),
                    NewState = State#state{connections = orddict:erase(CertFile, Connections)},
                    RestMessages = remove_sent_messages(Messages, ItemID),
                    send_messages(RestMessages, SSL_Options, NewState);
                {error, _ItemID, Reason} ->
                    ssl:close(Socket),
                    {{error, Reason}, State2#state{connections = orddict:erase(CertFile, Connections)}}
            end;
        {error, Reason} -> {{error, no_connection, Reason}, State}
    end.

-spec get_connection(#wg_push_ssl_options{}, #state{}) -> {ok, port(), #state{}} | {error, term()}.
get_connection(#wg_push_ssl_options{certfile = CertFile, keyfile = KeyFile},
               #state{apns_host = Host, apns_port = Port, connections = Connections} = State) ->
    case orddict:find(CertFile, Connections) of
        error ->
            case open_connection(CertFile, KeyFile, Host, Port) of
                {ok, Socket} ->
                    State2 = State#state{connections = orddict:store(CertFile, Socket, Connections)},
                    {ok, Socket, State2};
                {error, Reason} -> {error, Reason}
            end;
        {ok, Socket} ->
            case ssl:recv(Socket, 0, 0) of
                {ok, _} -> {ok, Socket, State};
                {error, _Reason} ->
                    %% bad socket, reopen
                    ssl:close(Socket),
                    case open_connection(CertFile, KeyFile, Host, Port) of
                        {ok, NewSocket} ->
                            State2 = State#state{connections = orddict:store(CertFile, NewSocket, Connections)},
                            {ok, NewSocket, State2};
                        {error, Reason} -> {error, Reason}
                    end
            end

    end.

-spec open_connection(file:name_all(), file:name_all() | undefined, list(), integer()) -> {ok, port(), #state{}} | {error, term()}.
open_connection(CertFile, KeyFile, Host, Port) ->
    Key = case KeyFile of %% when certfile is a .pem containing cert & key
        undefined -> CertFile;
        _         -> KeyFile
    end,
	Options = [
            {certfile, CertFile},
            {keyfile, Key},
            {versions,['tlsv1.1']},
            {active, false},
            binary
            ],
    case ssl:connect(Host, Port, Options) of
        {ok, Socket} -> {ok, Socket};
        {error, Reason} -> {error, Reason}
    end.


-spec send(port(), [#wg_push_item{}]) -> ok | {error, term()}.
send(Socket, Messages) ->
    case wg_push_pack:pack_items(Messages) of
        {ok, Bin} ->
            case ssl:send(Socket, Bin) of
                ok -> case ssl:recv(Socket, 6, 200) of %% TODO what timeout is better to use here?
                          {ok, Bin2} -> parse_reply(Bin2);
                          {error, timeout} -> ok; %% Message is sent successfully
                          {error, Reason} -> {error, Reason}
                      end;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> {error, Reason}
    end.

remove_sent_messages(Messages, ItemID) ->
    case lists:dropwhile(fun(#wg_push_item{id = Id}) -> Id /= ItemID end, Messages) of
        [] -> [];
        [_LastSent | T] -> T
    end.

parse_reply(<<8, 0, _ItemID/binary>>) -> ok;
parse_reply(<<8, 1, ItemID/binary>>) -> {error, ItemID, processing_error};
parse_reply(<<8, 2, ItemID/binary>>) -> {error, ItemID, missing_device_token};
parse_reply(<<8, 3, ItemID/binary>>) -> {error, ItemID, missing_topic};
parse_reply(<<8, 4, ItemID/binary>>) -> {error, ItemID, missing_payload};
parse_reply(<<8, 5, ItemID/binary>>) -> {error, ItemID, invalid_token_size};
parse_reply(<<8, 6, ItemID/binary>>) -> {error, ItemID, invalid_topic_size};
parse_reply(<<8, 7, ItemID/binary>>) -> {error, ItemID, invalid_payload_size};
parse_reply(<<8, 8, ItemID/binary>>) -> {error, ItemID, invalid_token};
parse_reply(<<8, 10, ItemID/binary>>) -> {error, ItemID, shutdown}; %% TODO try to send later
parse_reply(<<8, 255, ItemID/binary>>) -> {error, ItemID, uknown_error};
parse_reply(_Any) -> {error, unknown_reply}.
