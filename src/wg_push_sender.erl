-module(wg_push_sender).
-behavior(gen_server).

-include("wg_push.hrl").

-export([start_link/0, set_apns_host_port/2, send_message/2, send_messages/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_connection/2, send/2, parse_reply/1]). %% inner functions exported for testing


-record(state, {
          apns_host :: string(),
          apns_port :: integer(),
          connections = [] :: {file:name_all(), port()}
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
    {Reply, State3} =
        case get_connection(SSL_Options, State) of
            {ok, Socket, State2} -> {send(Socket, Messages), State2};
            {error, Reason} -> {{error, no_connection, Reason}, State}
        end,
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

-spec get_connection(#wg_push_ssl_options{}, #state{}) -> {ok, port(), #state{}} | {error, term()}.
get_connection(#wg_push_ssl_options{certfile = CertFile, keyfile = KeyFile},
               #state{apns_host = Host, apns_port = Port, connections = Connections} = State) ->
    case proplists:get_value(CertFile, Connections) of
        undefined ->
            Options = [{certfile, CertFile},
                       {keyfile, KeyFile},
                       %%{versions,['tlsv1.1']},
                       {active, false},
                       binary
                      ],
            case ssl:connect(Host, Port, Options) of
                {ok, Socket} ->
                    State2 = State#state{connections = [{CertFile, Socket} | Connections]},
                    {ok, Socket, State2};
                {error, Reason} -> {error, Reason}
            end;
        Socket ->
            %% TODO check Socket is not closed,
            %% reopen and update State if needed
            {Socket, State}
    end.


-spec send(port(), [#wg_push_item{}]) -> ok | {error, term()}.
send(Socket, Messages) ->
    case wg_push_pack:pack_items(Messages) of
        {ok, Bin} ->
            case ssl:send(Socket, Bin) of
                ok -> case ssl:recv(Socket, 6, 200) of %% TODO what timeout is better to use here?
                          {ok, Bin2} -> parse_reply(Bin2); %% TODO close socket and open new one
                          {error, Reason} -> {error, Reason}
                      end;
                {error, timeout} -> ok; %% Message is sent successfully
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> {error, Reason}
    end.


parse_reply(<<8, 0, _ItemID/binary>>) -> ok;
parse_reply(<<8, 1, _ItemID/binary>>) -> {error, processing_error};
parse_reply(<<8, 2, _ItemID/binary>>) -> {error, missing_device_token};
parse_reply(<<8, 3, _ItemID/binary>>) -> {error, missing_topic};
parse_reply(<<8, 4, _ItemID/binary>>) -> {error, missing_payload};
parse_reply(<<8, 5, _ItemID/binary>>) -> {error, invalid_token_size};
parse_reply(<<8, 6, _ItemID/binary>>) -> {error, invalid_topic_size};
parse_reply(<<8, 7, _ItemID/binary>>) -> {error, invalid_payload_size};
parse_reply(<<8, 8, _ItemID/binary>>) -> {error, invalid_token};
parse_reply(<<8, 10, _ItemID/binary>>) -> {error, shutdown}; %% TODO try to send later
parse_reply(<<8, 255, _ItemID /binary>>) -> {error, uknown_error};
parse_reply(_Any) -> {error, unknown_reply}.
