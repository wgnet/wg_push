-module(wg_push_sender).
-behavior(gen_server).

-include("wg_push.hrl").

-export([start_link/0, send_message/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([create_connection/1, send/1]). %% inner functions exported for testing

-record(connection, {
          port :: port(),
          ssl_options :: #wg_push_ssl_options{},
          items = [] :: [#wg_push_item{}]
         }).

-record(state, {
          num_items_to_send :: integer(),
          wait_for_items_timeout :: integer(),
          connections = [] :: [{file:name_all(), #connection{}}]
         }).


%%% module API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec send_message(#wg_push_item{}, #wg_push_ssl_options{}) -> ok.
send_message(Message, SSL_Options) ->
    gen_server:cast(?MODULE, {send_message, Message, SSL_Options}),
    ok.


%%% gen_server API

init([]) ->
    N = application:get_env(wg_push, num_items_to_send, 20),
    %% TODO check N > 1
    W = application:get_env(wg_push, wait_for_items_timeout, 100),
    {ok, #state{num_items_to_send = N, wait_for_items_timeout = W}}.


handle_call(_Any, _From, State) ->
    {noreply, State}.


handle_cast({send_message, Message, SSL_Options},
            #state{num_items_to_send = NumItems, connections = Connections} = State) ->
    CertFile = SSL_Options#wg_push_ssl_options.certfile,
    Connections2 =
        case proplists:get_value(CertFile, Connections) of
            undefined ->
                NewConnection = create_connection(SSL_Options),
                NewConnection2 = NewConnection#connection{items = [Message]},
                [{CertFile, NewConnection2} | Connections];
            #connection{items = Items} = Connection ->
                Items2 = [Message | Items],
                Connection2 = Connection#connection{items = Items2},
                Connection3 =
                    if
                        length(Items2) >= NumItems -> send(Connection2);
                        true -> Connection2
                    end,
                [{CertFile, Connection3} | proplists:delete(CertFile, Connections)]
        end,
    {noreply, State#state{connections = Connections2}};


handle_cast(_Any, State) ->
    {noreply, State}.


handle_info(_Request, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.



%%% inner functions

-spec create_connection(#wg_push_ssl_options{}) -> #connection{}.
create_connection(SSL_Options) ->
    %% TODO connect
    %% #wg_push_ssl_options{keyfile = Keyfile, certfile = CertFile} = SSL_Options,
    Port = null,
    #connection{port = Port, ssl_options = SSL_Options}.


-spec send(#connection{}) -> #connection{}.
send(#connection{port = Port, ssl_options = SSL_Options, items = Items} = Connection) ->
    %% TODO process errors
    %% need callback to send errors to
    Connection#connection{items = []}.
