%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(egremud_conn).
-behaviour(gen_statem).

-export([start_link/1]).
-export([handle/2]).

-export([login/3]).
-export([password/3]).
-export([live/3]).
-export([dead/3]).

-export([init/1]).
-export([callback_mode/0]).

-record(data, {socket :: pid(),
               conn_obj :: pid(),
               player :: pid(),
               login :: string(),
               attempts = 0 :: integer(),
               parse_fun :: mfa()}).

%% api

start_link(Socket) ->
    gen_statem:start_link(?MODULE, Socket, []).

handle(Pid, Msg) ->
    gen_statem:cast(Pid, Msg).

%% states

login(cast, Event, Data) ->
    {next_state, password, Data#data{login = Event}}.

password(cast, _Event = Password, Data = #data{login = Login,
                                               attempts = Attempts,
                                               socket = Socket}) ->
    case is_valid_creds(Login, Password) of
        {true, logger} ->
            egremud_event_log:register(self());
        {true, _Player} ->
            % All players are live processes at MUD startup; processes are almost free
            PlayerPid = egre_index:get_pid(player),
            ConnProps = [{owner, PlayerPid},
                         {conn, {self()}},
                         {rules, [rules_conn_enter_world,
                                  rules_conn_send]}],

            Socket ! {send, <<"Login successful.">>},
            {ok, ConnObjPid} = supervisor:start_child(egre_object_sup, [_Id = undefined, ConnProps]),
            % TODO Player is supposed to enter in a room
            Message = {PlayerPid, enter_world, in, room, with, ConnObjPid},

            egre_object:attempt(ConnObjPid, Message),
            %ConnObjPid ! {ConnObjPid, Message},

            {next_state, live, Data#data{login = undefined,
                                         player = PlayerPid,
                                         conn_obj = ConnObjPid}};
        false ->
            get_failed_auth_state(Data#data{login = undefined, attempts = Attempts + 1})
    end.

get_failed_auth_state(Data = #data{attempts = Attempts}) when Attempts < 3 ->
    {next_state, login, Data};
get_failed_auth_state(Data) ->
    {next_state, dead, Data}.

dead(cast, _, _Data = #data{socket = Socket}) ->
    Socket ! {send, "Connection Refused"},
    keep_state_and_data;

dead({call, From}, props, _Data) ->
    {keep_state_and_data, [{reply, From, _Props = []}]}.

live(cast, {send, Message}, _Data = #data{socket = Socket}) ->
    Socket ! {send, Message},
    keep_state_and_data;
live(cast, Message,
     Data = #data{player = PlayerPid,
                  conn_obj = ConnObjPid,
                  parse_fun = ParseFun}) ->
    log([{event, Message}, {state, live}, {player, Data}, {conn, ConnObjPid}]),

    % MUD should parse this, not the engine
   _ = case ParseFun(PlayerPid, Message) of
       {error, Error} ->
           Data#data.socket ! {send, Error};
       Event ->
           egre_object:attempt(ConnObjPid, Event)
    end,

    {next_state, live, Data};
live({call, {From, Ref}}, props, _Data) ->
    From ! {Ref, _Props = []},
    keep_state_and_data;
live(Type, Event, Data) ->
    console_log_unknown(live, Type, Event, Data),
    keep_state_and_data.

%% gen_statem

init(Socket) ->
    ParseFun =
        case application:get_env(egremud, parse_fun) of
            {ok, {M, F, A}} ->
                fun M:F/A;
            _ ->
                egremud_event_log:log(debug, [{error, <<"no parse fun specified">>}]),
                Socket ! {send, <<"Error: no parse function. Contact admin. SYSTEM ABEND '0D37'">>},
                throw("No player input parse function")
        end,

    Socket ! {send, <<"Welcome to egremud!">>},
    Socket ! {send, <<"Currently any login and password will do.">>},
    {ok, login, #data{socket = Socket, parse_fun = ParseFun}}.

callback_mode() ->
    state_functions.

%% private

is_valid_creds("log", "log") ->
    {true, logger};
is_valid_creds(_String, never_fails) ->
    false;
is_valid_creds(_Login, _Password) ->
    {true, egre_index:get_pid(player)}.

log(Terms) ->
    egremud_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).

console_log_unknown(State, EventType, EventData, _Data = #data{player = Player}) ->
    io:format("Connection ~p for player ~p received unrecognized event ~p:~p in state ~p",
              [self(), Player, EventType, EventData, State]).
