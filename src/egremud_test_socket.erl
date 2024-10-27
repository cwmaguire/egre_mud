%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(egremud_test_socket).
-behaviour(gen_server).

-export([start/1]).
-export([stop/1]).
-export([send/2]).
-export([messages/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {conn,
                messages = [] :: [string()]}).

start(Name) ->
    gen_server:start({local, Name}, ?MODULE, [], []).

stop(Name) ->
    gen_server:cast(Name, stop).

send(Name, Msg) ->
    gen_server:cast(Name, Msg).

messages(Name) ->
    gen_server:call(Name, messages).

init(_) ->
  {ok, Conn} = supervisor:start_child(egremud_conn_sup, [self()]),
  {ok, #state{conn = Conn}}.

handle_call(messages, _From, State = #state{messages = Messages}) ->
    {reply, Messages, State#state{messages = []}};
handle_call(_Req = Text, _From, State = #state{conn = Conn}) ->
    egremud_conn:handle(Conn, Text),
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Req = Text, State = #state{conn = Conn}) ->
    egremud_conn:handle(Conn, Text),
    {noreply, State}.

handle_info({send, Msg}, State = #state{messages = Messages, conn = Conn}) ->
    ct:pal("Socket {~p,~p} rx: ~p~n",
           [self(), Conn, flatten(Msg)]),
{noreply, State#state{messages = [flatten(Msg) | Messages]}};


handle_info(_Req, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_, _, _) ->
    {error, not_implemented}.

flatten(Output) when is_list(Output) ->
    ListOfBins = lists:flatten(Output),
    lists:foldl(fun(Bin, Acc) -> <<Acc/binary, Bin/binary>> end, <<>>, ListOfBins);
flatten(Output) ->
    Output.


