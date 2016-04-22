%% Copyright (c) 2015, Chris Maguire <cwmaguire@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-module(erlmud_character).

-behaviour(erlmud_object).

%% object behaviour
-export([id/3]).
-export([added/2]).
-export([removed/2]).
-export([attempt/3]).
-export([succeed/2]).
-export([fail/3]).

id(Props, _Owner, Pid) ->
    "character_" ++ proplists:get_value(name, Props, "NoName") ++ "_" ++ Pid.

added(_, _) -> ok.
removed(_, _) -> ok.

has_pid(Props, Pid) ->
    lists:any(fun({_, Pid_}) when Pid == Pid_ -> true; (_) -> false end, Props).

set(Type, Obj, Props) ->
    lists:keystore(Type, 1, Props, {Type, Obj}).

get_(Type, Props) ->
    lists:keyfind(Type, 1, Props).

attempt(_Owner, Props, {move, Self, Direction}) when Self == self() ->
    case proplists:get_value(room, Props) of
        undefined ->
            {{fail, "Character doesn't have room"}, false, Props};
        Room ->
            {{resend, Self, {move, Self, Room, Direction}}, false, Props}
    end;
%% No longer used?
attempt(_Owner, Props, {enter_world, Self}) when Self == self() ->
    case proplists:get_value(room, Props) of
        undefined ->
            {{fail, "Character doesn't have room"}, false, Props};
        Room when is_pid(Room) ->
            {succeed, true, Props}
    end;
attempt(_Owner, Props, {drop, Self, Pid}) when Self == self(), is_pid(Pid) ->
    case has_pid(Props, Pid) of
        true ->
            {room, Room} = get_(room, Props),
            {{resend, Self, {drop, Self, Pid, Room}}, true, Props};
        _ ->
            {succeed, _Interested = false, Props}
    end;
attempt(_Owner, Props, {attack, Attack, Attacker, TargetName}) when is_binary(TargetName) ->
    log(debug, [<<"Checking if name ">>, TargetName, <<" matches">>]),
    SelfName = proplists:get_value(name, Props, <<>>),
    case re:run(SelfName, TargetName, [{capture, none}]) of
        match ->
            {{resend, Attack, {attack, Attack, Attacker, self()}}, true, Props};
        _ ->
            log(debug,
                [<<"Name ">>,
                 TargetName,
                 <<" did not match this character's name: ">>,
                 SelfName,
                 <<".\n">>]),
            {succeed, false, Props}
    end;
attempt(_Owner, Props, {calc_next_attack_wait, Attack, Self, Target, Sent, Wait})
    when Self == self() ->
    CharacterWait = proplists:get_value(attack_wait, Props, 0),
    log(debug, [<<"Character attack wait is ">>, CharacterWait, <<"\n">>]),
    {succeed,
     {calc_next_attack_wait, Attack, Self, Target, Sent, Wait + CharacterWait},
     false,
     Props};
attempt(_Owner, Props, {move, Self, _, _}) when Self == self() ->
    {succeed, true, Props};
attempt(_Owner, Props, {move, Self, _, _, _}) when Self == self() ->
    {succeed, true, Props};
attempt(_Owner, Props, {attack, Self, _}) when Self == self() ->
    {succeed, true, Props};
attempt(_Owner, Props, {stop_attack, Attack}) ->
    {succeed, _IsCurrAttack = lists:member({attack, Attack}, Props), Props};
attempt(_Owner, Props, {die, Self}) when Self == self() ->
    {succeed, true, Props};
attempt(_Owner, Props, {look, Source, TargetName}) when Source =/= self(),
                                                  is_binary(TargetName) ->
    log(debug, [<<"Checking if name ">>, TargetName, <<" matches">>]),
    SelfName = proplists:get_value(name, Props, <<>>),
    SelfSpecies = proplists:get_value(species, Props, <<>>),
    case re:run(SelfName, TargetName, [{capture, none}, caseless]) of
        match ->
            Context = <<SelfSpecies/binary, " ", SelfName/binary, " -> ">>,
            NewMessage = {look, Source, self(), Context},
            {{resend, Source, NewMessage}, _ShouldSubscribe = true, Props};
        _ ->
            ct:pal("Name ~p did not match this character's name ~p~n", [TargetName, SelfName]),
            log(debug,
                [<<"Name ">>,
                 TargetName,
                 <<" did not match this character's name: ">>,
                 SelfName,
                 <<".\n">>]),
            {succeed, false, Props}
    end;
attempt(_Owner, Props, {look, _Source, Self, _Context}) when Self == self() ->
    {succeed, true, Props};
attempt(OwnerRoom, Props, {look, Player, OwnerRoom, _RoomContext}) when Player /= self() ->
    {succeed, true, Props};
attempt(_Owner, Props, {look, _Source, _Target, _Context}) ->
    {succeed, false, Props};
attempt(Room = _Owner, Props, {look, Self}) when Self == self() ->
    NewMessage = {look, Self, Room},
    {{resend, Self, NewMessage}, _ShouldSubscribe = false, Props};
attempt(_Owner, Props, _Msg) ->
    {succeed, false, Props}.

succeed(Props, {move, Self, Source, Target, _Exit}) when Self == self() ->
    log(debug, [<<"moved from ">>, Source, <<" to ">>, Target, <<"\n">>]),
    %erlmud_object:remove(Source, character, self()),
    %erlmud_object:add(Target, character, self()),
    log(debug, [<<"setting ">>, Self, <<"'s room to ">>, Target, <<"\n">>]),
    NewProps = set(owner, Target, Props),
    log(debug, [<<" finished moving rooms \n">>]),
    NewProps;
succeed(Props, {move, Self, Source, Direction}) when Self == self(), is_atom(Direction) ->
    log(debug, [<<"succeeded in moving ">>, Direction, <<" from ">>, Source, <<"\n">>]),
    Props;
succeed(Props, {enter_world, Self})
    when Self == self() ->
    Room = proplists:get_value(room, Props),
    log(debug, [<<"entering ">>, Room, <<"\n">>]),
    erlmud_object:add(Room, character, self()),
    erlmud_object:add(self(), owner, Room),
    Props;
succeed(Props, {get, Self, Source, Item}) when Self == self() ->
    log(debug, [<<"getting ">>, Item, <<" from ">>, Source, <<"\n\tProps: ">>, Props, <<"\n">>]),
    Props;
%% I don't get this: we delete any current attack if a partially started
%% attack (no attack pid, just source and target) succeeds?
succeed(Props, {attack, Self, Target}) when Self == self() ->
    log(debug, [<<"{attack, self(), ">>,
                case Target of
                    TPid when is_pid(TPid) -> TPid;
                    TBin -> TBin
                end,
                <<"} succeeded. Starting attack process">>]),
    %attack(Target, stop_attack(Props));
    attack(Target, lists:keydelete(attack, 1, Props));
succeed(Props, {stop_attack, AttackPid}) ->
    log(debug, [<<"Character ">>, self(), <<" attack ">>, AttackPid, <<" stopped; remove (if applicable) from props:\n\t">>, Props, <<"\n">>]),
    lists:filter(fun({attack, Pid}) when Pid == AttackPid -> false; (_) -> true end, Props);
succeed(Props, {die, Self}) when Self == self() ->
    lists:keydelete(attack, 1, Props);
succeed(Props, {cleanup, Self}) when Self == self() ->
    %% TODO: kill/disconnect all connected processes
    %% TODO: drop all objects
    {stop, cleanup_succeeded, Props};
succeed(Props, {look, Source, SelfTarget, _NoContext}) when SelfTarget == self() ->
    describe(Source, Props, _Context = <<>>),
    Props;
succeed(Props, {look, Source, Target, Context}) ->
    _ = case is_owner(Target, Props) of
            true ->
                describe(Source, Props, Context);
            _ ->
                ok
        end,
    Props;
%% TODO use Child to determine the preposition (In/on/at, etc.)
%% e.g. "in the crater", "on the dock", "at the back of the bus"
succeed(Props, {describe, Source, _Child, Desc}) ->
    Name = proplists:get_value(name, Props, <<"I-don't-have-a-name">>),
    FramedDesc = <<"In/on/at ", Name/binary, " you see ", Desc/binary>>,
    erlmud_object:attempt(Source, {send, FramedDesc});
succeed(Props, Msg) ->
    log(debug, [<<"saw ">>, Msg, <<" succeed\n">>]),
    Props.

fail(Props, target_is_dead, _Message) ->
    log(debug, [<<"Stopping because target is dead\n">>]),
    {stop, Props};
fail(Props, _Message, _Reason) ->
    Props.

attack(Target, Props) ->
    Args = [_Id = undefined,
            _Type = erlmud_attack,
            _Props = [{owner, self()}, {target, Target}]],
    {ok, Attack} = supervisor:start_child(erlmud_object_sup, Args),
    log(debug, [<<"Attack ">>, Attack, <<" started, sending attempt\n">>]),
    erlmud_object:attempt(Attack, {attack, Attack, self(), Target}),
    [{attack, Attack} | Props].

describe(Source, Props, Context) ->
    Description = description(Props),
    erlmud_object:attempt(Source, {send, Source, [<<Context/binary>>, Description]}).

is_owner(MaybeOwner, Props) when is_pid(MaybeOwner) ->
    MaybeOwner == proplists:get_value(owner, Props);
is_owner(_, _) ->
    false.

description(Props) when is_list(Props) ->
    DescTemplate = application:get_env(erlmud, character_desc_template, []),
    log(debug, [<<"description template: ">>, DescTemplate]),
    Description = [[description_part(Props, Part)] || Part <- DescTemplate],
    log(debug, [<<"Description: ">>, Description]),
    Description.

description_part(_, RawText) when is_binary(RawText) ->
    log(debug, [<<"description_part with unknown Props and RawText: ">>, RawText]),
    RawText;
description_part(Props, DescProp) ->
    log(debug, [<<"description_part with Props: ">>, Props, <<", DescProp: ">>, DescProp]),
    prop_description(proplists:get_value(DescProp, Props, <<"??">>)).

prop_description(undefined) ->
    [];
prop_description(Value) when not is_pid(Value) ->
    Value.

log(Level, IoData) ->
    erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).
