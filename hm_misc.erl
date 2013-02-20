%%-*- coding:utf-8 -*-
-module(hm_misc).

-export([monitor/2,demonitor/2]).
-export([to_tuplelist/2,to_record/3,to_match_record/3]).
-export([ensure_started/1,set_env/3,get_env/2,get_env/3]).
-export([to_hex/1,to_digit/1,boolean_to_number/1]).
-export([to_iolist/1]).

%% Tow functions to deal with pid monitor and demonitor in ets table
monitor(Pid,PidGroup) ->
    case ets:match_object(PidGroup, {Pid,'_'}) of
        [] ->
            M = erlang:monitor(process, Pid),
            ets:insert(PidGroup, {Pid, M});
        _ ->
            %% Already monitoring peer
            ok
    end.

demonitor(Pid,PidGroup) ->
    case ets:match_object(PidGroup,{Pid,'_'}) of
    	 [{Pid,Ref}] ->
	    erlang:demonitor(Ref),
	    ok;
         [] ->
	    ok
   end.

%% Some functions deal with record 
to_tuplelist(Fields,Record)->
    Values = tl(tuple_to_list(Record)),
    lists:zip(Fields,Values).

to_record(Name,Fields,TupleList)->
    Values = lists:map(
	       fun(F)->
		       case lists:keyfind(F,1,TupleList) of
			   false ->
			       undefined;
			   {_,V} ->
			       V
			end
	       end,Fields),
    Record = [Name] ++ Values ,
    erlang:list_to_tuple(Record).

to_match_record(Name,Fields,TupleList)->
    Values = lists:map(
	       fun(F)->
		       case lists:keyfind(F,1,TupleList) of
			   false ->
			       '_';
			   {_,V} ->
			       V
			end
	       end,Fields),
    Record = [Name] ++ Values ,
    erlang:list_to_tuple(Record).

%% Application ensure start
ensure_started(App) ->
        case application:start(App) of
            ok ->
                ok;
            {error, {already_started, App}} ->
                ok
        end.

get_env(AppName,Par) ->
    case application:get_env(AppName,Par) of
        undefined ->
            undefined;
        {ok,Val} ->
            Val
    end.

get_env(AppName,Par,DefVal) ->
    case get_env(AppName,Par) of
        undefined ->
            DefVal;
        Val ->
            Val
    end.

set_env(AppName,Par,Val)->
    application:set_env(AppName,Par,Val).

%% String tools
to_hex([]) ->
    [];
to_hex(Bin) when is_binary(Bin) -> 
    to_hex(binary_to_list(Bin));
to_hex([H|T]) ->
    [to_digit(H div 16), to_digit(H rem 16) | to_hex(T)].

to_digit(N) when N < 10 ->
    $0 + N;
to_digit(N) ->
    $a + N-10.

boolean_to_number(true)->
    1;
boolean_to_number(false) ->
    0;
boolean_to_number(undefined) ->
    0.

to_iolist(Item) when is_binary(Item)->
  Item;
to_iolist(Item) when is_atom(Item)->
  erlang:atom_to_binary(Item,utf8);
to_iolist(Item) when is_list(Item)->
  unicode:characters_to_binary(Item,utf8);
to_iolist(Item) when is_integer(Item)->
  erlang:integer_to_list(Item);
to_iolist(Item)->
  erlang:term_to_binary(Item).


